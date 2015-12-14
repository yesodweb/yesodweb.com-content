About a month ago, there were a few days of instability for yesodweb.com
hosting. The reason for this was that I was in the midst of moving hosting of
yesodweb.com (and a few other sites) to new hosting. I went through a few
iterations, and wanted to mention how the hosting now works, what I like, and
some pain points to notice.

The end result of this is a
[Docker](https://www.docker.com)/[Kubernetes](http://kubernetes.io/)
deployment, consisting of a single Docker image containing various sites (six
currently), and an extra application that reverse proxies to the appropriate
one based on virtual host.  But let's step through it bit by bit.

## Stack's Docker support

The [Stack build tool](http://haskellstack.com) supports using Docker in two
different ways, both of which are leveraged by this setup.

1. Using a Docker build image to provide build tools (like GHC), system
   libraries, and optionally Haskell libraries, and performing the build within
   such a container. This isolates your build from most host-specific
   configurations, and grants you immediate access to many tools (like PostgreSQL
   client libraries) without modifying your host.
2. Generate a Docker image based on a base image that includes necessary system
   libraries, and includes generated executables and any additional files
   requested (such as configuration files and static resources like CSS and
   Javascript).

What's really nice about this setup vs a more standard Docker image generation
approach is that our generated runtime image (from (2)) does not include any
build-specific tools. This makes our images lighter-weight, and avoids having
unnecessary code in production (which is good from a security standpoint).

What's really nice about all of this is how simple the Stack configuration is
to make it happen. Consider the following `stack.yaml` file:

```yaml
# Every Stack.yaml needs to specify its resolver
resolver: lts-3.14

# Build using Docker. Will use the default stack-build image, which includes
build tools but not precompiled libraries.
docker:
  enable: true

# Generate a Docker image
image:
  container:
    # New image will be called snoyberg/yesodweb
    name: snoyberg/yesodweb
    # Base it on the stack-run images
    base: fpco/stack-run
    # Include some config and static files in the image
    add:
      config: /app/config
      static: /app/static
```

With this in place, running `stack image container` will generate the
`snoyberg/yesodweb` image, which I can then push to whatever Docker registry I
want using normal Docker commands.

For more information on Stack's Docker support, see [the Docker integration
page](http://docs.haskellstack.org/en/stable/docker_integration.html).

## Mega-repo

I initially deployed each of my sites as a separate deployment. However, for
various resource-related reasons (disk space, number of machines), I decided to
try out deploying the six sites as a single deployment. I'm not convinced yet
that this is a great idea, but it's certainly working in practice. The result
is my [snoyman-webapps repo](https://github.com/snoyberg/snoyman-webapps). As a
short snippet:

```yaml
apps:
- vhost: www.yesodweb.com
  dir: /app/yesodweb.com
  exe: yesodweb
  args:
  - production
- vhost: www.haskellers.com
  dir: /app/haskellers
  exe: haskellers
  args:
  - production

redirects:
- src: yesodweb.com
  dst: www.yesodweb.com
- src: yesodweb.org
  dst: www.yesodweb.com
```

This file has a few important things to note:

* A submodule for each site in the deployment, inside [the sites
  directory](https://github.com/snoyberg/snoyman-webapps/tree/master/sites)
* The Kubernetes configuration (discussed below) in [the kube
  directory](https://github.com/snoyberg/snoyman-webapps/tree/master/kube)
* A [reverse proxying web
  application](https://github.com/snoyberg/snoyman-webapps/tree/master/webapps)
  for running the sites and serving appropriate content based on virtual host.

Let's jump into that last one right away.

### Reverse proxying

Probably the most instructive file on this program is the
[webapps.yaml](https://github.com/snoyberg/snoyman-webapps/blob/master/webapps/config/webapps.yaml)
config file. This shows that the web app is capable of:

* Running child applications (the six sites I mentioned)
* Reverse proxying to the appropriate applications
* Performing simple redirects between domain names

In theory this code could be turned into something standalone, but for now it's
really custom-tailored to my needs here.

The biggest downside with this approach is that (without [Server Name
Indication, or SNI](https://en.wikipedia.org/wiki/Server_Name_Indication)) it
doesn't support TLS connections. I chose sites for this that are not served
over TLS currently, and do not handle sensitive information (e.g., no password
collection). Upgrading to have SNI support and using something like [Let's
Encrypt](https://letsencrypt.org/) would be a fun upgrade in the future.

## Kubernetes configuration

Kubernetes uses YAML files for configuration. I'm not going to jump into the
syntax of the config files or the overarching model Kubernetes uses for running
your applications. If you're unfamiliar and interested, I recommended reading
the Kubernetes docs.

### Secrets

Three of the sites I'm hosting have databases, and therefore the database
credentials need to be securely provided to the apps during deployment.
Kubernetes provides a nice mechanism for this: secrets. You specify some
(base64-encoded) content in a YAML file, and then you can mount a virtual
filesystem for your apps to access the data from. Let's have a look at the
haskellers.com (scrubbed) secrets file:

```yaml
apiVersion: v1
kind: Secret
metadata:
    name: haskellers-postgresql-secret
data:
    # https://github.com/snoyberg/haskellers/blob/master/config/db/postgresql.yml.example
    postgresql.yml: base64-yaml-contents
    # https://github.com/snoyberg/haskellers/blob/master/config/db/aws.example
    aws: base64-yaml-contents
    client-session-key.aes: base64-version-of-client-session-key
```

I have a separate secrets config for each site. The decision was made mostly
for historical reasons, since the sites were originally hosted separately. I
still like to keep them separate though, since it's easy to put the secrets
into different subdirectories, as we'll see next.

### Replication controller

There's a lot of content in the [replication controller
config](https://github.com/snoyberg/snoyman-webapps/blob/master/kube/rc.yaml).
I'll strip it down just a bit:

```yaml
apiVersion: v1
kind: ReplicationController
metadata:
    name: snoyman-webapps
    labels:
        name: snoyman-webapps
spec:
  replicas: 1
  selector:
    name: snoyman-webapps
  template:
    metadata:
        labels:
            name: snoyman-webapps
    spec:
        volumes:
        - name: haskellers-postgresql-volume
          secret:
              secretName: haskellers-postgresql-secret
        containers:
        - name: webapps
          image: snoyberg/snoyman-webapps:latest
          command: ["webapps"]
          workingDir: /app/webapps
          ports:
          - name: webapps
            containerPort: 3000
          env:
          - name: PORT
            value: "3000"
          volumeMounts:
          - name: haskellers-postgresql-volume
            readOnly: true
            mountPath: /app/haskellers/config/db
```

The interesting stuff:

* We mount the haskellers.com secret at /app/haskellers/config/db, where the
  app itself expects it
* The webapps app needs to know what port to listen on, so we tell it via the
  PORT environment variable
* We also tell Kubernetes that the application is listening on port 3000
* The internally listened on ports for each application are irrelevant to us:
  the webapps app handles though for us automatically

### Service (load balancer)

The load balancing service is quite short and idiomatic:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: snoyman-webapps
  labels:
    name: snoyman-webapps
spec:
  type: LoadBalancer
  ports:
  - name: http
    port: 80
    targetPort: webapps
  selector:
    name: snoyman-webapps
```

## Updates

When it comes time to make updates to one the these sites, I do the following:

* Change that site's repo and commit
* Update the submodule reference for snoyman-webapps
* Run `stack image container && docker push snoyberg/snoyman-webapps`
    * See [the stack.yaml file for details](https://github.com/snoyberg/snoyman-webapps/blob/master/stack.yaml)
* Perform a rolling update with Kubernetes: `kubectl rolling-update snoyman-webapps --image=snoyberg/snoyman-webapps:latest`

Some notes:

* The `rolling-update` is lack-luster in Kubernetes; it can fail to work for a
  variety of reason I have yet to fully understand. My biggest advice: when
  possible, create single-container replication controllers.
* I always just push and use the latest image. For more control/reliability, I recommend tagging Docker images with the Git SHA1 of the commit being built from. I'm lazy for these sites, but for client deployments at FP Complete, we always follow this practice.

## Google Container Engine

I started off this whole project as a way to evaluate Kubernetes. Based on
that, I started hosting this on Google Container Engine instead of fiddling
with configuring AWS to host Kubernetes myself. Overall, I'm happy with how it
turned out. I had a few ugly issues

* Running out of disk space due to the large number of Docker images (likely the primary motivation to moving towards a single Docker image for all the sites).
* All my sites went down one day when my account switched over from the trial to non-trial. I don't remember getting an email warning me about this, which would have been nice.

A n1-standard-1 instance size has been plenty to support all of these sites,
which is nice (yay lightweight Haskell/Yesod!). That said, at FP Complete, we
host our stuff on AWS, and have had pretty good experience with running
Kubernetes there.

## Conclusion

Overall, I find the Docker/Kubernetes deployment workflow quite pleasant to
work with. I may find more hiccups over time, but for now, I'd strongly
recommend people consider it for deployments of their own, especially if you're
using tooling like Stack that makes it so easy to create Docker images.
