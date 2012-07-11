# The Haskell Platfrom on a Virtual Machines

I sent a patch to [update a vagrant Haskell VM to the latests version of the platform](https://bitbucket.org/puffnfresh/vagrant-haskell-heroku
). The author published a blog article titled [Haskell on Heroku](http://brianmckenna.org/blog/haskell_on_heroku) when originally releasing, but a Haskell VM has little to do with Heroku.


# Why Vms

* installation isolation
* easy to snapshot & rollback
* shareable
* run Linux software and use Linux package managers on Mac/Windows hardware
* properly match the environment your software will run in

The last point is very important for server software.
Your VM environment should match your deployment environment.
Installation on your server should be automated, and that same automation should be able to install your dependencies on your local VM.
This means only learning how to install to one platform, but also that you have less worries about bugs being introduced because of the difference between the environments. It also means you can statically link on a VM and push that binary to production.

The first point about installation isolation is very important for Haskell. The more levels of isolation you have, the less you will deal with install/dependency issues. Using A VM for some of your Haskell work (perhaps a paticular application) lets you avoid using cabal-dev or hsenv since you mostly want one canonical set of cabal packages.


# VM difficulties

VMs do add some hassle. A good portion of this hassle has been mitigated by the VM provisioning tool called Vagrant that the above VM uses.

In the past, VMs were less mature and computers were less powerful.
Today, even the free and open source Virtualbox is very capable.
I have a very capable CPU, a lot of RAM, and an SSD hard drive, and I cannot tell the difference between using the VM or my native system.


# My workflow on a Mac

I plan on [blogging](http://tunein.yap.tv) about this more in the future. Occasionally I fire up an XCode project on my Mac. But everything that is deployed to a server I run in a VM. My Mac is my work computer, and I also run a Linux VM with a GUI that serves as my personal computer.
