default:
    just --list --unsorted

generate-xml:
    rm -rf book/generated-xml
    ./book/tools/generate.sh
    ./book/tools/validate.hs

generate-static:
    git rm -r public
    rm -rf yesodweb.com
    git clone https://github.com/yesodweb/yesodweb.com
    cd yesodweb.com/make-it-static && cargo run
    mv yesodweb.com/public .
    git add public
    rm -rf yesodweb.com

generate:
    just generate-xml
    just generate-static
