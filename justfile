generate:
    rm -rf book/generated-xml
    ./book/tools/generate.sh
    ./book/tools/validate.hs
