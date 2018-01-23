all: build doc

build:
    gprbuild -d -p -P lse.gpr

doc:
    gnatdoc -P lse.gpr --output=html --encoding=utf-8

clean:
    rm -rf {obj,bin,doc}
    mkdir {obj,bin,doc}
