PROJECT=LSystem_Editor

all: build

build:
	gprbuild -d -p -P${PROJECT}

doc:
	gnatdoc -P${PROJECT} --output=html --encoding=utf-8 -l

clean:
	rm -rf {obj,bin,doc,gnatdoc,*.db*,gpsauto.cgpr,LSystem_Editor-loc.xml}
	mkdir {obj,bin,doc}
