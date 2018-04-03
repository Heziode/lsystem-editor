PROJECT=LSystem_Editor
KOCH_FLAKE_LEVEL=3

all: build

run:
	./bin/lsystem-editor -i ./data/koch-flake.ls -e PS -p ./data/koch-flake-${KOCH_FLAKE_LEVEL}.ps -d ${KOCH_FLAKE_LEVEL}

build:
	gprbuild -d -p -P${PROJECT}
	cp ./src/view/view.glade ./bin/view.glade

doc:
	gnatdoc -P${PROJECT} --output=html --encoding=utf-8 -l

clean:
	rm -rf {obj,bin,doc,gnatdoc,*.db*,gpsauto.cgpr,LSystem_Editor-loc.xml}
	mkdir {obj,bin,doc}
