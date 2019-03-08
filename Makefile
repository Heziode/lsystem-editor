PROJECT=LSystem_Editor
KOCH_FLAKE_LEVEL=3

all: build

run:
	./bin/lsystem-editor --no-gui -i ./data/koch-flake.ls -e PS -p ./koch-flake-${KOCH_FLAKE_LEVEL}.ps -d ${KOCH_FLAKE_LEVEL}

run-gui:
	./bin/lsystem-editor

build:
	mkdir -p bin/ressources
	gprbuild -d -p -P${PROJECT}
	cp ./src/view/ressources/* ./bin/ressources/

doc:
	gnatdoc -P${PROJECT} --output=html --encoding=utf-8 -l

clean:
	rm -rf {obj,bin,doc,gnatdoc,*.db*,gpsauto.cgpr,LSystem_Editor-loc.xml,*.adt,*.ali}
	mkdir {obj,bin,doc}
	mkdir bin/ressources
