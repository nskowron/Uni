all: build run

build:
	javac --module-path ./javafx-sdk-22.0.1/lib --add-modules javafx.controls AppMain.java

run:
	java --module-path ./javafx-sdk-22.0.1/lib --add-modules javafx.controls AppMain

clean:
	rm -rf *.class
	rm log.txt