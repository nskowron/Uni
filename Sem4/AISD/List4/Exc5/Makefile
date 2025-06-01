JAVAC=javac
JAVA=java
SRC=$(wildcard *.java)
CLASSES=$(SRC:.java=.class)
MAIN=Main

all: $(CLASSES)

%.class: %.java
	$(JAVAC) $<

run: all
	$(JAVA) $(MAIN)

clean:
	rm -f *.class