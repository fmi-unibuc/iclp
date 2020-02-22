objects := $(patsubst %.md,%.pdf,$(wildcard *.md))

all: $(objects)

%.pdf: %.md header.tex
	pandoc $< -o $@ -s -t beamer -V theme:metropolis -H header.tex

clean:
	-rm $(objects)

.PHONY: all clean
