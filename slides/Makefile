objects := $(patsubst %.md,%.pdf,$(wildcard *.md))

all: $(objects)

%.pdf: %.md header.tex Makefile
	pandoc $< -o $@ -s -t beamer --toc -V theme:metropolis -H header.tex

clean:
	-rm $(objects)

.PHONY: all clean
