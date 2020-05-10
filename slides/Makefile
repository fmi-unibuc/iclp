objects := $(patsubst %.md,%.pdf,$(wildcard *.md)) c11-python.ipynb

all: $(objects)

%.pdf: %.md header.tex Makefile
	pandoc $< -o $@ -s -t beamer --toc -V theme:metropolis -H header.tex

%.ipynb: %.md
	pandoc $< -o $@

clean:
	-rm $(objects)

.PHONY: all clean
