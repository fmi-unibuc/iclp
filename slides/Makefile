objects := $(patsubst %.md,%.pdf,$(wildcard *.md))

all: $(objects)

%.pdf: %.md
	pandoc $< -o $@ -s -t beamer -V theme:CambridgeUS -V colortheme:crane

clean:
	-rm $(objects)

.PHONY: all clean
