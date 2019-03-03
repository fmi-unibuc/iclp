all: c1-java1.pdf c2-java2.pdf

%.pdf: %.md
	pandoc $< -o $@ -s -t beamer -V theme:CambridgeUS -V colortheme:crane

clean:
	rm c1-java1.pdf c2-java2.pdf

.PHONY: all clean
