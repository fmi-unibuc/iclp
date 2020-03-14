objects := $(patsubst %.cc,%.out,$(wildcard *.cc))

all: $(objects)

%.out: %.cc
	g++ -fgnu-tm $< -o $@

04-withdrawx2.out: 04-account.hh
04-depositxn.out: 04-account.hh
04-transfer.out: 04-account.hh

08-transfer.out: 08-account.hh

clean:
	rm -rf *.out

.PHONY: all clean