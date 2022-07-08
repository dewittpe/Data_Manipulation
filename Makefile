EXAMPLES := $(addsuffix .Rout, $(basename $(shell find . -type f -name "*.R")))

all: $(EXAMPLES)

%.Rout : %.R
	R  CMD BATCH --vanilla $< $@
