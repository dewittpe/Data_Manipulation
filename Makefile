EXAMPLES := $(addsuffix .Rout, $(basename $(shell find . -type f -name "*.R")))

all: $(EXAMPLES)

%.Rout : %.R 000_data_sets/PSPS/psps_2019.csv 000_data_sets/PSPS/psps_2020.csv
	R  CMD BATCH --vanilla $< $@

000_data_sets/PSPS/psps_2020.csv :
	cd 000_data_sets/PSPS/; gzip -dc 2020/* | cat - > psps_2020.csv

000_data_sets/PSPS/psps_2019.csv :
	cd 000_data_sets/PSPS/; gzip -dc 2019/* | cat - > psps_2019.csv
