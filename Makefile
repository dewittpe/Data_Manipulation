EXAMPLES := $(addsuffix .Rout, $(basename $(shell find . -type f -name "*.R")))
EXAMPLESHTML := $(addsuffix .Rout.html, $(basename $(shell find . -type f -name "*.R")))

all: $(EXAMPLES) slides.html $(EXAMPLESHTML)

%.Rout : %.R 000_data_sets/PSPS/psps_2019.csv 000_data_sets/PSPS/psps_2020.csv
	R  CMD BATCH --vanilla $< $@

%.Rout.html : %.Rout
	nvim -c TOhtml -c wqa $<

000_data_sets/PSPS/psps_2020.csv :
	cd 000_data_sets/PSPS/; gzip -dc 2020/* | cat - > psps_2020.csv

000_data_sets/PSPS/psps_2019.csv :
	cd 000_data_sets/PSPS/; gzip -dc 2019/* | cat - > psps_2019.csv

slides.html : slides.Rmd style.css
	R --quiet --vanilla -e "rmarkdown::render('$<')"

slides.Rmd : slides.R
	R --quiet --vanilla -e "knitr::spin('$<', knit = FALSE)"
