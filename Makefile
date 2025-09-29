all: 00_run_all_plots.log

00_run_all_plots.log:
	Rscript -e 00_run_all_plots.R | tee $@


## clean: removes auto-generated files
.PHONY: clean
clean:
	rm *.log

