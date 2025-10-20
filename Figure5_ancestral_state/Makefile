# Domain: hierarchy
# # A tibble: 1 × 3
#   code   Universal.shorter   BT_EDGE_BF
#   <chr>  <fct>                    <dbl>
# 1 1053KA agr Dets > agr Mods       190.
#
# Domain: broad word order
# # A tibble: 1 × 3
#   code   Universal.shorter BT_EDGE_BF
#   <chr>  <fct>                  <dbl>
# 1 1152KA Rel-N > N-Rel           78.4
#
# Domain: narrow word order
# # A tibble: 1 × 3
#   code   Universal.shorter               BT_EDGE_BF
#   <chr>  <fct>                                <dbl>
# 1 0344KA VS (tr. verb) > VS (intr. verb)       180.
#
# Domain: other
# # A tibble: 1 × 3
#   code    Universal.shorter      BT_EDGE_BF
#   <chr>   <fct>                       <dbl>
# 1 1431mKA less case > more tense       27.9
RDS=1053KA.rds 1152KA.rds 0344KA.rds 1431mKA.rds
PDF=$(subst .rds,.pdf,$(RDS))
PLOT_RDS=$(subst .rds,.plot,$(RDS))

all: glottolog-cldf $(RDS) $(PLOT_RDS) panel.pdf

# shallow clone glottolog -> needed to label clades
glottolog-cldf:
	git clone --depth=1 https://github.com/glottolog/glottolog-cldf && cd $@ && git checkout

%.rds:
	./asr.R ../results/$(basename $@)/pruned_tree.tree ../results/$(basename $@)/BT_data.txt $@

%.plot: %.rds glottolog-cldf
	./plot_asr.R glottolog-cldf/cldf/cldf-metadata.json $< $(subst .plot,.pdf,$@) $@

panel.pdf: $(PLOT_RDS)
	./plot_panel.R $^


## clean: removes auto-generated files
.PHONY: clean
clean:
	rm *.rds *.pdf

## help: displays help
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<

# debugging helper to show target. Run make print-VARNAME
print-%  : ; @echo $* = $($*)