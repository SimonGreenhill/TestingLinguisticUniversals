# Enduring constraints on grammar revealed by Bayesian spatiophylogenetic analyses.

To generate plots and summary statistics run:

> 00_run_all_plots.R



## Analysis of Features

All the results of feature level analyses can be found in ./results. Each of
the subdirectories contains the analyses for one proposed universal. 

Inside each of these directories will be a set of files, and directories. The
files are the common datafiles across all analyses, while each directory is one
analysis. 

Note that not all analyses were done on all universals, if the analysis directory
is missing for a particular feature, we did not analyse it using that method.

The files are:

* BT_data.txt - the data for the linguistic universal, three column delimited text file.
* pruned_tree.tree - the single summary tree pruned to only contain the tips with data for this universal.
* pruned_trees.trees.gz - a compressed [GZIP](https://en.wikipedia.org/wiki/Gzip) archive of the full posterior distribution.
* script_*.R - a `R` script to generate the datafile (BT_data.txt) for this feature.



### Bayestraits:

* bayestraits/              - bayestraits results
** bayestraits/dep           - bayestraits dependent model
** bayestraits/ind           - bayestraits independent model

Each of these directories contains 4 files:

* in.txt - the commands given to `BayesTraits`
* BT_data.txt.Log.txt.gz  - log file from the `BayesTraits` analysis. Compressed with GZip.
* res_(dep|indep).txt.Schedule.txt.gz - log file from the `BayesTraits` analysis.  Compressed with GZip.
* res_(dep|indep).txt.Stones.txt -  log file from the `BayesTraits` analysis

### BRMS Analysis with no controls:

* brms.single/


### BRMS Analysis with family level control:

* brms.family/

### BRMS with spatiophylogenetic model

* brms_spphylo.single/      - single analysis on summary tree
* brms.posterior/           - BRMS analysis on full posterior distribution




# Running BRMS Analyses

To save space in the github repository duplicate files have been removed. 
To rerun the analyses, you will need to copy the directory e.g. `2012KA/brms.single`
and place the following files inside it as well:

* varcov.spatial_function.R
* Glottolog_Languages.csv
* BT_data.txt

and then run either `brms.R`, `brms_spatphylo.R` or `brms_spatfam.R`

