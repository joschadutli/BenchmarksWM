# Making RDAs

The `make-rda` scripts in this directory turn the original data in Benchmarks.WM into a
standard format. The name of the script corresponds to the section of the
benchmarks e.g. `make-rda5.R` contains the scripts to generate the data files
included as part of Section 5, see `B5MultipleDemands.Rd`. 

The `post-build.R` script fixes various formatting and compression issues that
have to be addressed in order to publish a maximally useful and compatible
package. See script for details.
