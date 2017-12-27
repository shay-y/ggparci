
ggparci
=======

Parallel Coordinates Plot with Groups medians and their Confidence Intervals

The main function `ggparci` plots parallel coordinates (gg)plot, in which each line connects the variables medians for each group.  
Confidence "bands"" for the medians are added to each line. This allows the assessment of groups (clusters) separations.  
The variables are normalized to [0,1] scale prior to plotting.  

The goal is to allow the comparison of several groups across several variables, with variability assessment via the confidence intervals.  

## Installation

To install the latest ("cutting-edge") GitHub version run:
```r
# You'll need devtools
install.packages.2 <- function (pkg) if (!require(pkg)) install.packages(pkg);
install.packages.2('devtools')
# make sure you have Rtools installed first! if not, then run:
#install.packages('installr'); install.Rtools()

devtools::install_github("ropensci/plotly") # you will probably benefit from the latest version of plotly
devtools::install_github('talgalili/ggparci')
```
## Example

Some basic examples:

``` r
library(ggparci)

ggparci(iris, groups_column = "Species")
ggparci(normalize(iris), groups_column = "Species") # already normalized in this version

#select only some of the variables
# plot a line for each observation
# and dont plot the CIs
ggparci(mtcars,columns = 3:7, groups_column = "carb", obs_lines = T, alpha_bands = 0)

# display each group in a different facet
ggparci(data = iris, groups_column = "Species", groups_in_facets = T)

# flip the plot
p <- ggparci(data = iris, groups_column = "Species", flip_coords = T)
```
