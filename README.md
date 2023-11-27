
# GABB

<!-- badges: start -->
<!-- badges: end -->

The GABB package is designed to assist casual R users in conducting and synthesizing RDA (Redundancy Analysis) and PCA (Principal Component Analysis) analyses. This encompasses everything from preparing datasets to generating visualizations.

GABB does not attempt to 'reinvent the wheel.' It requires main inputs to be RDA and PCA objects, which should be created using the 'vegan' and 'FactoMineR' packages, respectively.

For RDA, GABB aims to verify data conformity with RDA requirements and to simplify the process of displaying and saving RDA outputs.

For PCA, the package focuses on streamlining and enhancing the graphical representation of PCA, particularly in the projection of individuals and variables.

Graphical outputs are based on the 'ggplot2', 'ggplotify', 'ggforce', and 'ggpubr' packages. Plots created with GABB are saved as 'grob' or 'ggplot' objects, allowing users to further customize them using standard 'ggplot2' functions.

## Installation

You can install the development version of GABB like so:

``` r
# install.package("GABB")
# library(GABB)

```

## Example

This is a basic example which shows you how use GABB with the mtcars data.set:

``` r
library(GABB)
library(vegan)
library(FactoMiner)


#Example of GABB package pipeline with the base data.set "mtcars" 
my.data <- mtcars

#Data preparation for RDA and PCA : tranformation and scaling of numeric/quantitative variables

prep_data(data = my.data, quantitative_columns = c(1:7), transform_data_method = "log", scale_data = T)

check_data_for_RDA(data_quant = data_quant,initial_data = my.data,factor_names = c("vs", "am", "gear", "carb"))
# => the data set factor carb not suited for RDA analysis
# Simple solution : not considering it

check_data_for_RDA(data_quant = data_quant,initial_data = my.data,factor_names = c("vs", "am", "gear"))
#Check passed. Go for RDA.

#Performing simple RDA analysis
library(vegan)
my.RDA <- vegan::rda(data_quant~vs+am+gear, data=my.data)

#Display and save RDA outputs
#' RDA_outputs_synthesis(RDA = my.RDA, RDA.synth = TRUE, RDA.anova = TRUE, RDA.Table = TRUE)
# => the factor gear is not significant for the model residual variance modulations.
# For the following graphic displays, it will be considered as low importance factor.

#Performing simple PCA analysis
library(FactoMineR)
my.PCA <- FactoMineR::PCA(X = data_quant, scale.unit = F, ncp = 5, graph = F) 

#Create, display and save graphics of PCA individual and variable projections.
PCA_RDA_graphics(complete.data.set = my.data, factor.names = c("vs", "am", "gear"), 
                 PCA.object = my.PCA, Dim.a = 1, Dim.b = 2,
                 Barycenter = T, Segments = T, Barycenter.min.size = 2, Ind.min.size = 1,
                 Segment.line.type = 2, Segment.line.size = 0.1,
                 Ellipse.IC.95 = T, Barycenter.Ellipse.Fac1 = "vs", Barycenter.Ellipse.Fac2 = "am",
                 factor.colors = "vs", factor.shapes = "am",
                 Var.circle = T, Var.circle.size = 2, Var.label.size = 5,
                 Biplot.PCA = F, width.PCA.ind.graph = 0.6, width.PCA.var.graph = 0.4, 
                 Heat.map.graph = T, width.heat.map.graph = 0.3, var.parameter.heat.map = "cor",
                 Dims.heat.map = c(1,2),Display.cell.values.heat.map = T, Cluster.col.heat.map = T, Cluster.row.heat.map = T,
                 RDA.table.graph = T,RDA.table.graph.height = 0.3 )
  
``` 















```

