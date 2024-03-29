% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RDA_outputs_synthesis.R
\name{RDA_outputs_synthesis}
\alias{RDA_outputs_synthesis}
\title{RDA_outputs_synthesis function}
\usage{
RDA_outputs_synthesis(RDA, RDA.synth, RDA.anova, nbperms, RDA.Table)
}
\arguments{
\item{RDA}{The RDA object : RDA <- vegan::rda(mtcars_quant~gear+carb,data=mtcars)}

\item{RDA.synth}{TRUE or FALSE. If TRUE, displays the RDA model's global variance partitioning. Default is TRUE.}

\item{RDA.anova}{TRUE or FALSE. If TRUE, prints the results of the vegan function anova.cca, displaying the variance partitioning by terms in the RDA model. Default is TRUE.}

\item{nbperms}{Numeric number of permutations used by the RDA.anova function. Default is 1000.}

\item{RDA.Table}{TRUE or FALSE. TRUE : calculate and display the variance percentage of considered factor / total unconstrained variance. Default : TRUE.}
}
\value{
Results can be displayed in the console. Outputs are saved in data frame.
}
\description{
This function facilitate output analysis of vegan package RDA.
}
\examples{

library(vegan)
my.RDA <- vegan::rda(mtcars[,1:7]~vs+am+gear, data=mtcars)
RDA_outputs_synthesis(RDA = my.RDA, RDA.synth = TRUE, RDA.anova = FALSE, RDA.Table = TRUE)




}
