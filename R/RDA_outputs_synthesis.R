
#' RDA_outputs_synthesis function
#'
#' This function facilitate output analysis of vegan package RDA.
#'
#' @param RDA The RDA object : RDA <- vegan::rda(mtcars_quant~gear+carb,data=mtcars)
#' @param RDA.synth TRUE or FALSE. If TRUE, displays the RDA model's global variance partitioning. Default is TRUE.
#' @param RDA.anova TRUE or FALSE. If TRUE, prints the results of the vegan function anova.cca, displaying the variance partitioning by terms in the RDA model. Default is TRUE.
#' @param nbperms Numeric number of permutations used by the RDA.anova function. Default is 1000.
#' @param RDA.Table TRUE or FALSE. TRUE : calculate and display the variance percentage of considered factor / total unconstrained variance. Default : TRUE.
#'
#' @return Results can be displayed in the console. Outputs are saved in data frame.
#' @export
#'
#' @examples
#'
#' library(vegan)
#' my.RDA <- vegan::rda(mtcars[,1:7]~vs+am+gear, data=mtcars)
#' RDA_outputs_synthesis(RDA = my.RDA, RDA.synth = TRUE, RDA.anova = TRUE, RDA.Table = TRUE)
#'
#'
#'
#'

RDA_outputs_synthesis <- function(RDA, RDA.synth, RDA.anova, nbperms, RDA.Table){
  #Define default parameters
  if(missing(nbperms)){

    nbperms <- 1000

  }

  if(missing(RDA.synth)){

    RDA.synth <- TRUE

  }

  if(missing(RDA.anova)){

    RDA.anova <- TRUE

  }

  if(missing(RDA.Table)){

    RDA.Table <- TRUE

  }

  if(RDA.synth==TRUE){

    summary_output <- utils::capture.output(summary(RDA))
    variance_partitioning_lines <- suppressWarnings(summary_output[grep("Proportion", summary_output):grep("Unconstrained", summary_output)])

    # Nettoyer et transformer les données en dataframe
    variance_data <- do.call(rbind, strsplit(variance_partitioning_lines, "\\s+"))
    variance_df <- as.data.frame(variance_data)
    colnames(variance_df) <- c("Component", "Inertia", "Proportion")
    variance_df <- variance_df[-c(1),]

    # Convertir les colonnes Inertia et Proportion en numérique
    variance_df$Inertia <- as.numeric(as.character(variance_df$Inertia))
    variance_df$Proportion <- as.numeric(as.character(variance_df$Proportion))
    variance_df$Proportion <- variance_df$Proportion*100

    message("Calling RDA.synth function:")
    print(variance_df)

  }

  if(RDA.anova==TRUE){

    variance_terms <- vegan::anova.cca(RDA, permutations = nbperms, by="term")

    message("Calling RDA.anova function:")
    print(variance_terms)

  }

  if(RDA.Table==TRUE){

    message("Calculation of variance % associated with each RDA factor, considering unconstrained total variance")
    Table_RDA <- as.data.frame(variance_terms)
    Table_RDA$Unconstrained.var.percent <- round((100*Table_RDA$Variance)/(sum(Table_RDA$Variance)),2)
    Table_RDA$Sign.p.val <- NA

    for (i in 1:(nrow(Table_RDA)-1)){

      if (Table_RDA[i,4] <= 0.001){Table_RDA[i,6] <- "***"}
      else if(Table_RDA[i,4] < 0.01 & Table_RDA[i,4] > 0.001){Table_RDA[i,6] <- "**"}
      else if(Table_RDA[i,4]<0.05 & Table_RDA[i,4] >= 0.01){Table_RDA[i,6] <- "*"}
      else if(Table_RDA[i,4]>0.05){Table_RDA[i,6] <- "ns"}
      else{NULL}


    }

    Table_RDA[,c(2,3,5)] <- sapply(Table_RDA[,c(2,3,5)], FUN = function(x) round(x,2))
    Table_RDA[,4] <- round(Table_RDA[,4], 5)

    names(Table_RDA)[3] <- "F.val"

    anovaattr <- attr(variance_terms, "heading")
    message(anovaattr)
    print(Table_RDA)

    Table_RDA <<- Table_RDA

  }
}
