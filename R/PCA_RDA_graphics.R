#' PCA_RDA_graphics function
#' This function facilitate the user creation of PCA (! from FactoMineR package !) enhanced graphics with multiple options.
#' Individuals and variables graphics are possible. With or without variable cor, cos, contrib.. correlation to dimensions.
#' RDA outputs are displayable as table under graphics.
#'
#'
#' @param complete.data.set Original complete data set used for the PCA, with quantitative and qualitative/factor columns
#' @param factor.names Character vector of considered factor variables of PCA
#' @param sample.column Numeric or name (character) of the individual sample column. Default is data frame row names.
#' @param PCA.object The PCA object, built from package FactoMineR. PCA.object <- FactoMineR::PCA(mtcars_quant, scale.unit = T, ncp = 5, graph = F)
#' @param Dim.a Numeric value (1 ; 2 ...) of the first PCA dimension selected for graphic outputs.
#' @param Dim.b Numeric value (1 ; 2 ...) of the first PCA dimension selected for graphic outputs.
#' @param Barycenter TRUE or FALSE. TRUE : Calculate and Display the barycenter of individuals for Barycenter.Ellipse.Fac1 and/or .2 and/or .3.
#' @param Segments TRUE or FALSE. TRUE : Display the linking segments between individuals and barycenters.
#' @param Barycenter.min.size Numeric. Minimum size of barycenter point projections. Ignore if Barycenter = FALSE
#' @param Ind.min.size Numeric. Minimum size of individuals point projections.
#' @param Segment.line.type Numeric. Type of segment lines (see ggplot2 line type). Ignore if Segments = FALSE
#' @param Segment.line.size Numeric. Minimum size of segment lines. Ignore if Segments = FALSE
#' @param Ellipse.IC.95 TRUE or FALSE. TRUE : Calculate and Display the Ellipse (95% mean confidence intervals) of individuals for Barycenter.Ellipse.Fac1 and/or .2 and/or .3.
#' @param Barycenter.Ellipse.Fac1 Character. Name of 1st factor/data frame column for Barycenter / Ellipses calculation.
#' @param Barycenter.Ellipse.Fac2 Character. Name of 2nd factor/data frame column for Barycenter / Ellipses calculation.
#' @param Barycenter.Ellipse.Fac3 Character. Name of 3rd factor/data frame column for Barycenter / Ellipses calculation.
#' @param factor.colors Character. Name of the factor/column considered for individuals colors.
#' @param color.palette Vector of characters of desired colors.
#' @param factor.shapes Character. Name of the factor/column considered for individuals shapes.
#' @param factor.sizes Character. Name of the factor/column considered for individuals colors.
#' @param factor.col.border.ellipse Character. Name of the factor/column considered for ellipse border colors.
#' @param ellipse.line.type Numeric. R line type for ellipse borders.
#' @param Var.circle TRUE or FALSE. TRUE = Display the PCA variable circle projection.
#' @param Var.circle.size Numeric. Value for increasing the size of Var.circle graphic.
#' @param Var.label.size Numeric. Value for increasing the size of Var.circle graphic labels.
#' @param Var.selected Character vector of selected variables for the PCA plot. Default = all variables.
#' @param Overlaying.graphs TRUE or FALSE. TRUE = Overlaying of PCA individuals and variables graphics. Default is set to FALSE.
#' @param width.PCA.ind.graph Numeric. Width ratio for PCA individuals graphic.
#' @param width.PCA.var.graph Numeric. Width ratio for PCA variables graphic.
#' @param width.heat.map.graph Numeric. Width ratio for Heat map variables graphic.
#' @param Heat.map.graph TRUE or FALSE. TRUE = Display the heat map of variable X parameter correlation to dimension.
#' @param var.parameter.heat.map Character. Parameter selected for the heat map correlation of Variable parameter to dimensions. values : "cor", "cos2", "coor","contrib". Default = "cor".
#' @param Dims.heat.map Numeric. Numeric vector c(1,2) of dimensions considered for the variable parameter correlation.
#' @param Display.cell.values.heat.map TRUE or FALSE. TRUE = Display the rounded value of correlations within heat map cells.
#' @param width.cell.heat.map Numeric. Width  for Heat map cells. Default set to default pheatmap.
#' @param height.cell.heat.map Numeric. Width  for Heat map cells. Default set to default pheatmap.
#' @param Cluster.col.heat.map TRUE or FALSE. TRUE = cluster heatmap columns / dimensions.
#' @param Cluster.row.heat.map TRUE or FALSE. TRUE = cluster heatmap rows / quantitative variables.
#' @param RDA.object The RDA object, built from package vegan. RDA.object <- vegan::rda(mtcars_quant, scale.unit = T, ncp = 5, graph = F)
#' @param RDA.table.graph TRUE or FALSE. TRUE = Display the RDA outputs table under PCA graphics.
#' @param Size.RDA.table.graph Numeric. Set the ratio of RDA table graphic size. Defaut is set to 7.
#' @param RDA.table.graph.height Numeric. Set the ratio of RDA table graphic height. Defaut is set to 1.
#'
#' @return Several graphics
#' @export
#'
#' @examples
#'
#' library(FactoMineR)
#' my.PCA <- FactoMineR::PCA(mtcars[,1:7], scale.unit = FALSE, ncp = 5, graph = FALSE)
#' PCA_RDA_graphics(complete.data.set = mtcars, factor.names = c("vs", "gear"), PCA.object = my.PCA)
#'

PCA_RDA_graphics <- function(complete.data.set, factor.names, sample.column,
                             PCA.object, Dim.a, Dim.b, Barycenter, Segments,
                             Barycenter.min.size, Ind.min.size, Segment.line.type, Segment.line.size,
                             Ellipse.IC.95, Barycenter.Ellipse.Fac1, Barycenter.Ellipse.Fac2, Barycenter.Ellipse.Fac3,
                             factor.colors, color.palette, factor.shapes, factor.sizes, factor.col.border.ellipse, ellipse.line.type,
                             Var.circle, Var.circle.size, Var.selected, Var.label.size, Overlaying.graphs,
                             width.PCA.ind.graph, width.PCA.var.graph, width.heat.map.graph,
                             Heat.map.graph, var.parameter.heat.map, Dims.heat.map, Display.cell.values.heat.map,
                             width.cell.heat.map, height.cell.heat.map,Cluster.col.heat.map, Cluster.row.heat.map,
                             RDA.object, RDA.table.graph, RDA.table.graph.height,Size.RDA.table.graph){





  #Define default parameters

  if(missing(Size.RDA.table.graph)){

    Size.RDA.table.graph <- 7

  }

  if(missing(width.cell.heat.map)){

    width.cell.heat.map <- FALSE

  }

  if(missing(height.cell.heat.map)){

    height.cell.heat.map <- FALSE

  }

  if(missing(RDA.table.graph)){

    RDA.table.graph <- FALSE

  }

  if(missing(Cluster.row.heat.map)){

    Cluster.row.heat.map <- FALSE

  }

  if(missing(Cluster.col.heat.map)){

    Cluster.col.heat.map <- FALSE

  }

  if(missing(Display.cell.values.heat.map)){

    Display.cell.values.heat.map <- TRUE

  }

  if(missing(var.parameter.heat.map)){

    var.parameter.heat.map <- "cor"

  }

  if(missing(Ind.min.size)){

    Ind.min.size <- FALSE

  }

  if(missing(factor.sizes)){

    factor.sizes <- FALSE

  }
    if(missing(factor.shapes)){

    factor.shapes <- FALSE

  }

    if(missing(factor.colors)){

    factor.colors <- FALSE

  }

  if(missing(color.palette)){

    color.palette <- FALSE

  }

  if(missing(Var.selected)){

    Var.selected <- FALSE

  }

  if(missing(factor.col.border.ellipse)){

    factor.col.border.ellipse <- FALSE

  }

  if(missing(ellipse.line.type)){

    ellipse.line.type <- FALSE

  }

  if(missing(Barycenter)){

    Barycenter <- FALSE

  }

  if(missing(Barycenter.min.size)){

    Barycenter.min.size <- FALSE

  }

  if(missing(Segments)){

    Segments <- FALSE

  }

  if(missing(Ellipse.IC.95)){

    Ellipse.IC.95 <- FALSE

  }

  if(missing(RDA.object)){

    is.RDA.object <- FALSE

  }else if(missing(RDA.object)==FALSE){

    is.RDA.object <- TRUE

  }

  if(missing(Heat.map.graph)){

    Heat.map.graph <- FALSE

  }

  if(missing(Overlaying.graphs)){

    Overlaying.graphs <- FALSE

  }

  if(missing(Dim.a)){
    Dim.a <- 1
  }

  if(missing(Dim.b)){
    Dim.b <- 2
  }

  #Empty data frame for missing parameters
  barycentre <- data.frame(0,0,0,0)
  vec1 <- c(Dim.a, Dim.b)
  vec11 <- paste("Dim.",vec1,sep="")
  vec11b <- paste(vec11,"_b",sep="")
  names(barycentre) <- c(vec11,vec11b)

  barycentre_ind <- barycentre
  barycentre_ind$facteur_IC95.1 <- 0
  barycentre_ind$facteur_IC95.2 <- 0


  Table_RDA <- data.frame(0)

  #Get columns number of factors
  colnumber <- which(names(complete.data.set)%in%factor.names)

  #Extract coords for individuals and variables
  data_ind_ACP <- cbind(data.frame(cbind(PCA.object$ind$coord[,c(1:5)])), complete.data.set[,c(colnumber)])
  data_var_ACP <- cbind(data.frame(cbind(PCA.object$var$coord[,c(1:5)])))

  #Define considered "factor columns" as factors

  colnumber2 <- which(names(data_ind_ACP)%in%factor.names)
  for (i in unique(colnumber2)){

    data_ind_ACP[,i] <- as.factor(data_ind_ACP[,i])

  }

  #Sample names : individuals

  if(missing(sample.column)==T){

    data_ind_ACP$Samples <- row.names(complete.data.set)

  }else if(is.numeric(sample.column)==T){

    data_ind_ACP$Samples <- complete.data.set[,sample.column]

  }else if(is.character(sample.column)==T){

    colnumbersample <- which(names(complete.data.set)%in%sample.column)
    data_ind_ACP$Samples <- complete.data.set[,colnumbersample]

  }else{NULL}

  #Variables names
  data_var_ACP$Var.names <- rownames(data_var_ACP)
  Var.names_column <- which(names(data_var_ACP)%in%"Var.names")


  #Barycenter calculation
  Dima <- paste("Dim.",Dim.a,sep="")
  Dimb <- paste("Dim.",Dim.b,sep="")

  Dima2 <- which(names(data_ind_ACP)%in%Dima)
  Dimb2 <- which(names(data_ind_ACP)%in%Dimb)

  if(missing(Barycenter.Ellipse.Fac1)==TRUE & missing(Barycenter.Ellipse.Fac2)==TRUE & missing(Barycenter.Ellipse.Fac3)==TRUE){

    void <- 1

  }else if(missing(Barycenter.Ellipse.Fac2)==TRUE & missing(Barycenter.Ellipse.Fac3)==TRUE){

    F1 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac1)
    min_comb <- as.numeric(min(summary(data_ind_ACP[,F1])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,F1])))

    if(min_comb <= 1){

      warning("WRONG factor choices used for ellipses representation (1 unique individual / factor combination)")

    }else if(min_comb < 3 & min_comb > 1){

      message("Problematic factor choices used for ellipses representation (less than 3 unique individuals / factor combination)")

    }

    if(min_comb >= 2){

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Barycenter.Ellipse.Fac1
      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <- stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[2] <- "sd.1"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = length) ; names(IC95_n)[2] <- "nb.1"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[2] <- "sd.2"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1], data = data_ind_ACP, FUN = length) ; names(IC95_n)[2] <- "nb.2"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1

      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

    }

  }else if(missing(Barycenter.Ellipse.Fac2)==FALSE & missing(Barycenter.Ellipse.Fac3)==TRUE){

    F1 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac1)
    F2 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac2)

    data_ind_ACP$nb_comb <- as.factor(paste(data_ind_ACP[,F1], data_ind_ACP[,F2]))

    min_comb <- as.numeric(min(summary(data_ind_ACP[,ncol(data_ind_ACP)])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,ncol(data_ind_ACP)])))

    if(min_comb <= 1){

      warning("WRONG factor choices used for ellipses representation (1 unique individual / factor combination)")

    }else if(min_comb < 3 & min_comb > 1){

      message("Problematic factor choices used for ellipses representation (less than 3 unique individuals / factor combination)")

    }else{NULL}

    if(min_comb >= 2){

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Barycenter.Ellipse.Fac1
      names(barycentre)[2] <- Barycenter.Ellipse.Fac2

      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[3] <- "sd.1"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_sd)[2] <- Barycenter.Ellipse.Fac2

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = length) ; names(IC95_n)[3] <- "nb.1"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_n)[2] <- Barycenter.Ellipse.Fac2

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[3] <- "sd.2"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_sd)[2] <- Barycenter.Ellipse.Fac2

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2], data = data_ind_ACP, FUN = length) ; names(IC95_n)[3] <- "nb.2"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_n)[2] <- Barycenter.Ellipse.Fac2
      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

    }
  }else if(missing(Barycenter.Ellipse.Fac2)==FALSE & missing(Barycenter.Ellipse.Fac3)==FALSE){

    F1 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac1)
    F2 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac2)
    F3 <- which(names(data_ind_ACP)%in%Barycenter.Ellipse.Fac3)

    data_ind_ACP$nb_comb <- as.factor(paste(data_ind_ACP[,F1], data_ind_ACP[,F2], data_ind_ACP[,F3]))

    min_comb <- as.numeric(min(summary(data_ind_ACP[,ncol(data_ind_ACP)])))
    max_comb <- as.numeric(max(summary(data_ind_ACP[,ncol(data_ind_ACP)])))

    if(min_comb <= 1){

      message("WRONG factor choices used for ellipses representation (1 unique individual / factor combination)")

    }else if(min_comb < 3 & min_comb > 1){

      message("Problematic factor choices used for ellipses representation (less than 3 unique individuals / factor combination)")

    }else{NULL}

    if(min_comb >= 2){

      barycentre1<-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = mean)
      names(barycentre1)[ncol(barycentre1)] <- Dima

      barycentre2<-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = mean)
      names(barycentre2)[ncol(barycentre2)] <- Dimb

      barycentre<-dplyr::inner_join(barycentre1, barycentre2)
      names(barycentre)[1] <- Barycenter.Ellipse.Fac1
      names(barycentre)[2] <- Barycenter.Ellipse.Fac2
      names(barycentre)[3] <- Barycenter.Ellipse.Fac3

      names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))]<-paste(names(barycentre)[c((ncol(barycentre)-1):ncol(barycentre))], "b", sep="_") #Numero des colonnes
      barycentre_ind<-dplyr::left_join(data_ind_ACP, barycentre)

      #Calcul de l'intervalle de confiance 95%
      IC95_sd <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[4] <- "sd.1"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_sd)[2] <- Barycenter.Ellipse.Fac2
      names(IC95_sd)[3] <- Barycenter.Ellipse.Fac3

      IC95_n <-stats::aggregate(data_ind_ACP[,Dima2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = length) ; names(IC95_n)[4] <- "nb.1"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_n)[2] <- Barycenter.Ellipse.Fac2
      names(IC95_n)[3] <- Barycenter.Ellipse.Fac3

      IC95_1 <- dplyr::inner_join(IC95_sd,IC95_n)

      IC95_sd <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = stats::sd) ; names(IC95_sd)[4] <- "sd.2"
      names(IC95_sd)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_sd)[2] <- Barycenter.Ellipse.Fac2
      names(IC95_sd)[3] <- Barycenter.Ellipse.Fac3

      IC95_n <-stats::aggregate(data_ind_ACP[,Dimb2]~data_ind_ACP[,F1]+data_ind_ACP[,F2]+data_ind_ACP[,F3], data = data_ind_ACP, FUN = length) ; names(IC95_n)[4] <- "nb.2"
      names(IC95_n)[1] <- Barycenter.Ellipse.Fac1
      names(IC95_n)[2] <- Barycenter.Ellipse.Fac2
      names(IC95_n)[3] <- Barycenter.Ellipse.Fac3

      IC95_2 <- dplyr::inner_join(IC95_sd,IC95_n)

      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_1)
      barycentre_ind<-dplyr::inner_join(barycentre_ind, IC95_2)

      barycentre_ind$facteur_IC95.1 <- ((1.96*barycentre_ind$sd.1)/sqrt(barycentre_ind$nb.1))
      barycentre_ind$facteur_IC95.2 <- ((1.96*barycentre_ind$sd.2)/sqrt(barycentre_ind$nb.2))

    }
  }else{NULL}

  #Arguments for plot
  #Individuals
  data_ind_ACP$IPZ <- 2

  if(Ind.min.size==FALSE){

    Ind.min.size <- 2

  }

  if (factor.sizes==FALSE & Ind.min.size!=FALSE){

    data_ind_ACP$IPZ <- Ind.min.size
    factor.sizes.forced <- 1

  }else if (factor.sizes!=FALSE & Ind.min.size!=FALSE){

    factor.sizes.forced <- factor.sizes
    IPZf <- which(names(data_ind_ACP)%in%factor.sizes)
    data_ipz <- data.frame(unique(data_ind_ACP[,IPZf]))
    names(data_ipz) <- factor.sizes
    data_ipz$IPZ2 <- seq(Ind.min.size, (Ind.min.size+nrow(data_ipz)-1), by=1)

    data_ind_ACP <- dplyr::left_join(data_ind_ACP, data_ipz)
    data_ind_ACP$IPZ <- data_ind_ACP$IPZ2

  }


  #Barycenter
  barycentre$BCZ <- 3
  barycentre$BCZfill <- "white"
  barycentre$BCZcol <- "black"


  if(Barycenter==FALSE){

    barycentre$BCZfill <- NA
    barycentre$BCZcol <- NA
    barycentre$BCZ <- 0


  }

  if(Barycenter.min.size!=FALSE){

    barycentre$BCZ <- Barycenter.min.size

  }

  if(Barycenter!=FALSE & factor.colors==FALSE){

    colBar <- which(names(barycentre)%in%Barycenter.Ellipse.Fac1)
    barycentre$BCZfill <- barycentre[,colBar]


  }

  if(Barycenter!=FALSE & factor.colors!=FALSE){

    colBar <- which(names(barycentre)%in%factor.colors)
    barycentre$BCZfill <- barycentre[,colBar]


  }



  #Ellipses

  barycentre_ind$EFill <- "white"
  barycentre_ind$Ecol <- "black"
  barycentre_ind$CBE <- "black"

  ELT <- 1

  if (Ellipse.IC.95==FALSE){

    barycentre_ind$EFill <- NA
    barycentre_ind$Ecol <- NA
    barycentre_ind$CBE <- NA

  }else if (Ellipse.IC.95==TRUE & factor.colors==FALSE){

    colBar <- which(names(barycentre_ind)%in%Barycenter.Ellipse.Fac1)

    barycentre_ind$EFill <- barycentre_ind[,colBar]

  }else if (Ellipse.IC.95==TRUE & factor.colors!=FALSE){

    colBar <- which(names(barycentre_ind)%in%factor.colors)

    barycentre_ind$EFill <- barycentre_ind[,colBar]

  }

  if(factor.col.border.ellipse!=FALSE){

    colBar <- which(names(barycentre_ind)%in%factor.col.border.ellipse)
    barycentre_ind$CBE <- barycentre_ind[,colBar]

  }

  if(ellipse.line.type!=FALSE){

    ELT <- ellipse.line.type

  }

  #Segments

  if (missing(Segment.line.type)==TRUE){

    SLT <- 1

  }else if (missing(Segment.line.type)==FALSE){

    SLT <- Segment.line.type

  }

  if (missing(Segment.line.size)==TRUE){

    SLS <- 0

  }else if (missing(Segment.line.size)==FALSE){

    SLS <- Segment.line.size

  }

  SLC <- "black"

  if(Segments==FALSE | missing(Segments)==TRUE){

    SLC = NA

  }

  #Default variable circle and label sizes
  if(missing(Var.label.size)==TRUE){
    kz <- 4
  }else if (missing(Var.label.size)==FALSE){
    kz <- Var.label.size
  }

  if(missing(Var.circle.size)==TRUE){
    k <- 1
  }else if (missing(Var.circle.size)==FALSE){
    k <- Var.circle.size
  }


  colnumber_dima3 <- Dima
  Dima3 <- which(names(barycentre_ind)%in%colnumber_dima3)

  colnumber_dima4 <- paste(Dima, "_b", sep="")
  Dima4 <- which(names(barycentre_ind)%in%colnumber_dima4)

  colnumber_dima5 <- paste(Dima, "_b", sep="")
  Dima5 <- which(names(barycentre)%in%colnumber_dima4)

  colnumber_dimb3 <- Dimb
  Dimb3 <- which(names(barycentre_ind)%in%colnumber_dimb3)

  colnumber_dimb4 <- paste(Dimb, "_b", sep="")
  Dimb4 <- which(names(barycentre_ind)%in%colnumber_dimb4)

  colnumber_dimb5 <- paste(Dimb, "_b", sep="")
  Dimb5 <- which(names(barycentre)%in%colnumber_dimb5)


  VD1 <- which(names(data_var_ACP)%in%Dima)
  VD2 <- which(names(data_var_ACP)%in%Dimb)



  #IND colors and shape default parameters
  if(factor.colors==FALSE){

    data_ind_ACP$f.colors <- "one.col"


  }else if(factor.colors!=FALSE){

    FC10 <- which(names(data_ind_ACP)%in%factor.colors)
    data_ind_ACP$f.colors <- data_ind_ACP[,FC10]

  }

  FC <- which(names(data_ind_ACP)%in%"f.colors")

  if(factor.shapes==FALSE){

    data_ind_ACP$f.shapes <- "one.shape"

  }else if(factor.shapes!=FALSE){

    FS10 <- which(names(data_ind_ACP)%in%factor.shapes)
    data_ind_ACP$f.shapes <- data_ind_ACP[,FS10]

  }

  FS <- which(names(data_ind_ACP)%in%"f.shapes")

  vector_shapes <- c(21,22,24,25,23)
  vector_shapes <- vector_shapes[1:length(unique(data_ind_ACP[,FS]))]

  d1 <- paste("Dim1 (",round(PCA.object$eig[1,2],1),"%)", sep="")
  d2 <- paste("Dim2 (",round(PCA.object$eig[2,2],1),"%)", sep="")
  d3 <- paste("Dim3 (",round(PCA.object$eig[3,2],1),"%)", sep="")
  d4 <- paste("Dim4 (",round(PCA.object$eig[4,2],1),"%)", sep="")
  d5 <- paste("Dim5 (",round(PCA.object$eig[5,2],1),"%)", sep="")

  da <- ifelse(Dim.a==1, d1,
               ifelse(Dim.a==2, d2,
                      ifelse(Dim.a==3, d3,
                             ifelse(Dim.a==4, d4,
                                    ifelse(Dim.a==5, d5, NULL)))))

  db <- ifelse(Dim.b==1, d1,
               ifelse(Dim.b==2, d2,
                      ifelse(Dim.b==3, d3,
                             ifelse(Dim.b==4, d4,
                                    ifelse(Dim.b==5, d5, NULL)))))




  #Defaut widths
  if(missing(width.PCA.ind.graph)==TRUE){

    WidthIG <- 1

  }else if(missing(width.PCA.ind.graph)==FALSE){

  WidthIG <- width.PCA.ind.graph

  }

  if(missing(width.PCA.var.graph)==TRUE){

    WidthVG <- 1

  }else if(missing(width.PCA.var.graph)==FALSE){

    WidthVG <- width.PCA.var.graph

  }

  if(missing(width.heat.map.graph)==TRUE){

    WidthHM <- 0.3

  }else if(missing(width.heat.map.graph)==FALSE){

    WidthHM <- width.heat.map.graph

  }


  #legends

  factor.colors.legend <- NA
  factor.shapes.legend <- NA
  factor.sizes.legend <- NA



  if(factor.colors!=FALSE){
    factor.colors.legend <- factor.colors
  }
  if(factor.shapes!=FALSE){
    factor.shapes.legend <- factor.shapes
  }
  if(factor.sizes!=FALSE){
    factor.sizes.legend <- factor.sizes.forced
  }

  #RDA table output as graph

  if(is.RDA.object==FALSE){

    RDA.table.graph <- FALSE

  }else if(is.RDA.object==TRUE){

  Table_RDA <- as.data.frame(RVAideMemoire::MVA.anova(RDA.object))
  Table_RDA$Unconstrained.var.percent <- round((100*Table_RDA$Variance)/(sum(Table_RDA$Variance)),2)
  Table_RDA$Sign.p.val <- NA

  for (i in 1:(nrow(Table_RDA)-1)){

    if (Table_RDA[i,4] <= 0.001){Table_RDA[i,6] <- "***"}
    else if(Table_RDA[i,4] < 0.01 & Table_RDA[i,4] > 0.001){Table_RDA[i,6] <- "**"}
    else if(Table_RDA[i,4]<0.05 & Table_RDA[i,4] >= 0.01){Table_RDA[i,6] <- "*"}
    else if(Table_RDA[i,4]>0.05){Table_RDA[i,6] <- "ns"}
    else{NULL}


  }

  names(Table_RDA)[3] <- "F.val"
  names(Table_RDA)[5] <- paste("Unconstr.", "Var(%)", sep="\n")
  names(Table_RDA)[6] <- paste("Sign", "p.val", sep="\n")

  Table_RDA$Variance <- round(Table_RDA$Variance,2)
  names(Table_RDA)[2] <- "Var"

  Table_RDA$F.val <- round(Table_RDA$F.val,2)

  Table_RDA$Factor <- rownames(Table_RDA)
  Table_RDA$Factor <- gsub("Residual", "residuals", Table_RDA$Factor)

  tab <- ggpubr::ggtexttable(Table_RDA, theme = ggpubr::ttheme("light", base_size = Size.RDA.table.graph), rows=NULL)
  tab <- ggpubr::tab_add_title(tab, text = "Redundancy analysis")
  tab <- ggpubr::table_cell_font(tab, column = c(3,4,6),row = (nrow(Table_RDA)+2),
                                 face="italic", color="darkgrey", size=8)

  tab <- ggpubr::tab_add_border(tab)
  Table_RDA_graph <- tab
  Table_RDA_graph <<- Table_RDA_graph

  }


  if(missing(RDA.table.graph)==T | RDA.table.graph==F){

    HeightRDA <- 0

  }else if(missing(RDA.table.graph)==F & RDA.table.graph==T){

    if(missing(RDA.table.graph.height)==T){

      HeightRDA <- 1

    }else if(missing(RDA.table.graph.height)==F){

      HeightRDA <- RDA.table.graph.height

    }else{NULL}

  }else{NULL}



  #Heatmap for variable cos2, contrib or cor to dims

  if(missing(Heat.map.graph)==T | Heat.map.graph==F){

    WidthHM <- 0

  }else if(missing(Heat.map.graph)==F & Heat.map.graph==T){

    if(var.parameter.heat.map=="coord"){

      matrixHM <- as.data.frame(PCA.object[[2]][1])
      names(matrixHM) <- gsub("coord.","",names(matrixHM))
      gng <- "ok"
      textvar <- "coordinates"

    }else if(var.parameter.heat.map=="cor"){

      matrixHM <- as.data.frame(PCA.object[[2]][2])
      names(matrixHM) <- gsub("cor.","",names(matrixHM))
      gng <- "ok"
      textvar <- "correlations"

    }else if(var.parameter.heat.map=="cos2"){

      matrixHM <- as.data.frame(PCA.object[[2]][3])
      names(matrixHM) <- gsub("cos2.","",names(matrixHM))
      gng <- "ok"
      textvar <- "cos2"

    }else if(var.parameter.heat.map=="contrib"){

      matrixHM <- as.data.frame(PCA.object[[2]][4])
      names(matrixHM) <- gsub("contrib.","",names(matrixHM))
      gng <- "ok"
      textvar <- "contributions"

    }else if (var.parameter.heat.map != "coord" | var.parameter.heat.map != "cor" |var.parameter.heat.map != "cos2" |var.parameter.heat.map != "contrib"){

      gng <- "nook"

    }else {NULL}

    if(gng=="nook"){

      warning("Please choose valid PCA variable parameter (coord, cor, cos2 or contrib")

    }else if (gng=="ok"){

      if(missing(Dims.heat.map)==TRUE){

        Dims.heat.map <- c(1,2)

      }else if(missing(Dims.heat.map)==FALSE){

        Dims.heat.map <- Dims.heat.map

      }

      DHM <- paste("Dim.",Dims.heat.map,sep="")
      colnumber_pheatmap <- which(names(matrixHM)%in%DHM)

      text_pheatmap <- matrixHM[,colnumber_pheatmap]

      if( Display.cell.values.heat.map==F){
        colheatmap <- NA

      }else if(Display.cell.values.heat.map==TRUE){
        colheatmap <- "black"

      }

      if(missing(Cluster.col.heat.map)==T | Cluster.col.heat.map==F){

        CC <- F

      }else if(missing(Cluster.col.heat.map)==F & Cluster.col.heat.map==T){

        CC <- T

      }

      if(missing(Cluster.row.heat.map)==T | Cluster.row.heat.map==F){

        CR <- F

      }else if(missing(Cluster.row.heat.map)==F & Cluster.row.heat.map==T){

        CR <- T

      }


      Pheatmap_var_graph <- pheatmap::pheatmap(matrixHM[,colnumber_pheatmap],
                                               display_numbers = round(text_pheatmap,2),
                                               number_color = colheatmap,
                                               cluster_rows = CR,
                                               cluster_cols = CC,
                                               angle_col = 0)

      if(width.cell.heat.map!=FALSE & height.cell.heat.map==FALSE){

        Pheatmap_var_graph <- pheatmap::pheatmap(matrixHM[,colnumber_pheatmap],
                                                 display_numbers = round(text_pheatmap,2),
                                                 number_color = colheatmap,
                                                 cluster_rows = CR,
                                                 cluster_cols = CC,
                                                 angle_col = 0,
                                                 cellwidth = width.cell.heat.map)


      }else if(width.cell.heat.map==FALSE & height.cell.heat.map!=FALSE){

        Pheatmap_var_graph <- pheatmap::pheatmap(matrixHM[,colnumber_pheatmap],
                                                 display_numbers = round(text_pheatmap,2),
                                                 number_color = colheatmap,
                                                 cluster_rows = CR,
                                                 cluster_cols = CC,
                                                 angle_col = 0,
                                                 cellheight = height.cell.heat.map)


      }else if(width.cell.heat.map!=FALSE & height.cell.heat.map!=FALSE){

        Pheatmap_var_graph <- pheatmap::pheatmap(matrixHM[,colnumber_pheatmap],
                                                 display_numbers = round(text_pheatmap,2),
                                                 number_color = colheatmap,
                                                 cluster_rows = CR,
                                                 cluster_cols = CC,
                                                 angle_col = 0,
                                                 cellwidth = width.cell.heat.map,
                                                 cellheight = height.cell.heat.map)


      }

      Pheatmap_var_graph <- ggplotify::as.ggplot(Pheatmap_var_graph)

      Pheatmap_var_graph <- Pheatmap_var_graph + egg::theme_article()+
        ggplot2::labs(title=paste("Variable ", textvar, " to dims", sep=""))+
        ggplot2::xlab(" ")+
        ggplot2::ylab("Void")+
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),

                       axis.title.x = ggplot2::element_text(colour=NA),
                       axis.ticks.x = ggplot2::element_line(colour=NA),
                       axis.text.x = ggplot2::element_text(colour=NA))

      Pheatmap_var_graph <<- Pheatmap_var_graph

    }else{NULL}
  }


  #Dummy objects, to be overwrite

  #Individuals PCA Plot
  BCZ_column <- which(names(barycentre)%in%"BCZ")
  BCZcol_column <- which(names(barycentre)%in%"BCZcol")
  BCZfill_column <- which(names(barycentre)%in%"BCZfill")

  Efill_column <- which(names(barycentre_ind)%in%"EFill")
  Ecol_column <- which(names(barycentre_ind)%in%"Ecol")
  CBE_column <- which(names(barycentre_ind)%in%"CBE")

  facteur_IC95.1_column <- which(names(barycentre_ind)%in%"facteur_IC95.1")
  facteur_IC95.2_column <- which(names(barycentre_ind)%in%"facteur_IC95.2")

  IPZ_column <- which(names(data_ind_ACP)%in%"IPZ")

  bie <- barycentre_ind[!duplicated(barycentre_ind[,facteur_IC95.1_column]),]



  PCA_ind_graphic <- ggplot2::ggplot()+
    ggplot2::geom_segment(ggplot2::aes(x = barycentre_ind[,Dima3], y = barycentre_ind[,Dimb3], xend = barycentre_ind[,Dima4], yend = barycentre_ind[,Dimb4]), linetype=2,linewidth=SLS, data = barycentre_ind, color = SLC)

  if(Ellipse.IC.95==FALSE){

    PCA_ind_graphic <- PCA_ind_graphic + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                   a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                   angle = 0, color=I(bie[,Ecol_column])),fill=NA, alpha=0.1, data=bie)

  }else if(Ellipse.IC.95==TRUE){


    if(factor.col.border.ellipse!=FALSE){


        PCA_ind_graphic <- PCA_ind_graphic + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                                a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                                angle = 0,fill=bie[,Efill_column], color=bie[,CBE_column]), alpha=0.1, data=bie, size=0.1, linetype=ELT)


    }else if(factor.col.border.ellipse==FALSE){

    PCA_ind_graphic <- PCA_ind_graphic + ggforce::geom_ellipse(ggplot2::aes(x0 = bie[,Dima4], y0 = bie[,Dimb4],
                                                                             a = (bie[,facteur_IC95.1_column]), b = (bie[,facteur_IC95.2_column]),
                                                                             angle = 0,fill=bie[,Efill_column]), color="black", alpha=0.1, data=bie, size=0.1, linetype=ELT)

    }
  }

  if(factor.colors==FALSE){
    data_ind_ACP[,FC] <- "black"
  }


    PCA_ind_graphic <- PCA_ind_graphic + ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, alpha=1)+
    ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, fill=NA, colour="black")+
    ggplot2::geom_point(ggplot2::aes(x=barycentre[,Dima5], y=barycentre[,Dimb5], fill=barycentre[,BCZfill_column], size=barycentre[,BCZ_column]), colour="black", shape=23, data = barycentre,show.legend = F)+
    ggplot2::scale_shape_manual(values = vector_shapes)


    if(factor.sizes==FALSE){

    PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend)+
    ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
    ggplot2::scale_size_identity()+ggplot2::guides(size = "none")

    }else if(factor.sizes!=F){

    PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend, size=factor.sizes.legend)+
    ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
    ggplot2::scale_size_identity()

    }else{NULL}

    if(factor.colors==FALSE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::guides(fill = "none")

    }else if(factor.colors!=FALSE){

      PCA_ind_graphic <- PCA_ind_graphic

    }else{NULL}


    if(factor.shapes==FALSE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::guides(shape = "none")

    }else if(factor.shapes!=FALSE){

      PCA_ind_graphic <- PCA_ind_graphic

    }else{NULL}

    PCA_ind_graphic <- PCA_ind_graphic+ ggplot2::ggtitle("PCA : Sample projection")+
    ggplot2::xlab(da)+
    ggplot2::ylab(db)+

    egg::theme_article()+
    ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
    ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))

    if(is.character(color.palette)==TRUE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::scale_fill_manual(values=color.palette)

    }


  PCA_ind_graphic <<- PCA_ind_graphic
  PCA_ind_graphic

  data_var_ACP <<- data_var_ACP
  var.title <- "PCA : Variable projection"

  if(is.character(Var.selected)==TRUE){

    data_var_ACP <- data_var_ACP[data_var_ACP$Var.names %in% Var.selected,]
    var.title <- "PCA : Variable subset projection"
  }

  PCA_var_graphic <- ggplot2::ggplot()+
    ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = 1*k), color = "grey", linetype = 2, alpha = 0.5)+
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = data_var_ACP[,VD1]*k, yend = data_var_ACP[,VD2]*k),
                          colour = "black", data = data_var_ACP, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")))+
    ggplot2::geom_text(ggplot2::aes(x=data_var_ACP[,VD1]*(k+(1/10*k)), y=data_var_ACP[,VD2]*(k+(1/10*k)), label=data_var_ACP[,Var.names_column]), color = "black", size = kz, data=data_var_ACP)+



    ggplot2::ggtitle(var.title)+
    egg::theme_article()+
    ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
    ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))+

    ggplot2::geom_segment(ggplot2::aes(x = -1*k, y = 0, xend = 1*k, yend = 0), color="grey", alpha=0.7)+
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = -1*k, xend = 0, yend = 1*k), color="grey", alpha=0.7)+

    ggplot2::xlab(d1)+
    ggplot2::ylab(d2)

  PCA_var_graphic <<- PCA_var_graphic

  if(Overlaying.graphs==TRUE){

    Overlaying_PCA_ind_var <- ggplot2::ggplot()+
      ggplot2::geom_segment(ggplot2::aes(x = barycentre_ind[,Dima3], y = barycentre_ind[,Dimb3], xend = barycentre_ind[,Dima4], yend = barycentre_ind[,Dimb4]),
                            linetype=2,linewidth=SLS, data = barycentre_ind, color = SLC)

    if(missing(Ellipse.IC.95)==TRUE | Ellipse.IC.95==F){

      Overlaying_PCA_ind_var <- Overlaying_PCA_ind_var + ggforce::geom_ellipse(ggplot2::aes(x0 = barycentre_ind[,Dima4], y0 = barycentre_ind[,Dimb4],
                                                                                   a = (barycentre_ind[,facteur_IC95.1_column]), b = (barycentre_ind[,facteur_IC95.2_column]),
                                                                                   angle = 0, color=I(barycentre_ind[,Ecol_column])),fill=NA, alpha=0.1, data=barycentre_ind)

    }else if(missing(Ellipse.IC.95)==F & Ellipse.IC.95==T){

      Overlaying_PCA_ind_var <- Overlaying_PCA_ind_var + ggforce::geom_ellipse(ggplot2::aes(x0 = barycentre_ind[,Dima4], y0 = barycentre_ind[,Dimb4],
                                                                                   a = (barycentre_ind[,facteur_IC95.1_column]), b = (barycentre_ind[,facteur_IC95.2_column]),
                                                                                   angle = 0,fill=barycentre_ind[,Efill_column], color=I(barycentre_ind[,Ecol_column])), alpha=0.1, data=barycentre_ind)


    }

    Overlaying_PCA_ind_var <- Overlaying_PCA_ind_var +ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, alpha=1)+
      ggplot2::geom_point(ggplot2::aes(x=data_ind_ACP[,Dima2], y=data_ind_ACP[,Dimb2], fill=data_ind_ACP[,FC], shape=data_ind_ACP[,FS], size=data_ind_ACP[,IPZ_column]), data=data_ind_ACP, fill=NA, colour="black")+
      ggplot2::geom_point(ggplot2::aes(x=barycentre[,Dima5], y=barycentre[,Dimb5], fill=barycentre[,BCZfill_column], size=barycentre[,BCZ_column], colour=barycentre[,BCZcol_column]), shape=23, data = barycentre)+
      ggplot2::scale_shape_manual(values = vector_shapes)

    if(factor.sizes==FALSE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend)+
        ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
        ggplot2::scale_size_identity()+ggplot2::guides(size = "none")

    }else if(factor.sizes!=FALSE){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::labs(fill=factor.colors.legend, shape=factor.shapes.legend, size=factor.sizes.legend)+
        ggplot2::guides(fill=ggplot2::guide_legend(override.aes = list(shape=21)))+
        ggplot2::scale_size_identity()

    }else{NULL}

    if(missing(factor.colors)==T){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::guides(fill = "none")

    }else if(missing(factor.colors)==F){

      PCA_ind_graphic <- PCA_ind_graphic

    }else{NULL}


    if(missing(factor.shapes)==T){

      PCA_ind_graphic <- PCA_ind_graphic+ggplot2::guides(shape = "none")

    }else if(missing(factor.shapes)==F){

      PCA_ind_graphic <- PCA_ind_graphic

    }else{NULL}

    PCA_ind_graphic <- PCA_ind_graphic+ ggplot2::ggtitle("PCA : Sample projection")+
      ggplot2::xlab(da)+
      ggplot2::ylab(db)+

      egg::theme_article()+
      ggplot2::theme(strip.background =ggplot2::element_rect(fill="white"))+
      ggplot2::theme(strip.text = ggplot2::element_text(colour = 'black'))+

      ggplot2::geom_segment(ggplot2::aes(x = -1*k, y = 0, xend = 1*k, yend = 0), color="grey", alpha=0.7)+
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = -1*k, xend = 0, yend = 1*k), color="grey", alpha=0.7)+

      ggplot2::xlab(d1)+
      ggplot2::ylab(d2)

    Overlaying_PCA_ind_var <<- Overlaying_PCA_ind_var
    Overlaying_PCA_ind_var

  }else if(Overlaying.graphs==FALSE | missing(Overlaying.graphs)==TRUE){

    if(Heat.map.graph==FALSE & RDA.table.graph==FALSE){

      Separated_PCA_ind_var <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic, nrow=1, ncol=2, widths = c(WidthIG,WidthVG))
      Separated_PCA_ind_var <<- Separated_PCA_ind_var
      Separated_PCA_ind_var

    }else if(Heat.map.graph==TRUE & RDA.table.graph==FALSE){

      Separated_PCA_ind_var_HM <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic,Pheatmap_var_graph, nrow=2, ncol=2, widths = c(WidthIG,WidthVG,WidthHM))
      Separated_PCA_ind_var_HM <<- Separated_PCA_ind_var_HM
      Separated_PCA_ind_var_HM

    }else if(Heat.map.graph==FALSE & RDA.table.graph==TRUE){

      Separated_PCA_ind_var <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic, nrow=1, ncol=2, widths = c(WidthIG,WidthVG))
      Separated_PCA_ind_var_RDAtable <- ggpubr::ggarrange(Separated_PCA_ind_var, tab, nrow=2, ncol=1, heights = c(1,HeightRDA))
      Separated_PCA_ind_var_RDAtable <<- Separated_PCA_ind_var_RDAtable
      Separated_PCA_ind_var_RDAtable

    }else if(Heat.map.graph==TRUE & RDA.table.graph==TRUE){

      Separated_PCA_ind_var_HM_RDAtable <- ggpubr::ggarrange(PCA_ind_graphic,PCA_var_graphic,Pheatmap_var_graph,tab, nrow=2, ncol=2,
                                                             widths = c(WidthIG,WidthVG,WidthHM), heights = c(1,1,1, HeightRDA))
      Separated_PCA_ind_var_HM_RDAtable <<- Separated_PCA_ind_var_HM_RDAtable
      Separated_PCA_ind_var_HM_RDAtable

    }else{NULL}

  }else{NULL}


}
