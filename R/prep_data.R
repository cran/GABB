#' prep_data function
#'
#' This function helps user to transform and scale quantitative defined data (columns identitified from a given dataset)
#' for better PCA and RDA outputs.
#'
#' @param data A data frame with at least one numeric column
#' @param quantitative_columns Numeric vector of numeric columns position : c(1,2,4)
#' @param transform_data_method Method of quantitative data transformation : "log", "sqrt4", "clr", "none"
#' @param scale_data TRUE or FALSE. TRUE => scaling of quantitative columns
#'
#' @return The transformed/scaled quantitative data frame
#' @export
#'
#' @examples
#'
#' prep_data(mtcars, c(1:7), "log", TRUE)
#'
#'

prep_data <- function(data,
                      quantitative_columns,
                      transform_data_method,
                      scale_data){

  #Check for non variable quantitative column
  sd_columns <- apply(data[,quantitative_columns],2,stats::sd)
  zero_sd_columns_pos <- as.numeric(which(sd_columns==0))
  zero_sd_columns_names <- names(sd_columns[sd_columns==0])

  if(length(zero_sd_columns_names)>0){

    quantitative_columns <- quantitative_columns[-zero_sd_columns_pos]
    data_quant <- data[,quantitative_columns]

    message(paste("The following columns were removed from the data quant due to 0 standard deviation (check raw data): ",
                  zero_sd_columns_names, sep=""))

  }else{data_quant <- data[,quantitative_columns]

  }

  #Transform data

  if(transform_data_method=="log"){
    data_quant <- log(data_quant)
  }else if (transform_data_method=="sqrt4"){
    data_quant <- (data_quant)^(1/4)
  }else if (transform_data_method=="clr"){
    data_quant <- Hotelling::clr(data_quant)
  }else if (transform_data_method=="none"){
    data_quant <- data_quant
  }else{data_quant <- data_quant}


  #Scale data

  if (scale_data==TRUE){
    data_quant <- scale(data_quant)
  }else if (scale_data==FALSE){
    data_quant <- data_quant
  }else {data_quant <- scale(data_quant)}

  data_quant <<- as.data.frame(data_quant)
  initial_data_with_quant_transformed <- "void"
  initial_data_with_quant_transformed <<- cbind(data_quant, data[-c(quantitative_columns)])

}


