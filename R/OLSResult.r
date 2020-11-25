OLSResult <- function(info, estimation, std_error){
    info <- data.frame(unlist(info))
    colnames(info) <- "Model: OLS"
    
    estimation <- data.frame(estimation, std_error)
    colnames(estimation) <- c("coef", "std err")
    
    result <- list(info=info, estimation=estimation)
    return(result)
}