OLSResult <- function(info, coef, std_error, t_ratio, stat_signif){
    info <- data.frame(unlist(info))
    colnames(info) <- "Model: OLS"
    
    estimation <- data.frame(coef,
                             std_error,
                             format(t_ratio, digits=3),
                             format(stat_signif, width=7, justify="right"))
    colnames(estimation) <- c("coef", "std err", "t", "signif")
    
    result <- list(info=info, estimation=estimation)
    return(result)
}