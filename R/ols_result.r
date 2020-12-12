#' @export
OLSResult <- function(info, coef, std_error, t_ratio, stat_signif, overall_significance, resid_normality){
    info <- data.frame(unlist(info))
    colnames(info) <- ""
    
    estimation <- data.frame(coef,
                             std_error,
                             format(t_ratio, digits=3),
                             format(stat_signif, width=7, justify="right"))
    colnames(estimation) <- c("coef", "std err", "t", "signif")
    
    result <- list(info=info,
                            estimation=estimation,
                            overall_significance=overall_significance,
                            residual_normality=resid_normality)
    # class(result) <- "OLSResult"
    return(result)
}


print.OLSResult <- function(ols_result){
    print("HI")
}