.ols <- function(X, y){
    solve(t(X) %*% X, t(X) %*% y)
}


#' @export
ols <- function(formula, data, robust=FALSE){
    X <- model.matrix(formula, data)
    y <- model.frame(formula, data)[, 1]
    n <- dim(X)[1]
    k <- dim(X)[2]

    coef <- .ols(X, y)
    std_err <- standard_error(X, y, robust)
    residual <- y - X %*% coef

    t_ratio <- coef / std_err[["value"]]
    f_stat <- t_ratio^2
    stat_signif <- significant_level(f_stat, df=n-k)
    
    overall_significance <- overall_significance_f_test(X, y)
    resid_normality <- jarque_bera_test(residual)


    info <- list(
        "Dep. Variable: " = toString(formula[2]),
        "R-squared: " = format(r_squared(X, y), digits = 3),
        "Adj. R-squared: " = format(adjusted_r_squared(X, y), digits = 3),
        "R-tilde-squared: " = format(r.tilde_squared(X, y), digits = 3),
        "Covariance Type: " = std_err[["cov_type"]]
    )

    return(OLSResult(info, coef, std_err[["value"]], t_ratio, stat_signif, overall_significance, resid_normality))
}

