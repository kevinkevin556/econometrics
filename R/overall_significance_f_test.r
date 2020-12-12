.overall_f_statistics <- function(X, y){
    n <- dim(X)[1]
    k <- dim(X)[2]
    intercept <- matrix(rep(1, length(y)), ncol=1)
    intecept_only_residual.2 <- sum(residual(intercept, y)^2)
    regression_residual.2 <- sum(residual(X, y)^2)
    f_stat <- (n-k)/k * (intecept_only_residual.2 - regression_residual.2)/regression_residual.2
    return(f_stat)
}   

#' @export
overall_significance_f_test <- function(X, y){
    n <- dim(X)[1]
    k <- dim(X)[2]
    f_stat <- .overall_f_statistics(X, y)

    return(TestResult(test_name = "Overall Significant F Test",
                      H0_description = "All coefficients (except intercept) equal 0: β1 = β2 = ... = βk = 0",
                      test_statistic = f_stat,
                      distribution = "f",
                      df1 = k,
                      df2 = n-k))
}