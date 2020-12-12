.jarque_bera <- function(X){
    n <- length(X)
    sk <- skewness(X)
    kr <- kurtosis(X)

    jb <- n * ((1/6)*sk^2 + (1/24)*(kr-3)^2)
    return(jb)
}

#' @export
jarque_bera_test <- function(X){
    jb_statistic <- .jarque_bera(X)
    return(TestResult(test_name = "Jarque-Bera Test",
                       H0_description = "Input X follows normal distribution",
                       test_statistic = jb_statistic,
                       distribution = "chisq",
                       df = 2))
}