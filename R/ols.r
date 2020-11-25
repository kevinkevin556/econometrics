.ols <- function(X, y){
    solve(t(X) %*% X) %*% t(X) %*% y # inv(X'X)(X'y)
}


#' @export
ols <- function(formula, data, robust=FALSE){
    X <- model.matrix(formula, data)
    y <- model.frame(formula, data)[, 1]

    estimation <- .ols(X, y)
    if (robust=="white") {
        std_error <- robust_stde0(X, y)
    } else {
        robust <- "homoskadestic"
        std_error <- stde(X, y)
    }

    info <- list(
        "Dep. Variable: " = toString(formula[2]),
        "R-squared: " = format(r_squared(X, y), digits = 3),
        "Adj. R-squared: " = format(adjusted_r_squared(X, y), digits = 3),
        "R-tilde-squared: " = format(r.tilde_squared(X, y), digits = 3),
        "Covariance Type: " = robust
    )

    return(OLSResult(info, estimation, std_error))
}
