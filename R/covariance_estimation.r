#' @export
homoskedastic_covariance_estimator <- function(X, y){
    s.2 <- unbiased_error_variance_estimation(X, y)
    
    solve(t(X) %*% X) * s.2
}

#' @export
white_heteroskedastic_covariance_estimator <- function(X, y){
    e.hat <- c(residual(X, y))
    `inv(X'X)` <- solve(t(X) %*% X)
    `X'DX` <- t(X) %*% diag(e.hat^2) %*% X
    
    `inv(X'X)` %*% `X'DX` %*% `inv(X'X)`
}

#' @export
hinkley_heteroskedastic_covariance_estimator <- function(X, y){
    e.hat <- c(residual(X, y))
    `inv(X'X)` <- solve(t(X) %*% X)
    `X'DX` <- t(X) %*% diag(e.hat^2) %*% X
    n <- dim(X)[1]
    k <- dim(X)[2]

    n/(n-k) * `inv(X'X)` %*% `X'DX` %*% `inv(X'X)`
}

#' @export
horn_heteroskedastic_covariance_estimator <- function(X, y){
    e.bar <- c(standardized_residual(X, y))
    `inv(X'X)` <- solve(t(X) %*% X)
    `X'DX` <- t(X) %*% diag(e.bar^2) %*% X

    `inv(X'X)` %*% `X'DX` %*% `inv(X'X)`
}

#' @export
mackinnon_heteroskedastic_covariance_estimator <- function(X, y){
    e.tilde <- prediction_error(X, y)
    `inv(X'X)` <- solve(t(X) %*% X)
    `X'DX` <- t(X) %*% diag(e.tilde^2) %*% X

    `inv(X'X)` %*% `X'DX` %*% `inv(X'X)`
}


# Notations and Abbreviations

#' @export
V.hat_beta.hat <- homoskedastic_covariance_estimator
#' @export
vce <- homoskedastic_covariance_estimator

#' @export
V.hat.hc0_beta.hat <- white_heteroskedastic_covariance_estimator
#' @export
robust_vce0 <- white_heteroskedastic_covariance_estimator

#' @export
V.hat.hc1_beta.hat <- hinkley_heteroskedastic_covariance_estimator
#' @export
robust_vce1 <- hinkley_heteroskedastic_covariance_estimator

#' @export
V.hat.hc2_beta.hat <- horn_heteroskedastic_covariance_estimator
#' @export
robust_vce2 <- horn_heteroskedastic_covariance_estimator

#' @export
V.hat.hc3_beta.hat <- mackinnon_heteroskedastic_covariance_estimator
#' @export
robust_vce3 <- mackinnon_heteroskedastic_covariance_estimator
