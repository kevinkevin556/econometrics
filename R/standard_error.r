#' @export
homoskedastic_standard_error <- function(X, y){
    diag(homoskedastic_covariance_estimator(X, y))^(1/2)
}

#' @export
white_heteroskedastic_standard_error <- function(X, y){
    diag(white_heteroskedastic_covariance_estimator(X, y))^(1/2)
}


#' @export
stde <- homoskedastic_standard_error

#' @export
robust_stde0 <- white_heteroskedastic_standard_error