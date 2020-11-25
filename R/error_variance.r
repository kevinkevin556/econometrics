#' @export
error_variance_estimation <- function(X, y){
    e_hat <- residual(X, y)
    
    mean(e_hat^2)
}

#' @export
unbiased_error_variance_estimation <- function(X, y){
    e_hat <- residual(X, y)
    n <- dim(X)[1]
    k <- dim(X)[2]
    
    sum(e_hat^2)/(n-k)
}

#' @export
standardized_residual_error_variance_estimation <- function(X, y){
    e_bar <- standardized_residual(X, y)
    
    mean(e_bar^2)
}

# Notation

#' @export
sigma.hat.2 <- error_variance_estimation

#' @export
s.2 <- unbiased_error_variance_estimation

#' @export
sigma.bar.2 <- standardized_residual_error_variance_estimation