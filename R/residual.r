#' @export
residual <- function(X, y){
    M <- orthogonal_projection_matrix(X)
    
    M %*% y
}

#' @export
prediction_error <- function(X, y){
    M.star <- prediction_error_tranformation_matrix(X)
    
    M.star %*% residual(X, y)
}


standardized_residual <- function(X, y){
    M.star <- prediction_error_tranformation_matrix(X)
    
    (M.star^(1/2)) %*% residual(X, y)
}

# Notations

#' @export
e.hat <- residual

#' @export
e.tilde <- prediction_error

#' @export
e.bar <- standardized_residual