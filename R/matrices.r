#' @export
identity_matrix <- function(X=NULL, n=NULL){
    if (is.null(n)) {
        n <- dim(X)[1]
    }        
    return(diag(n))
}

#' @export
projection_matrix <- function(X){
    P <- X %*% solve(t(X) %*% X) %*% t(X)
    return(P)
}

#' @export
orthogonal_projection_matrix <- function(X){
    M <- I(X) - P(X)
    return(M)
}

#' @export
prediction_error_tranformation_matrix <- function(X){
    M_star <- diag((1-leverage_values(X))^(-1))
    return(M_star)
}

# Notations

#' @export
I <- identity_matrix

#' @export
P <- projection_matrix

#' @export
M <- orthogonal_projection_matrix

#' @export
M_star <- prediction_error_tranformation_matrix

