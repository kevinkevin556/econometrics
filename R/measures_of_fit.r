#' @export
r_squared <- function(X, y){
    y.bar <- mean(y)
    e.hat <- residual(X, y)
    
    1 - sum(e.hat^2)/sum((y-y.bar)^2)
}

#' @export
adjusted_r_squared <- function(X, y){
    y.bar <- mean(y)
    e.hat <- residual(X, y)
    n <- dim(X)[1]
    k <- dim(X)[2]

    1 - (n-1)*sum(e.hat^2)/((n-k)*sum((y-y.bar)^2))
}

#' @export
r.tilde_squared <- function(X, y){
    y.bar <- mean(y)
    e.tilde <- prediction_error(X, y)
    
    1 - sum(e.tilde^2)/sum((y-y.bar)^2)
}