#' @export
leverage_values <- function(X){
    `inv(X'X)` <- solve(t(X) %*% X)
    leverage_values <- apply(X, MARGIN = 1, FUN = function(xi){
                             t(xi) %*% `inv(X'X)` %*% xi
                    })
    return(leverage_values)
}
