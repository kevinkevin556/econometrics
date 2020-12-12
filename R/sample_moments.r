variance <- function(X){
    mean(X^2) - mean(X)^2
}

standard_deviation <- function(X){
    variance(X)^(1/2)
}


mu <- mean
sigma <- standard_deviation
sigma.2 <- var


skewness <- function(X){
    mean(((X - mu(X)) / sigma(X))^3)
}

kurtosis <- function(X){
    mean(((X - mu(X)) / sigma(X))^4)
}