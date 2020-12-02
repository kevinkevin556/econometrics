# Util

standard_error <- function(X, y, robust){
    if (robust %in% c("White", "white", "HC0")) {
        std_error <- robust_stde0(X, y)
        cov_type <- "heteroskadestic (White)"
    } else if (robust %in% c("Hinkley", "hinkley", "HC1")) {
        std_error <- robust_stde1(X, y)
        cov_type <- "heteroskadestic (Hinkley)"
    } else if (robust %in% c("Horn", "horn", "HC2")) {
        std_error <- robust_stde2(X, y)
        cov_type <- "heteroskadestic (Horn)"
    } else if (robust %in% c("MacKinnon", "mackinnon", "HC3")) {
        std_error <- robust_stde3(X, y)
        cov_type <- "heteroskadestic (MacKinnon)"
    } else {
        std_error <- stde(X, y)
        cov_type <- "homoskadestic"
    }
    return(list("value"=std_error, "cov_type"=cov_type))
}

# Formula

#' @export
homoskedastic_standard_error <- function(X, y){
    diag(homoskedastic_covariance_estimator(X, y))^(1/2)
}

#' @export
white_heteroskedastic_standard_error <- function(X, y){
    diag(white_heteroskedastic_covariance_estimator(X, y))^(1/2)
}

#' @export
hinkley_heteroskedastic_standard_error <- function(X, y){
    diag(hinkley_heteroskedastic_covariance_estimator(X, y))^(1/2)
}

#' @export
horn_heteroskedastic_standard_error <- function(X, y){
    diag(horn_heteroskedastic_covariance_estimator(X, y))^(1/2)
}

#' @export
mackinnon_heteroskedastic_standard_error <- function(X, y){
    diag(mackinnon_heteroskedastic_covariance_estimator(X, y))^(1/2)
}


# Abbreviation

#' @export
stde <- homoskedastic_standard_error

#' @export
robust_stde0 <- white_heteroskedastic_standard_error

#' @export
robust_stde1 <- hinkley_heteroskedastic_standard_error

#' @export
robust_stde2 <- horn_heteroskedastic_standard_error

#' @export
robust_stde3 <- mackinnon_heteroskedastic_standard_error

