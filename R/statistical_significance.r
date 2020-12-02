# @' export
t_ratio <- function(coef, std_err){
    coef/std_err
}

# @' export
f_stat <- function(coef, std_err){
    t_ratio(coef, std_err)^2
}


critical_values <- function(df){
    quantile90 <- qf(0.9, 1, df)
    quantile95 <- qf(0.95, 1, df)
    quantile99 <- qf(0.9, 1, df)
    return(list("0.9"=quantile90, "0.95"=quantile95, "0.99"=quantile99))
}

significant_level <- function(f_stat, df){
    crit_values <- critical_values(df)

    result <- sapply(f_stat, function(f){
        if      (f > crit_values[["0.99"]])   '***'
        else if (f > crit_values[["0.95"]])   '**'
        else if (f > crit_values[["0.9"]])    '*'
        else                                  ' '
    })

}

