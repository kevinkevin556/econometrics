# @' export
t_ratio <- function(coef, std_err){
    coef/std_err
}

# @' export
f_stat <- function(coef, std_err){
    t_ratio(coef, std_err)^2
}

critical_values <- function(distribution, ...){
    quantile_func <- get(paste0("q", distribution))

    quantile90 <- quantile_func(p=0.9, ...)
    quantile95 <- quantile_func(p=0.95, ...)
    quantile99 <- quantile_func(p=0.99, ...)
    
    ci_lower_bound90 <- quantile_func(p=0.5*0.1, ...)
    ci_lower_bound95 <- quantile_func(p=0.5*0.05, ...)
    ci_lower_bound99 <- quantile_func(p=0.5*0.01, ...)

    ci_upper_bound90 <- quantile_func(p=1-0.5*0.1, ...)
    ci_upper_bound95 <- quantile_func(p=1-0.5*0.05, ...)
    ci_upper_bound99 <- quantile_func(p=1-0.5*0.01, ...)

    output <- data.frame(
        critical_values = c(quantile90, quantile95, quantile99),
        ci_lower_bound = c(ci_lower_bound90, ci_lower_bound95, ci_lower_bound99),
        ci_upper_bound = c(ci_upper_bound90, ci_upper_bound95, ci_upper_bound99), 
        row.names = c("0.9", "0.95", "0.99")
    )

    return(output)
}

significant_level <- function(f_stat, df){
    crit_values <- critical_values(distribution="f", df1=1, df2=df)

    result <- sapply(f_stat, function(f){
        if      (f > crit_values["0.99", "critical_values"])   '***'
        else if (f > crit_values["0.95", "critical_values"])   '**'
        else if (f > crit_values["0.9", "critical_values"])    '*'
        else                                  ' '
    })
}

