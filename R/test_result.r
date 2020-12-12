
#' @export
TestResult <- function(test_name, H0_description, test_statistic, distribution, ...){
    crit_values <- critical_values(distribution=distribution, ...)
    
    test_result <- structure(list("test" = test_name,
                                  "H0"= H0_description,
                                  "test_statistic" = test_statistic,
                                  "critical_values" = crit_values),
                            class="TestResult")
    return(test_result)
}

#' @export
print.TestResult <- function(test_result){
    cat(paste0("\033[1m", test_result$test, "\033[0m\n")) # Bold font for test name
    cat("Null Hypothesis (H0): ", test_result$H0, "\n\n")
    cat("Test Statistic = ", test_result$test_statistic, "\n\n")
    cat(capture.output(test_result$critical_values), sep="\n")
}