require(vcd)
require(readr)
require(Hmisc)
require(ggplot2)


assocer <- function (var1, var2) {
    assocs <- assocstats(table(var1, var2))$chisq_tests
    return(paste("X^2 =", round(assocs[2], 3), "(", assocs[4], "), p =", assocs[6]))
}

