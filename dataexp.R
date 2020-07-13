require(vcd)
require(tidyverse)


check_info <- function (index, data) {
  print(paste(data[index,]$author, 
              "-",
              data[index,]$texto,
              "-",
              data[index,]$local))
  print(data[index,]$passage)
}


atuaplot <- function () {
    print(table(data$pressuposition, data$attraction))
    print(assocstats(table(data$pressuposition, data$attraction)))
    ggplot(data, aes(attraction, fill=pressuposition)) +
        geom_bar()
}

atuaplot2 <- function () {
    print(table(data$pressuposition, data$attraction))
    print(assocstats(table(data$pressuposition, data$attraction)))
    ggplot(data, aes(attraction, fill=pressuposition)) +
        geom_bar() + facet_wrap(~author) 
}
