#! /usr/bin/env R
##
##
## Author: Caio Borges Aguida Geraldes
## These files comprehend the annotated data used in my MS dissertation
## *Atração de Caso em Orações Infinitivas do Grego Antigo: estudo de caso em
## Heródoto, Platão e Xenofonte* and some usefull scripts I have employed for
## analysing it.
##
##

                                        # Loading dependencies
require(tidyverse)
require(vcd)
require(FactoMineR)
require(ca)

                                        # Reading database
data <- read_csv('./data.csv')

                                        # Produce basic statistics

                                        # General distribution of Attraction
tab.attraction <- table(data$attraction); addmargins(tab.attraction)
prop.table(tab.attraction)

                                        # Stats
chisq.test(tab.attraction)

                                        # Plot
p <- ggplot(data, aes(attraction, fill=attraction)) +
  geom_bar() +
  scale_fill_brewer(palette="Dark2")
p


                                        # Attraction and Distance
                                        # Distance and the normal distribution
p <- ggplot(data, aes(distance_xy)) +
  geom_bar()
p

shapiro.test(data$distance_xy)

wilcox.test(data$distance_xy ~ data$attraction)

p <- ggplot(data, aes(attraction, distance_xy)) +
  geom_boxplot()
p
