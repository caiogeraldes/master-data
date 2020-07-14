#! /usr/bin/env R
##
## Author: Caio Borges Aguida Geraldes
## e-mail: caio.geraldes@usp.br
##
## This file comprehend a script to evaluate the annotated data in data.csv.
## It corresponds to the quantitative part of my masters dissertation,
## *Atração de Caso em Orações Infinitivas do Grego Antigo: estudo de caso em
## Heródoto, Platão e Xenofonte* written while in the postgrad program
## of Classics in the University of São Paulo and funded by Fapesp.
##
##

                                        # Loading dependencies
require(tidyverse)
require(vcd)
require(FactoMineR)
require(factoextra)
require(ca)

                                        # Reading database
data <- read_csv('./data.csv')
attach(data)

## General distribution of Attraction

tab.attraction <- table(attraction); addmargins(tab.attraction)
prop.table(tab.attraction)

                                        # Stats
chisq.test(tab.attraction)

## Chi-squared test for given probabilities

## data:  tab.attraction
## X-squared = 17.065, df = 1, p-value = 3.613e-05

                                        # Plot
## DO NOT OVERWRITE p, it wil work as base for the next plots
## Next plots will be called b if needed to be stored.

p <- ggplot(data, aes(attraction)) +
  geom_bar(aes(fill=attraction), width = 0.7) +
  labs(x = "", y = "Frequency count",
       fill = "Attraction") +
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  scale_fill_brewer(palette="Dark2")
b <- p + labs(title = "Frequency count of Case Attraction")
b

ggsave("freq.attr.png", path = "./plots/",
       height = 10, width = 10, units = "cm", dpi = 1200)


## Attraction and Distance
                                        # Distance and the normal distribution
b <- ggplot(data, aes(distance_xy)) +
  geom_histogram(bins = 30, aes(fill=attraction)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Distance between Xobl and Y", y = "Frequency counts",
       fill = "Attraction",
       title = "Histogram of the distances between Xobl and Y")
b

ggsave("hist.dist.png", path = "./plots/",
       height = 10, width = 10, units = "cm", dpi = 1200)

## Clearly the plot shows that the distance between Xobl and Y is
## skewed to the leftmost side, with a steep distance between
## the frequency of counts 0 < x < 3 and x < 0.

                                        # Stats

## The Shapiro-Wilk normality test also shows that with
## p << 0.05 (***).

shapiro.test(distance_xy)

## Shapiro-Wilk normality test

## data:  distance_xy
## W = 0.83295, p-value = 1.533e-10

                                        # Testing the correlation

wilcox.test(distance_xy ~ attraction, conf.int = T)

## Wilcoxon rank sum test with continuity correction

## data:  distance_xy by attraction
## W = 2289.5, p-value = 0.000629
## alternative hypothesis: true location shift is not equal to 0
## 95 percent confidence interval:
##  0.9999225 3.9999760
## sample estimates:
## difference in location
##               2.000053

## The median difference is considerable, generally around 2points.

                                        # Plot
b <- ggplot(data, aes(attraction, distance_xy, fill=attraction)) +
  geom_boxplot(outlier.shape=15, outlier.size=2.4) +
  labs(y="Distance between Xobl and Y",
       title = "Correlation of distance between Xobl and Y, and Attraction") +
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  scale_fill_brewer(palette="Dark2")
b

ggsave("box.dist.attr.png", path = "./plots/",
       height = 10, width = 10, units = "cm", dpi = 1200)

## Copula and Attraction
                                        # Stats

tab.copula <- table(copula, attraction); addmargins(tab.copula)
prop.table(tab.copula, 1)

assocstats(tab.copula)

                                        # Plot

lables <- c("FALSE" = "Copula = False", "TRUE" = "Copula = True")
b <- p + facet_wrap(.~copula, labeller = as_labeller(lables)) +
    labs(title="Case Attraction and Copular Infinitives")
b

ggsave("copula.attr.png", path = "./plots/",
       height = 10, width = 10, units = "cm", dpi = 1200)


## Multivariate models

                                        # MCA
mca <- MCA(data[,c("attraction",
                   "copula",
                   "poss_verb",
                   "personal",
                   "pressuposition",
                   "author")],
           method = "Burt", graph = F)

                                        # Eigen-values

fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 80),
               barfill="#1B9E77", barcolor="#1B9E77", main="Eigenvalues") +
  theme_get()

ggsave("mca.eigenvalues.png", path = "./plots/",
       height = 10, width = 10, units = "cm", dpi = 1200)

                                        # Dim1 and Dim2

mca.plot <- plot(mca, invisible = "ind", graph.type =  "ggplot",
          col.var=c('#1B9E77', '#1B9E77',
                    '#D95F02', '#D95F02',
                    '#7570B3', '#7570B3',
                    '#E7298A', '#E7298A',
                    '#E6AB02', '#E6AB02',
                    '#66A61E', '#66A61E', '#66A61E')) +
  theme_get()
mca.plot

ggsave("mca.dim1dim2.png", path = "./plots/", dpi = 1200)

## Author and Attraction
                                        # Stats

tab.author <- table(author, attraction); addmargins(tab.author)
assocstats(tab.author)

                                        # Plot

b <- p + facet_wrap(.~author)+
  labs(title = "Frequency counts of Attraction divided by author")
b

ggsave("author.attr.png", path = "./plots/",
       height = 10, width = 10, units = "cm", dpi = 1200)


## Possibility Construction
                                        # Stats

tab.poss <- table(poss_verb, attraction)

addmargins(tab.poss)
assocstats(tab.poss)

                                        # Plot

lables <- c("FALSE" = "Possibility Construction = False",
            "TRUE" = "Possibility Construction = True")
b <- p +
  facet_wrap(.~poss_verb, labeller = as_labeller(lables)) +
  labs(title = "Frequency counts of Attraction by Type of Construction")
b

ggsave("poss.attr.png", path = "./plots/", dpi = 1000,
       height = 12, width = 14, units = "cm")


## Pressuposition and Attraction
                                        # Stats

tab.press <- table(pressuposition, attraction)

addmargins(tab.press)
assocstats(tab.press)

                                        # Plot

lables <- c("FALSE" = "Pressuposed = False", "TRUE" = "Pressuposed  = True")
b <- p +
  facet_wrap(.~pressuposition, labeller = as_labeller(lables)) +
  labs(title = "Frequency counts of Attraction by Pressupostion")
b

ggsave("press.attr.png", path = "./plots/", dpi = 1000,
       height = 12, width = 14, units = "cm")
