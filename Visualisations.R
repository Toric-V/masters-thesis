setwd("G:/Mit drev/Universitet/Speciale")

library(groundhog)
groundhog.library("ggplot2", '2022-05-31')
groundhog.library("tidyverse", '2022-05-31')
groundhog.library("dplyr", '2022-05-31')


fun1 <- function(x) 2*x^2 + 0.5*x + 0

fun2 <- function(x) -2*x^2 - 0.5*x + 50

base <- ggplot() + xlim(-6.24, 6)


p <- base + geom_function(fun = fun1, color="cornflower blue", size=1.5) + geom_function(fun = fun2, color="orange", size=1.5) + theme_classic() + ylab("Accepted Degree of anti-democratic behaviour") 

p + xlab("More Democratic \u2194 More Republican") + annotate(geom="text", x=-4.5, y=65, label="Expressive Partisanship", color="cornflower blue") + 
     annotate(geom="text", x=-4.5, y=-6, label="Rational Choice", color="orange") + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) 
                                                                                          