#if(!require (devtools)) install.packages ("devtools")

#library (devtools)
#devtools::install_github("m-freitag/cjpowR")

library(groundhog)

library (cjpowR)
groundhog.library("tidyverse", '2022-05-31')


# regular amce for reference
#cjpowr_amce(amce = 0.12, alpha=0.05, power = 0.8, levels = 5)



cjpowr_amcie(delta0=0.224, delta1=0.126, delta2=0.304, delta3 = 0.043, alpha=0.05, power = c(0.7, 0.8,0.9), levels1 = 2, levels2 = 2)


#delta3 = the amcie - difference between punishment for in-party and outparty,
#levels1 = co-partisan or not, levels2 = democracy-levels



#Interactive Plot

library(plotly)
#groundhog.library("plotly", '2022-05-31')


power <- expand.grid(power=c(0.7, 0.8, 0.9))
d <- plyr::mdply(power, cjpowr_amcie,
                 delta3 = seq(from = 0.035, to = 0.06, length.out = 1000),
                 delta2 = 0.304,
                 delta1 = 0.126,
                 delta0 = 0.244,
                 alpha = 0.05,
                 levels1 = 2,
                 levels2 = 2,
                 sims = NULL)


plot_ly(data=d, x = ~delta3, y = ~n, type = 'scatter', mode = 'lines', linetype = ~power) %>%
  layout(
    xaxis = list(title = "AMCIE", zeroline = F, hoverformat = '.4f'),
    yaxis = list(title = "Minimum Effective Sample Size", zeroline = F, hoverformat = '.2f'),
    legend=list(title=list(text='<b> Power </b>')),
    hovermode = "x unified")

