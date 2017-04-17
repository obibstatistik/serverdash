

library(rCharts)
library(plyr)
library(knitr)
library(reshape2)
library(scales)
options(RCHART_WIDTH = 600, RCHART_HEIGHT = 400)
knitr::opts_chunk$set(comment = NA, results = 'asis', tidy = F, message = F)

names(iris) = gsub("\\.", "", names(iris))
p1 = rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
p1$print(include_assets=T)