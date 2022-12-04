# wykres skrzypcowy 
rm(list=ls())

library(ggplot2)
library(ggpubr)
liczba <- sort(unique(mtcars$gear))

colors <- heat.colors(length(liczba))

p <- ggviolin(mtcars,x="gear",y="mpg",fill="gear",palette = colors,
              add = "boxplot", add.params = list(fill = "white"))

x11();print(p)


