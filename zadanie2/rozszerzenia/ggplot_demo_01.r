rm(list=ls())
options(width=200)

library(ggplot2)
library(gridExtra)

library(ggcorrplot)

corr <- round( cor(mtcars), 1)

head(corr[, 1:6])
wyk <- ggcorrplot(corr)
x11();print(wyk)

wyk <- ggcorrplot(corr, method = "circle")
x11();print(wyk)

wyk <- ggcorrplot(corr, hc.order =!TRUE, type = "lower",outline.col = "white")
x11();print(wyk)

wyk <- ggcorrplot(corr, hc.order = TRUE, type = "upper",outline.col = "white")
x11();print(wyk)

wyk <- ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE)
x11();print(wyk)


wyk <- ggcorrplot(corr, hc.order = TRUE, type = "lower",
  outline.col = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("blue", "white", "red"))

x11();print(wyk)





