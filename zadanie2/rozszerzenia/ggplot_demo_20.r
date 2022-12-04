rm(list=ls())

library(ggplot2)
#install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)

data <- ggplot(mtcars) +
        geom_point(aes(x = hp, y = qsec, colour = factor(cyl))) +
        facet_wrap(~gear) +
        ggtitle("MTCARS")
x11();print(data)

x11()
data + theme_solarized(light = FALSE)

x11()
data + theme_excel() + scale_colour_excel()

x11()
data + scale_colour_colorblind()

data1 <- ggplot(mtcars) +
  geom_point(aes(x = hp, y = qsec, colour = factor(cyl))) +
  ggtitle("MTCARS")
x11();print(data1)

x11()
data1 + theme_tufte(base_size = 14, ticks=FALSE)

x11()
data1 + theme_base()

data2 <- ggplot(diamonds, aes(x=color, fill = cut)) +
  geom_bar()
x11();print(data2)

x11()
data2 + scale_fill_excel_new(theme = "Parallax")

