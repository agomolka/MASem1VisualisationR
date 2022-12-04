library(ggplot2)
library(plotly)

img <- ggplot(cars,aes(x=speed,y=dist)) + geom_line()
x11();print(img)

ggplotly(img)

