# zarzadzanie tekstem na wykresach 
rm(list=ls())
library(ggrepel)
library(ggplot2)
library(gridExtra)

set.seed(42)
d <- mtcars
d$car <- rownames(d)

p <- (
  ggplot(d, aes(wt, mpg, label = car)) 
  +
  geom_point(color = "red")
)

p1 <- p + geom_text() + labs(title = "geom_text()")
p2 <- p + geom_text_repel() + labs(title = "geom_text_repel()")

x11();gridExtra::grid.arrange(p1, p2, ncol = 2)
