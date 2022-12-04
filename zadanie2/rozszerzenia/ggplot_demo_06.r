rm(list=ls())

library(ggplot2)
library(gridExtra)

library(ggedit)


p <- ggplot(mtcars, aes(x = hp, y = wt)) + geom_point() + geom_smooth()
x11(); print(p)
# edytujemy wykres w przegladarec - lepiej wykonac to z konsoli R, a nie z RStudio
p2 <- ggedit(p)

names(p2) 
x11();print(p2) 

ggsave("wyedytoway.pdf")

