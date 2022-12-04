# alternatywa dla gridExtra 

rm(list=ls())

library(ggplot2)
library(patchwork)

w1 <- ggplot(iris) +
  geom_point(aes(Sepal.Length, Sepal.Width, colour = Species)) +
  ggtitle("Wykres 1")

w2 <- ggplot(iris) + 
  geom_boxplot(aes(Petal.Length, Sepal.Length, group = Species)) +
  ggtitle("Wykres 2")

w3 <- ggplot(iris) + 
  geom_bar(aes(Petal.Width)) +
  facet_wrap(~Species) +
  ggtitle("Wykres 3")

x11()
w <- w1 + w1 + w3
plot(w)

x11()
w <- (w1 / w2 / w3) + plot_annotation(title = "Charakterystyki irys")
plot(w)



