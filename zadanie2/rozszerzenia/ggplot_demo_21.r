rm(list=ls())

#install.packages("ggforce")
#install.packages("deldir")
library(ggforce)
#library(deldir)

data(iris)
Wykres1<- 
  ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  facet_zoom(x = Species == "versicolor")

x11()
print(Wykres1)



Wykres2<-ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_mark_ellipse(aes(fill = Species)) +
  geom_point()

x11()
print(Wykres2)


wykres3<-
  ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_delaunay_tile(alpha = 0.3) + 
  geom_delaunay_segment2(aes(colour = Species, group = -1), size = 2,
                         lineend = 'round')

x11()
print(wykres3)


