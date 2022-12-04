rm(list=ls())
library(ggplot2)
library(ggparallel)
library(vcd)

#Arthritis dataset
data(Arthritis)
#str(Arthritis)

#podstawowy wykres
x11()
wyk <- ggparallel(list("Sex", "Treatment"), data=Arthritis)
print(wyk)

#metoda parset, zmiana przezroczystosci oraz ustawienie tekstu poziomo
x11()
wyk <- ggparallel(list("Sex", "Treatment"), data=Arthritis, method="parset", alpha = 0.1, text.angle = 0)
print(wyk)

#metoda hammock, legenda na dole wykresu
x11()
wyk <- (
    ggparallel(list("Sex", "Treatment"), data=Arthritis, 
           method="hammock", ratio= 0.25, alpha = 0.5, 
           label.size = 5)
    + 
    theme(legend.position = "bottom")
    )
print(wyk)

