rm(list=ls())

library(ggplot2)
library(gridExtra)


# https://briatte.github.io/
# https://briatte.github.io/ggcorr/

library(GGally)

# dane 
wyk <- ggcorr(mtcars)
x11();print(wyk)

# lub macierz korelacji 
wyk <- ggcorr(data = NULL, cor_matrix = cor(mtcars[, -1], use = "everything"))
x11();print(wyk)

# konkretna dafinicja korelacji 
wyk <- ggcorr(mtcars, method = c("pairwise", "pearson"))
x11();print(wyk)

# breaks 
wyk <- ggcorr(mtcars, nbreaks = 5)
x11();print(wyk)

# paleta kolorow 
wyk <- ggcorr(mtcars, nbreaks = 4, palette = "RdGy")
x11();print(wyk)

# geometria punktow 
wyk <- ggcorr(mtcars, geom = "circle", nbreaks = 5)
x11();print(wyk)
wyk <- ggcorr(mtcars, geom = "circle", nbreaks = 5, min_size = 0, max_size = 6)
x11();print(wyk)

# wartosci korelacji 
wyk <- ggcorr(mtcars, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")
x11();print(wyk)

# nazwy zmiennych 
wyk <- ggcorr(mtcars, hjust = 0.75, size = 5, color = "grey50")
x11();print(wyk)

wyk <- ggcorr(mtcars, angle = 45, size = 5, color = "grey50")
x11();print(wyk)



