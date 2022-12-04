rm(list=ls())
#library(colorout)

library(ggplot2)
library(gridExtra)


# https://briatte.github.io/
# https://briatte.github.io/ggnetwork/
#install.packages("ggnetwork")
# devtools::install_github("briatte/ggnetwork")


library(ggplot2)
library(network)
library(sna)
library(ggnetwork)

set.seed(1)
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
n %v% "family" <- sample(letters[1:3], 10, replace = TRUE)
n %v% "importance" <- sample(1:3, 10, replace = TRUE)
e <- network.edgecount(n)
set.edge.attribute(n, "type", sample(letters[24:26], e, replace = TRUE))
set.edge.attribute(n, "day", sample(1:3, e, replace = TRUE))

#ggnetwork(n, layout = "fruchtermanreingold", cell.jitter = 0.75)
#ggnetwork(n, layout = "target", niter = 100)

wyk <- (
    ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(linetype = type), color = "grey50") +
    theme_blank()
)
x11(); print(wyk)

wyk <- (
    ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(linetype = type), color = "grey50") +
    geom_nodes(aes(color = family, size = importance)) +
    theme_blank()
)
x11(); print(wyk)


# przyklad grafu skierowanego 
data(emon)
wyk <- (
    ggplot(emon[[1]], aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  geom_nodes(color = "tomato", size = 4) +
  theme_blank()
)
x11();print(wyk)

