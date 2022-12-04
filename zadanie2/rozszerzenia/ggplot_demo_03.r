rm(list=ls())

library(ggplot2)
library(gridExtra)


# https://briatte.github.io/
# https://briatte.github.io/ggnet/

library(GGally)
# devtools::install_github("briatte/ggnet")
# library(ggnet)

library(network)
library(sna)

set.seed(20171211)
net <- rgraph(10, mode = "graph", tprob = 0.5)
net <- network(net, directed = FALSE)

wyk <- ggnet2(net)
x11();print(wyk)

# wielkosc nodów 
wyk <- ggnet2(net, node.size = 6, node.color = "black", edge.size = 2, edge.color = "grey")
x11();print(wyk)

# kolor nodów 
wyk <- ggnet2(net, size = 6, color = rep(c("tomato", "steelblue"), 5))
x11();print(wyk)

# rozmieszczenie wezłow
wyk <- ggnet2(net, mode = "circle")
x11();print(wyk)

# kolory krawedzi
net %v% "phono" = ifelse(letters[1:10] %in% c("a", "e", "i"), "vowel", "consonant")
wyk <- ggnet2(net, color = "phono")
x11();print(wyk)

net %v% "color" = ifelse(net %v% "phono" == "vowel", "steelblue", "tomato")
wyk <- ggnet2(net, color = "color")
x11();print(wyk)


# wielkosc nodów 
wyk <- ggnet2(net, size = "phono")
x11();print(wyk)

wyk <- ggnet2(net, size = "degree", size.min = 1)
x11();print(wyk)


# podpisy nodow 
wyk <- ggnet2(net, size = 12, label = TRUE, label.size = 5)
x11();print(wyk)


# ksztalt nodow 
wyk <- ggnet2(net, color = "phono", shape = 15)
x11();print(wyk)


#----------------------------------------------------------------------------
r <- "https://raw.githubusercontent.com/briatte/ggnet/master/"
# krawedzie 
v <- read.csv(paste0(r, "inst/extdata/nodes.tsv"), sep = "\t")
print(names(v))
# nody 
e <- read.csv(paste0(r, "inst/extdata/network.tsv"), sep = "\t")
names(e)

net <- network(e, directed = TRUE)
x <- data.frame(Twitter = network.vertex.names(net))
x <- merge(x, v, by = "Twitter", sort = FALSE)$Groupe
net %v% "party" = as.character(x)

y <- RColorBrewer::brewer.pal(9, "Set1")
names(y) <- levels(x)

wyk <- ggnet2(net, color = "phono", alpha = 0.75, size = 4, edge.alpha = 0.5)
x11();print(wyk)


wyk <- ggnet2(net, color = "phono", alpha = 0.75, size = 4, edge.alpha = 0.5,
       edge.color = c("color", "grey50"), label = c("BrunoLeRoux", "nk_m"), label.size = 4)

x11();print(wyk)

