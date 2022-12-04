rm(list=ls())
#library(colorout)

library(ggplot2)
library(gridExtra)

# https://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html

#library(ggplot2)
library(ggdendro)

# segmentacja 
hc <- hclust(dist(USArrests), "ave")
wyk <- ggdendrogram(hc, rotate = FALSE, size = 2)
x11();print(wyk)


# drzewa
library(rpart)
model <- rpart(Kyphosis ~ Age + Number + Start, 
               method = "class", data = kyphosis)


ddata <- dendro_data(model)
wyk <- ggplot() + 
  geom_segment(data = ddata$segments, 
               aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = ddata$labels, 
            aes(x = x, y = y, label = label), size = 3, vjust = 0) +
  geom_text(data = ddata$leaf_labels, 
            aes(x = x, y = y, label = label), size = 3, vjust = 1) +
  theme_dendro()
x11();print(wyk)


ddata <- dendro_data(model)
wyk <- (
    ggplot()
    + labs(x="",y="")
    + geom_segment(data = ddata$segments, aes(x = x, y = y, xend = xend, yend = yend),colour="green")
    + geom_text(data = ddata$labels, aes(x = x, y = y, label = label), size = 3.2, vjust = 0,colour="red")
    + geom_text(data = ddata$leaf_labels, aes(x = x, y = y, label = label), size = 3, vjust = 1,col="blue")
    + theme(axis.text.x=element_blank(),axis.text.y=element_blank())
)
x11();print(wyk)

# warto wiedziec o mozliwościach tej biblioteki
# library(rattle)
# x11()
# fancyRpartPlot(model)



