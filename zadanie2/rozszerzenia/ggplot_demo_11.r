# wykres radarowy 
rm(list=ls())
options(width=200)

library(ggplot2)
# devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
library(ggradar)

mtcars_radar <- tail(mtcars,10)
for(j in 1:ncol(mtcars_radar)){
  mtcars_radar[,j] <- mtcars_radar[,j]/max(mtcars_radar[,j])
}
mtcars_radar <- cbind(group=rownames(mtcars_radar),mtcars_radar)

obraz <- ggradar(mtcars_radar)
x11();print(obraz)


