# pakiet sluzy do wizualizacji danych w przestrzeni
# w kodzie zaprezentowano wykres dla Polski z losowo wybranym punktem na mapie

rm(list=ls())
library(ggmap)

poland <- c(left = 14, bottom = 48, right = 25, top = 55)
map_pl <- get_stamenmap(poland, zoom = 5, maptype = "toner-lite")
x11(); ggmap(map_pl)
x11(); ggmap(map_pl, extent = "device")

wyk <- (ggmap(map_pl, extent = "normal") 
    + 
    geom_point(aes(x = 20, y = 50),size=5, colour="red")
)
x11(); print(wyk)
