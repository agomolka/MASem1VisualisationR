# animacja 
rm(list=ls())

# install.packages("gganimate")
# install.packages("vctrs")
# install.packages("gifski")
# install.packages("png")

library(ggplot2)
library(gganimate)
#theme_set(theme_bw())

anim <- (
  ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) 
  +
  geom_point(aes(colour = Species, group = 1L)) 
  +
  transition_states(Species,transition_length = 2,state_length = 1)
  + 
  labs(colour = "Gatunek") + labs(x = "Szerokosc liscia") 
  + 
  labs(y = "Dlugosc liscia")
  +
  ggtitle('Obecnie prezentowany gatunek: {closest_state}',
          subtitle = 'Klatka {frame} z {nframes} klatek')
)

animate(anim, renderer = gifski_renderer(), nframes = 50)

anim_save("animation.gif")

