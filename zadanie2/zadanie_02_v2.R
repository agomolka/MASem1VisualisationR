library(ggpattern)
library(ggplot2)
library(dplyr, warn.conflicts=FALSE)
library(tidyr)

d <- iris %>% 
  group_by(Species) %>% 
  summarize_all(mean) %>% 
  gather(key, val, -Species)

ggplot(d, aes(x = Species, y = val, fill = key)) +
  geom_col_pattern(position = "dodge",
                   pattern = 
                     c(
                       "stripe", "stripe", "stripe", # 3rd col
                       "stripe", "stripe", "stripe", # 4th col
                       "none", "none", "none", # 1st col
                       "crosshatch", "crosshatch", "crosshatch" # 2nd col
                     ),
                   pattern_angle = c(rep(0, 3), 
                                     rep(45, 3), 
                                     rep(0, 6)),
                   pattern_density = .1,
                   pattern_spacing = .04,
                   pattern_fill = 'black') +
  scale_fill_grey() +
  guides(fill = guide_legend(override.aes = 
                               list(
                                 pattern = c("none", "crosshatch", "stripe", "stripe"),
                                 pattern_spacing = .01,
                                 pattern_angle = c(0, 0, 0, 45)
                               )
  ))

#-----------------------
#plot 2
install.packages("cowplot")

library(ggplot2)
library(cowplot)

#---
ggplot(mtcars, aes(mpg, fill = as.character(gear))) + 
  guides(fill = guide_legend(title = "Number of gears")) +
  scale_fill_brewer(palette="Accent") +
  geom_density(alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_hgrid(12)
