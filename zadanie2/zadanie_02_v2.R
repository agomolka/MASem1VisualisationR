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
                   pattern_fill = 'yellow') +
  scale_fill_grey() +
  guides(fill = guide_legend(override.aes = 
                               list(
                                 pattern = c("none", "crosshatch", "stripe", "stripe"),
                                 pattern_spacing = .01,
                                 pattern_angle = c(0, 0, 0, 45)
                               )
  ))
#-------------
ggplot(d, aes(x = Species, y = val, fill = key)) +
  geom_col_pattern(
    #aes(
   #   pattern_angle = class
  #  ), 
    pattern         = 'placeholder',
    pattern_type    = 'kitten',
    fill            = 'white', 
    colour          = 'black',
    pattern_spacing = 0.025
  ) +
  theme_bw(18) +
  labs(
    title = "ggpattern::geom_bar_pattern()",
    subtitle = "pattern = 'placeholder', pattern_type = 'kitten'"
  ) + 
  theme(legend.position = 'none') +
  coord_fixed(ratio = 1/15) + 
  scale_pattern_discrete(guide = guide_legend(nrow = 1))
#----------------------------------------------------------------
#-----------------------
#plot 2
install.packages("cowplot")

library(ggplot2)
library(cowplot)

#---
p21 <- ggplot(mtcars, aes(mpg, fill = as.character(gear))) + 
  labs(title = "Density plot for gears", x = "Miles/(US) gallon") +
  guides(fill = guide_legend(title = "Number of forward gears")) +
  scale_fill_brewer(palette="Accent") +
  geom_density(alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_hgrid(12)
#---
p22 <- ggplot(mtcars, aes(qsec, fill = as.character(cyl))) + 
  labs(title = "Density plot for number of cylinders", x = "1/4 mile time") +
  guides(fill = guide_legend(title = "Number of cylinders")) +
  scale_fill_brewer(palette="Accent") +
  geom_density(alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_hgrid(12)
#x11()
plot_grid(
  p21, p22,
  labels = "AUTO", 
  ncol = 1,
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "blue"
)
#-------------
#plot 3
install.packages("afex")
install.packages("ggwithinstats")
install.packages("BayesFactor")
set.seed(123)
library(ggstatsplot)
#library(WRS2) ## for data
#library(afex) ## to run ANOVA
library(BayesFactor)

ggscatterstats(
  data         = mtcars,
  x            = cyl,
  y            = mpg,
  xlab  = "Number of cylinders",
  ylab  = "Miles/(US) gallon [mpg]",
  title = "Number of cylinders per fuel consumption indicator"
)

#plot 1
#-------------------

library(ggtext)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(glue)
mtcars_1 <- mtcars %>% group_by(cyl) %>%tally()

mtcars_1 %>% mutate(
  colors = c("green", "yellow", "pink"),
  name = glue("<i style='color:{colors}'>{cyl}</i>"),
  name = fct_reorder(name, n)
)  %>%
ggplot(aes(cyl, name, fill = colors)) + 
  geom_col(alpha = 0.5) + 
  scale_fill_identity() +
  labs(caption = "Aleksandra Gomolka") +
  theme(
    axis.text.y = element_markdown(),
    plot.caption = element_markdown(lineheight = 1.2)
  ) +
  labs(
    title = "<b>Count number of cylinders </b><br>
    <span style = 'font-size:10pt'>within *mtcars* dataset",
    x = "Number of cylinders",
    y = "Count of cars with specific number of cylinders"
  )+ 
theme(
  plot.title = element_textbox_simple(
    size = 13,
    lineheight = 1,
    padding = margin(5.5, 5.5, 5.5, 5.5),
    margin = margin(0, 0, 5.5, 0),
    fill = "cornsilk"
  )
)