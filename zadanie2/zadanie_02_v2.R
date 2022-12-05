#-------------------------------------------------------------
#Aleksandra Gomolka
#123034

#-------------------------------------------------------------
#plot 1
#ggpattern
library(ggpattern)
library(ggplot2)
library(dplyr, warn.conflicts=FALSE)
library(tidyr)

sub_mtcars <- mtcars[,c("mpg","cyl","qsec", "drat", "wt")]
d <- sub_mtcars %>% group_by(cyl) %>% summarize_all(mean) %>% gather(key, val, -cyl)

ggplot(d, aes(x = cyl, y = val, fill = key)) +
  labs(title = "Number of cylinders per different variables", 
       x = "Number of cylinders", 
       y = "Mean  of varaibles") +
  geom_col_pattern(position = "dodge",
                   pattern = 
                     c(
                       "stripe", "stripe", "stripe", 
                       "none", "none", "none", 
                       "none", "none", "none", 
                       "crosshatch", "crosshatch", "crosshatch" 
                     ),
                   pattern_density = .15,
                   pattern_spacing = .05,
                   pattern_key_scale_factor = 1.2,
                   pattern_fill = 'black'
                   ) +
  guides(fill = guide_legend(override.aes = 
                               list(
                                 pattern = c("none", "stripe", "none", "crosshatch"),
                                 pattern_spacing = .01,
                                 pattern_angle = c(0, 35, 0, 45)
                               ),
                             title = "Variables:"
  ))

#-------------------------------------------------------------
#plot 2
#ggtext

library(ggtext)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(glue)
#install.packages("Cairo")

mtcars_1 <- mtcars %>% group_by(cyl) %>% tally()

mtcars_1 %>% mutate(
  colors = c("green", "blue", "pink"),
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