install.packages("gganimate")
install.packages("ggpattern")
data(mtcars)
head(mtcars)
View(mtcars)

#library(ggplot2)
#install ggplot2
install.packages("ggplot2")

#load ggplot2
library(ggplot2)
#--------
library(ggplot2)
library(gganimate)
library(ggbetweenstats) 
data(iris)
ggbetweenstats(
  data  = iris,
  x     = Species,
  y     = Sepal.Length,
  title = "Distribution of sepal length across Iris species"
)
#----------
ggplot(df, aes(x=x, y=y)) +
  geom_point()
#-----------

if (require("PMCMRplus")) {
  # to get reproducible results from bootstrapping
  set.seed(123)
  library(ggstatsplot)
  
  # simple function call with the defaults
  ggbetweenstats(mtcars, am, mpg)
  
  # more detailed function call
  ggbetweenstats(
    data = morley,
    x = Expt,
    y = Speed,
    type = "robust",
    xlab = "The experiment number",
    ylab = "Speed-of-light measurement",
    pairwise.comparisons = TRUE,
    p.adjust.method = "fdr",
    outlier.tagging = TRUE,
    outlier.label = Run
  )
}

library(magick)
library(ggpattern)
if (require("magick")) {
  
  p <- ggplot(mpg, aes(class)) +
    geom_bar_pattern(
      aes(
        pattern_angle = class
      ), 
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
  
  p
  
}