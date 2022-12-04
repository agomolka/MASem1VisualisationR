# Biblioteka ggiraph pozwala tworzyc interaktywne wykresy
# Zrodlo: https://davidgohel.github.io/ggiraph/articles/offcran/customizing.html

library(ggplot2)
library(ggiraph)

data <- mtcars
data$carname <- row.names(data)

wykres <- (
  ggplot(data = data) 
  +
  geom_point_interactive(aes(x = hp, y = mpg, color = disp,
                             tooltip = carname, data_id = carname), size=8)
  + 
  theme_minimal() + labs(title = "Interaktywne punkty")
  + 
  scale_colour_gradientn("Disp", colours = c("blue","yellow","red"))
)

# po najechaniu myszka na punkt pojawia sie nazwa samochodu
# a reszta punktow jest bardziej przezroczysta

girafe(ggobj = wykres,
       options = list(opts_tooltip(use_fill = TRUE),
                      opts_hover_inv(css = "opacity:0.3;"),
                      opts_hover(css = "fill:green;")))


