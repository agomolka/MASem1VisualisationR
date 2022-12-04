library(ggridges)
library(ggplot2)

#rozklad sredniej temperatury w poszczegolnych miesiacach w Lincoln NE w 2016 
wyk_1 <- (
  ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = Month, fill = stat(x))) 
    +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) 
    +
    scale_fill_viridis_c(name = "Temp. [F]", option = "C") 
    +
    labs(title = 'Temperatures in Lincoln NE in 2016')
)
x11();print(wyk_1)

#Pokolorowane po kwartylach 
wyk_2 <- (
  ggplot(iris, aes(x=Sepal.Length, y=Species, fill = factor(stat(quantile)))) 
  +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) 
  +
  scale_fill_viridis_d(name = "Quartiles")
)
x11();print(wyk_2)

##rozklad prawdopodobienstwa 
wyk_3 <- (
  ggplot(iris, aes(x = Sepal.Length, y = Species, fill = factor(stat(quantile)))) 
  +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)
  ) 
  +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )
)
x11();print(wyk_3)

wyk_4 <- (
  ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) 
  +
  geom_density_ridges(
    aes(point_shape = Species, point_fill = Species, point_size = Petal.Length), 
    alpha = .2, point_alpha = 1, jittered_points = TRUE
  ) 
  +
  scale_point_color_hue(l = 40) + scale_point_size_continuous(range = c(0.5, 4)) 
  +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))
)

x11();print(wyk_4)


wyk_5 <- (
  ggplot(mtcars, aes(x =wt , y = as.factor(cyl))) 
  +
  geom_density_ridges(scale = 4, fill="red", colour="green") 
  +
  scale_y_discrete(expand = c(0.01, 0)) 
  +
  scale_x_continuous(expand = c(0, 0)) 
  +
  coord_cartesian(clip = "off") 
  +
  theme_ridges()
)
x11();print(wyk_5)




