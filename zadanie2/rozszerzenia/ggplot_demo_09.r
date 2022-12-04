# tabela na wykresie 

rm(list=ls())

library(ggplot2)
library(ggpmisc)

my_data <- data.frame(group = letters[1:3],         
                      x = 1:9,
                      y = c(1, 3, 2, 1, 3, 1, 2, 1, 1))

ggp <- (
  ggplot(my_data, aes(x, y, color = group)) 
  +  
  geom_point(size = 3)
)

my_table <- as.data.frame(table(my_data[ , c(1, 3)]))

# dodawanie tabeli 
ggp <- (ggp 
  +                                               
  annotate(geom = "table",
           x = 9,
           y = 3,
           label = list(my_table))
)

print(ggp)
