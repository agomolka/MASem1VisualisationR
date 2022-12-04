rm(list=ls())
options(width=200)

# https://ggplot2.tidyverse.org/reference/
library(ggplot2)
library(gridExtra)


wyk <- (
   #ggplot(data=cars,mapping=aes(x=speed,y=dist))
   ggplot(cars,aes(x=speed,y=dist))
   # +
   # geom_point(colour="red",shape=4,size=8)
   # + 
   # geom_line(colour="blue",linetype=2,size=1)
   # + 
   # geom_line(colour="green",size=0.5)
   # +
   # #geom_text(aes(label=1:nrow(cars)))
   # geom_label(aes(label=1:nrow(cars)))
   # +
   # geom_hline(yintercept=c(2:3)*10,col="orange",linetype=3,size=2)
   # +
   # geom_vline(xintercept=15)
   # +
   # xlab("OX")
   # +
   # ylab("OY - podpis")
   # +
   # ggtitle("ggplot2 - !!!")
)

x11();print(wyk)




print(head(mtcars))
wyk <- (
   ggplot(mtcars,aes(
       x=mpg,
       y=disp#,
       #group=factor(gear),
       #col=factor(gear)
       )
   )
   +
   geom_point()
   #+
   #geom_smooth(method="lm")
)
x11();print(wyk)


print(head(diamonds))
wyk <- (
   ggplot(diamonds, aes(price))##, group=cut, fill=cut))
   +
   geom_histogram(bins=55)
   # +
   # facet_wrap(~cut)
)
x11();print(wyk)



wyk <-(
       ggplot(mtcars, aes(x=gear))
       +
       geom_bar(colour="blue",fill="orange")
       +
       coord_flip()
)
x11();print(wyk)


wyk <-(
       ggplot(mtcars)
       +
       geom_bar(
           aes(x=factor(gear),
               group=factor(cyl),fill=as.factor(cyl)
               ),
               # colour="red"#,
               position="dodge") # stack|dodge
      )
x11()
print(wyk)


wyk <- (
       ggplot(mpg, aes(x=class, y=hwy))#, fill=class))
       +
       geom_boxplot(
            # fill = "orange",
            # colour = "green",
            # outlier.colour = "red",
            # outlier.shape = 5
       )
       #+ coord_flip()
       )
x11();print(wyk)

print(table(mtcars$cyl))
wyk <- (
   ggplot(mtcars,
           aes(x = factor(1),
           group = factor(cyl),
           fill=factor(cyl)))
   +
   geom_bar(width = 1)
   #+
   #coord_polar(theta="y")
   #coord_polar(theta="x")
)

x11();print(wyk)

# rozbudowa w roznych kierunkach 
wyk <- ggplot(mpg, aes(displ, hwy))  + geom_point()
x11(); print(wyk)

wyk_1 <- wyk + geom_point(aes(col=class)) + facet_grid(cyl~class) 
wyk_2 <- wyk + geom_smooth()

x11();print(wyk_1)
x11();print(wyk_2)

wyk <- (
       ggplot(diamonds,aes(x=carat,y=price,colour=clarity))
       + geom_point()
       + geom_smooth(method="lm")
       #+ facet_wrap(. ~ clarity, nrow=2, scales="fixed") # free, free_x,free_y
       #+ facet_grid( . ~ clarity )
       + facet_grid( clarity ~ .)
)
x11();print(wyk)


#-----
wykres1   <- ggplot(data=cars,aes(x=speed,y=dist)) + geom_point()
wykres2   <- ggplot(data=cars,aes(x=speed,y=dist)) + geom_line()
wykres3   <- ggplot(data=cars,aes(x=speed,y=dist)) + geom_line(col="blue")

# library(gridExtra)
x11();grid.arrange(
   arrangeGrob(wykres1,wykres2,wykres3,ncol=1)
)


lay <- rbind(
       c(1,1),
       c(2,4),
       c(3,5)
      )
x11()
wykresGlowny <- grid.arrange(
   arrangeGrob(grobs=list(
       wykres1, 
       wykres1,
       wykres2, 
       wykres2,
       wykres3
   ),
   layout_matrix = lay)
   )

# library(patchwork)


set.seed(1)
tmpSeq <- seq(from=as.Date("2000-01-01"), to=as.Date(Sys.time()), by="1 day")
data <- data.frame(
    Timestamp = tmpSeq,
    Notowanie = cumsum(rnorm(length(tmpSeq))),
    stringsAsFactors=F
)

# data$Timestamp <- as.character(data$Timestamp)
# data$Timestamp <- as.POSIXct(data$Timestamp)
# data$Timestamp <- as.Date(data$Timestamp)


wyk <- ggplot(data,aes(x=Timestamp,y=Notowanie)) + geom_line()
x11();print(wyk)














