rm(list=ls())
options(width=200)

library(ggplot2)
library(gridExtra)


##--------------------------------------------------------------
## zamiana danych w wykresie 
##--------------------------------------------------------------
data <- mtcars
print(head(data))

wyk_1 <-( 
   ggplot(data,aes(x=mpg, y=wt, colour=cyl)) #as.character(cyl)))
   + 
   geom_point()
   +
   ggtitle("Ala ma kota")
)
x11()
print(wyk_1)


data_2 <- data
data_2$wt <- (-1)*(data_2$wt - mean(data_2$wt))

wyk_2 <- wyk_1 %+% data_2
x11()
plot(wyk_2)


##--------------------------------------------------------------
## zmiana osi  
##--------------------------------------------------------------
wyk_1 <- (
   ggplot(mpg,aes(x=cty,y=hwy))
   +
   geom_point()
   + 
   ggtitle("III")
)
x11()
print(wyk_1)

wyk_2 <- wyk_1 + aes(x=drv) #+ scale_x_discrete()

x11()
print(wyk_2)


#--------------------------------------------------------------
# Kolory  
#--------------------------------------------------------------

wyk_1 <- (
   ggplot(
       msleep,
       aes(x=sleep_total, y=sleep_cycle, col = vore))
   +
   geom_point()
   )
x11()
print(wyk_1)

wyk_2 <- wyk_1 + scale_colour_hue("Legenda - moja")
x11(); print(wyk_2)

wyk_2 <- wyk_1 + scale_colour_manual(name="Legenda",
                                     values=c("black","red","yellow","orange"))#,na.value="cyan",)
x11(); print(wyk_2)

wyk_2 <- wyk_1 + scale_colour_grey()
#wyk_2 <- wyk_1 + scale_colour_grey("Legenda - moja ")
x11(); print(wyk_2)

# http://ggplot2.tidyverse.org/reference/scale_brewer.html
wyk_2 <- wyk_1 + scale_colour_brewer()
x11(); print(wyk_2)
wyk_2 <- wyk_1 + scale_colour_brewer("Legenda",palette="Set2")
x11(); print(wyk_2)



df <- data.frame(
 x = runif(100),
 y = runif(100),
 z1 = rnorm(100),
 z2 = abs(rnorm(100))
)

wyk_1 <- (
       ggplot(df, aes(x, y))
       +
       geom_point(aes(colour = z2))
       )
x11()
print(wyk_1)

wyk_2 <-( wyk_1 
    + 
    scale_colour_gradientn("Legenda",colours = c("blue","yellow","red"))
)

x11()
print(wyk_2)


#--------------------------------------------------------------
# Theme 
#--------------------------------------------------------------

set.seed(10)
data <- data.frame(
    x = sample(1:20,100,replace=T),
    y = rnorm(100),
    stringsAsFactors=F
)

wyk <- ggplot(data,aes(x,y)) + geom_point()
theme_set(theme_bw())
x11()
print(wyk)

old <- theme_get()
theme_set(theme_classic())
x11(); print(wyk)

theme_set(theme_gray())
x11(); print(wyk)

#theme_grey()      
#theme_bw()        
#theme_dark()      
#theme_gray()      
#theme_light()     
#theme_minimal()   
#theme_void()      
#theme_classic()   
#theme_linedraw()  


x11(); print(wyk + theme_linedraw())
x11(); print(wyk + theme_minimal())
x11(); print(wyk + theme_grey())

theme_set(theme_grey())

wyk <- (
    ggplot(data,aes(x,y)) 
    + 
    geom_point()
    + 
    ggtitle("Dane...")
   )
x11(); print(wyk)


## http://ggplot2.tidyverse.org/reference/theme.html
## https://www.rdocumentation.org/packages/ggplot2/versions/1.0.1/topics/theme
wyk_new <- ( 
   wyk
   +
   theme(plot.title = element_text(family="Times",size=50,colour="blue"))
   + 
   theme(text = element_text(family="Times",size=20,colour="green"))
)

x11(); print(wyk_new)

## https://www.rdocumentation.org/packages/ggplot2/versions/2.1.0/topics/element_rect
x11(); print(wyk + theme(panel.background = element_rect(fill="yellow",colour="red")))

wyk <- (
   ggplot(data,aes(x,y))
   +
   geom_point()
   +
   ggtitle("Dane")
)

x11(); print(wyk)

myTheme <- theme_get()

print(myTheme$text)
myTheme$text$family <- "Times"
myTheme$text$size <- 20
myTheme$panel.background$fill="yellow"
save(myTheme,file="myTheme")
myTheme <- get(load("myTheme"))
theme_set(myTheme)

x11();print(wyk)




#--------------------------------------------------------------
#  
#--------------------------------------------------------------

theme_set(theme_grey())

d <- data.frame(x=c(0,1,1,0,0),y=c(0,0,1,1,0))

img <- (ggplot(d,aes(x=x,y=y)) + geom_polygon(fill="pink",col="red"))
x11()
print(img)



# aes_string
wyk <-(
       ggplot(
           mtcars,
               aes_string( 
                  x=colnames(mtcars)[1],
                  y=colnames(mtcars)[6],
                  colour=colnames(mtcars)[2]
               )
       )
       +
       geom_point()
   )
x11()
print(wyk)
#ggsave(file="wyk.pdf")



