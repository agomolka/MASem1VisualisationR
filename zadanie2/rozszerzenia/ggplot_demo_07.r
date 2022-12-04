library(ggplot2)
#install.packages("ggimage",dep=T)
library(ggimage) 

p <- (
        ggplot(iris) 
	+ 
	aes(x = Sepal.Length, y = Sepal.Width, color=Species) 
	+ 
        geom_point(size=5) + theme_classic()
    )
x11();print(p)
img <- "my_img_1.jpg"

wyk <- ggbackground(p, img)
x11();print(wyk)


# UWAGA - spodziewane klopoty pod Windowsami
library(cowplot)
library(magick)

wyk <- ggbackground(p, img,
    image_fun = function(x) image_negate(image_convolve(x, 'DoG:0,0,2')))

x11();print(wyk)

img <- "my_img_2.png"
p1 <- ggbackground(p, img) + ggtitle("ggbackground(p, img)")
p2 <- ggbackground(p, img, alpha=.3) + ggtitle("ggbackground(p, img, alpha=.3)")
p3 <- ggbackground(p, img, alpha=.3, color="steelblue") + ggtitle('ggbackground(p, img, alpha=.3, color="steelblue")')
wyk <- cowplot::plot_grid(p1, p2, p3, ncol=3)
x11();print(wyk)


