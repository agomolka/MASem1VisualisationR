# https://rpubs.com/cardiomoon/398623

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("cardiomoon/moonBook")
#devtools::install_github("cardiomoon/webr")

library(ggplot2)
library(moonBook)
library(webr)


PieDonut(acs,aes(pies=Dx,donuts=smoking))

PieDonut(acs,aes(Dx,smoking),explode=1)