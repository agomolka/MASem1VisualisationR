# https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk
# https://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do
# https://appsso.eurostat.ec.europa.eu/nui/setupDownloads.do

options(width=250)
rm(list=ls())

library(ggplot2)
library(data.table)

dataDir    <- file.path(getwd(),"data")

download.file(url="https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mwk_ts.tsv.gz",
              destfile=file.path(dataDir,"demo_r_mwk_ts.tsv.gz"),method="curl")


d <- read.table(file=file.path(dataDir,"demo_r_mwk_ts.tsv.gz"),sep="\t",dec=".",header=T)

x <- as.data.frame(rbindlist(lapply(c("PL","DE"),function(country){

  x <- t(d[grep(country,d[,1]),])
  x <- x[-1,]
  options(warn=-1)
  x <- data.frame(
    week = gsub("X","",rownames(x)), 
    f = as.integer(gsub(" p","",x[,1])),
    m = as.integer(gsub(" p","",x[,2])),
    t = as.integer(gsub(" p","",x[,3])),
    c = country
  )
  options(warn=0)
  rownames(x) <- NULL
  x <- x[order(x$week),]

  return(x)
})))
rownames(x) <- NULL


img <- (
  ggplot(data=x,aes(x=as.integer(factor(week)),y=t))
  + 
  geom_line()
  + 
  facet_wrap(~c,ncol=1,scales="free_y")
  + 
  geom_smooth(se=F)
)

print(img)


