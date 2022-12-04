rm(list=ls())
library(ggplot2)

d <- read.table(file="data/BZN_PL.csv",sep=";",dec=".",header=T,stringsAsFactors=F)
d <- d[c("Date","Hour","TotalLoad_Actual_MW")]
d <- d[d$TotalLoad_Actual_MW!="N/A",]
d$TotalLoad_Actual_MW <- as.numeric(d$TotalLoad_Actual_MW)
d <- na.omit(d)

d$Hour <- substr(d$Hour,1,5)
d$Timestamp <- as.POSIXct(paste0(d$Date," ",d$Hour,":00"),tz="GMT")
d <- d[c("Timestamp","TotalLoad_Actual_MW")] 
d$Wday <- as.POSIXlt(d$Timestamp)$wday 
d$Hour <- as.POSIXlt(d$Timestamp)$hour 
d$Hour <- ifelse(nchar(d$Hour)==1,paste0("0",d$Hour),d$Hour)

d$WdayHour <- paste0(d$Wday,"_",d$Hour)
d$WdayHour <- factor(d$WdayHour,levels=sort(unique(d$WdayHour)))
print(head(d))

d$Year <- as.POSIXlt(d$Timestamp)$year + 1900
d$Month <- as.character(as.POSIXlt(d$Timestamp)$mon + 1 )
    
img <- (
    ggplot(d,aes(x=Timestamp,y=TotalLoad_Actual_MW, col=Month))
    + 
    geom_line()
    + 
    facet_wrap(~Year, scales="free_x")
)
x11();print(img)


wyk <- (
  ggplot(d, aes(x=WdayHour, y=TotalLoad_Actual_MW, fill=factor(Wday)))
  +
  geom_boxplot()
  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5))
)
x11();print(wyk)


