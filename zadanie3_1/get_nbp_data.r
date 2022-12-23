library(reshape)
library(ggplot2)

getNBPData <- function(year=2021){
    
  ret <- data.frame()

  if(year>=2013){

    fileName <- paste0(year,"_NBP_data.csv")
  
    try({
      if(file.exists(fileName)){
        if(as.Date(file.info(fileName)$mtime)==Sys.Date()){
          cat(paste("Reading data from local file\n"))
          ret<-read.table(file=fileName,sep=";",dec=",",header=T,stringsAsFactor=F)
    	  colnames(ret) <- gsub("X","",colnames(ret))
	  return(ret)
	}
      }
    })
  
    cat(paste("Downloading data\n"))
  
    res <- try({
  
      d <- readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"))
      d <- d[-2]
      d <- d[-c((length(d)-3):length(d))]
      tmpColnames <- strsplit(d[1],";",useBytes=T)[[1]]
      tmpColnames <- tmpColnames[-c((length(tmpColnames)-1):length(tmpColnames))]
      d <- do.call("rbind",
        lapply(strsplit(d[-1],";"),
        function(x){
          matrix(as.numeric(gsub(",",".",x[-c((length(x)-1):length(x))])),nrow=1)
        })
      )
      colnames(d) <- gsub("1|0","",tmpColnames)
      d <- as.data.frame(d)
      
      d$data <- as.Date(as.character(d$data),format="%Y%m%d")
      ret <- d
      write.table(ret,file=fileName,sep=";",dec=",",row.names=F)
    
    },silent=T)
  
    if(inherits(res,"try-error")){
      cat(paste("An error occurred while downloading data!!!\n")) 
    }
  

  }

  return(ret)

}
#--------------------------------------
#ret <- getNBPData(2022)
ret <- getNBPData(as.integer(as.POSIXlt(Sys.Date())$year+1900))
  
                                                                                                    
ret <- do.call("rbind",
  lapply(grep("^20",dir(),value=T),function(f){
    d <- read.table(file=f,sep=";",dec=",",header=T,stringsAsFactors=F)
    d <- d[c("data","USD","EUR")];
    d
    })); 
ret <- ret[order(ret$data),]; 
write.table(ret,file="nbp.csv",sep=";",dec=".",row.names=F)

ret$data <- as.Date(ret$data)
ret <- reshape::melt(ret,id.vars="data")

img_0 <- (
    ggplot(ret,aes(x=data,y=value,col=variable))
    + 
    geom_line()
    )

img_1 <- (
    img_0    
    + 
    ylim(c(0,max(ret$value,na.rm=T)))
)

pdf(file="nbp.pdf",width=12,height=12)
print(img_0)
print(img_1)
ret <- ret[as.Date(ret$data)>=(Sys.Date()-2*28),]
img_2 <- (img_0 + ylim(range(ret$value,na.rm=T))) %+% ret
print(img_2)
img_3 <- img_0 + facet_wrap(~variable,scales="free_y")
print(img_3)
img_4 <- img_3 %+% ret 
print(img_4)
dev.off()





