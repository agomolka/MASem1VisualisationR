# -- bedzie zawieral wykres o generacji zrodel wiatrowych i fotowoltaicznych dla danych 
#za ostatnie 7 dni (Sys.Date())!!! 
##Obie generacje powinny zostac zwizualizowane w osobnych ukladach wspolrzednych.
#     Wykres ma znajdowac sie w osobnym rozdziale raportu. 


dates <- lapply(gsub("-","",as.character(seq(from=Sys.Date()-6,to=Sys.Date(),by="1 day"))),function(d){d})

download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/", dates[1][1], sep=""),
              destfile=paste(dates[1][1],".csv",sep=""), method="curl")
d <- read.table(file=paste( dates[1][1], ".csv", sep=""), sep=";",dec=",",header=T,fileEncoding="CP1250")

for(date in dates[c(2:length(dates))]){
  download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/",date[1],sep=""),
                destfile=paste(date[1],".csv",sep=""), method="curl")
  
  #d_temp <- readLines(con=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/",date[1],sep=""))
  d_temp <- read.table(file=paste(date[1], ".csv", sep=""), sep=";",dec=",",header=T,fileEncoding="CP1250")
  d <- rbind(d, setNames(d_temp, names(d_temp)))
}

#View(d)
#d <- read.table(file="data/PL_WYK_KSE.csv",sep=";",dec=",",header=T,stringsAsFactors=F)
#d <- d[,c(1:3)]
#colnames(d) <- c("Data","Godzina","KSE")
print(table(d$Godzina))
#d <- d[-grep("2A",d$Godzina),]

#print(table(d$Godzina))
d$Godzina <- as.integer(d$Godzina)
d$Timestamp <- as.POSIXct(paste0(d$Data," ",d$Godzina-1,":00:00"),tz="GMT")
print(head(d))
print(tail(d))

#d <- d[c("Timestamp","KSE")]
d <- d[order(d$Timestamp),]
print(head(d))
print(tail(d))

plot(x=d$Timestamp,y=d$Generacja.źródeł.wiatrowych,xlab="",ylab="",main="eee")
lines(x=d$Timestamp,y=d$Generacja.źródeł.fotowoltaicznych,col="red")

library(ggplot2)
img <- ( 
  ggplot(d,aes(x=Timestamp,y=Generacja.źródeł.fotowoltaicznych)) 
  + 
    xlab("Czas") 
  +
    geom_line() 
  + 
    geom_point() 
  + 
    geom_smooth()
)
print(img)


#Wskazowka:
x <- head(cars)
print(rbind(x,x,x,x))  

# -- bedzie zawieral tabele z danymi. Tabela powinna miec kolumny: Timestamp;Wiatr;PV; 
#     Tabelka ma zawierac sie w osobnym rozdziale raportu.
