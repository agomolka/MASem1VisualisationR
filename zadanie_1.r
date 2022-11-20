rm(list=ls())
options(width=200)

zapotrzebowanie <- read.table(file="data/PL_WYK_KSE.csv",sep=";",dec=",",header=T)
zapotrzebowanie <- zapotrzebowanie[,c(1,3)]
colnames(zapotrzebowanie)[2] <- "Zapotrzebowanie"
zapotrzebowanie <- zapotrzebowanie[as.Date(zapotrzebowanie$Data) < as.Date("2022-11-01"),]
zapotrzebowanie$RokMiesiac <- substr(zapotrzebowanie$Data,1,7)

print(head(zapotrzebowanie))
zapotrzebowanie <- aggregate(Zapotrzebowanie~RokMiesiac,data=zapotrzebowanie,FUN=sum,na.rm=T)
print(head(zapotrzebowanie))

oze <- read.table(file="data/PL_GEN_WIATR.csv",sep=";",dec=",",header=T)
colnames(oze)[3:4] <- c("Wiatr","PV")
oze <- oze[as.Date(oze$Data) >= as.Date("2020-05-01"),]
oze <- oze[as.Date(oze$Data) < as.Date("2022-11-01"),]
oze$PV <- gsub(",",".",oze$PV)
oze$PV <- as.numeric(oze$PV)
oze$RokMiesiac <- substr(oze$Data,1,7)

oze <- aggregate(cbind(Wiatr,PV)~RokMiesiac,data=oze,FUN=sum,na.rm=T)
oze <- merge(oze, zapotrzebowanie, on = RokMiesiac, how="left")
oze$OZE <- oze$Wiatr + oze$PV

oze$wiatr_percent= 100*(oze$Wiatr/oze$Zapotrzebowanie)
oze$wiatr_pv= 100*(oze$PV/oze$Zapotrzebowanie)

#PLOT 1
#------
oze$oze_percent <- 100*(oze$OZE/oze$Zapotrzebowanie)
oze_temp <- oze[order(oze$RokMiesiac),]
oze_temp$id <- 1:nrow(oze_temp)
print(head(oze_temp))
print(tail(oze_temp))
model <- lm(oze_temp$oze_percent~id,data=oze_temp)
oze_temp$oze_percent <- round(oze_temp$oze_percent, digits = 2)
my_list <- c(oze_temp$oze_percent)
print(my_list)

pdf("zad1.pdf", width = 10, height = 5)
plot(x=oze_temp$id, y=oze_temp$oze_percent, 
     type="b", col="red", 
     xlim=c(1,30), ylim=c(0,30),
     axes = FALSE,
     main="Produkcja OZE do zapotrzebowania na moc w Polsce \n w miesiacach 2020.05 − 2022.10",
     xlab="[Miesiąc]", ylab="[%]",
     pch=16)
axis(2)
grid(30, 30)
mtext("Zrodlo danych: PSE Operator \n https://www.pse.pl/dane−systemowe/funkcjonowanie−kse/raporty−dobowe−z−pracy−kse", side = 4, adj = 0,cex = 0.7)
text(x=oze_temp$id, y=oze_temp$oze_percent+2,
     labels=my_list, cex = 0.5)
text(15.5, 26, "Wzrost miesieczny: 0.362 %",col="red");
text(15.5, 24, "Wzrost roczny: 4.342 %",col="red");
abline(a=model$coefficients[1],b=model$coefficients[2],lty=8,lwd=1,col="blue")
dev.off()

#

#PLOT 2

barplot1 <- as.matrix(oze[2:3])
rownames(barplot1) <- oze$RokMiesiac
barplot2 <- as.matrix(oze[6:7])
rownames(barplot2) <- oze$RokMiesiac

pdf("zad2.pdf", width = 20, height = 8)
par(mfrow = c(2, 1))
end_point = 0.5 + nrow(oze) + nrow(oze) - 1
barplot(t(barplot1),
        col = c("blue", "yellow"), 
        beside=TRUE, ylab = "[MMh/miesiąc]",
        legend.text = c("Wiatr", "PV"),
        args.legend = list(x = "topleft"), 
        las=3
        )
mtext(expression(paste(bold("Produkcja OZE do zapotrzebowania na moc w Polsce"))), 3, 1.5)
mtext(expression(paste(bold("w miesiacach 2020.05-2022.10"))), 3, 0.5)

barplot(t(barplot2),beside=T,
        col = c("blue", "yellow"),
        legend.text = c("Wiatr", "PV"),
        args.legend = list(x = "topleft"),
        ylab = "[%]", las=3,
        )
mtext('Zrodlo danych: PSE Operator', 
      side=4, cex=0.5, line =0, adj =0)
mtext('https://www.pse.pl/dane%E2%88%92systemowe/funkcjonowanie%E2%88%92kse/raporty%E2%88%92dobowe%E2%88%92z%E2%88%92pracy%E2%88%92kse', 
      side=4, cex=0.45, line =0.5, adj =0)
dev.off()



