###################################################
#Environmental Rebound in 17th Century New England#
###################################################


#Import Data----
BIfaun <- read.csv("BlockIsland.csv")
Mainfaun <- read.csv("GreenwichMonhantic.csv")
srates <- read.csv("ShellfishReturnRates.csv")
trates <- read.csv("TerrestrialReturnRates.csv")


#Figures 1 & 2: Return Rates----

jpeg("ShellfishReturnRates.jpg", height=5, width=6, units="in", res=300)

par(mar=c(6, 5.1, 4.1, 2.1))

boxplot(srates, ylab="kcal/hr", xaxt="n")

slabels <- colnames(srates)
text(x=seq_along(slabels)+.1, y=par("usr")[3] - 100, srt=45, adj=1.1, labels=slabels, xpd=TRUE)

dev.off()


jpeg("OtherReturnRates.jpg", height=5, width=6, units="in", res=300)

par(mar=c(6, 5.1, 4.1, 2.1))

boxplot(trates, ylab="kcal/hr", xaxt="n")

tlabels <- colnames(trates)
text(x=seq_along(tlabels)+.1, y=par("usr")[3] - 100, srt=45, adj=1.2, labels=tlabels, xpd=TRUE)

dev.off()


#Abundance Indices----

BIClamOyster <- (BIfaun$Quahog+BIfaun$SoftClam)/(BIfaun$Quahog+BIfaun$SoftClam+BIfaun$Oyster)
MLClamOyster <- (Mainfaun$Quahog+Mainfaun$SoftClam)/(Mainfaun$Quahog+Mainfaun$SoftClam+Mainfaun$Oyster)

MLPropDeer <- Mainfaun$Deer/Mainfaun$SubClass


#Bar Plots----

#Figure 3: Clam Index (Block Island and Mainland)----
jpeg("ClamIndex_BIML.jpg", height=4, width=5.5, units="in", res=300)
periods <- c("Late Woodland", "17th Century")
ClamOyster <- cbind(BIClamOyster, MLClamOyster)
barplot(ClamOyster, col=c("grey90","grey50"), beside=TRUE, legend=periods, ylim=c(0,1.1), ylab="Clam Index", axisnames=F, args.legend=list(x="topright", cex=.75, inset=c(-0.05, -0)))
BILWci.ClamOyst <- index.ci(A=(BIfaun$Quahog[1]+BIfaun$SoftClam[1]), B=BIfaun$Oyster[1], nsim=500, propsamp=.8)
points(c(1.5, 1.5), c(BILWci.ClamOyst[1], BILWci.ClamOyst[2]), pch="-", cex=2)
segments(1.5, BILWci.ClamOyst[1], 1.5, BILWci.ClamOyst[2], lwd=2)
BI17ci.ClamOyst <- index.ci((BIfaun$Quahog[2]+BIfaun$SoftClam[2]), BIfaun$Oyster[2], 500, .8)
points(c(2.5, 2.5), c(BI17ci.ClamOyst[1], BI17ci.ClamOyst[2]), pch="-", cex=2)
segments(2.5, BI17ci.ClamOyst[1], 2.5, BI17ci.ClamOyst[2], lwd=2)
BI_ClOy.LW <- toString(sum(BIfaun[1,]$Quahog, BIfaun[1,]$SoftClam, BIfaun[1,]$Oyster))
BI_ClOy.17 <- toString(sum(BIfaun[2,]$Quahog, BIfaun[2,]$SoftClam, BIfaun[2,]$Oyster))
ML_ClOy.LW <- toString(format(sum(Mainfaun[1,]$Quahog, Mainfaun[1,]$SoftClam, Mainfaun[1,]$Oyster), nsmall=1))
ML_ClOy.17 <-toString(format(sum(Mainfaun[2,]$Quahog, Mainfaun[2,]$SoftClam, Mainfaun[2,]$Oyster),nsmall=1))
mtext(side=1, cex=.9, at=c(1.4, 1.7, 2.4, 2.7, 4.43, 4.87, 5.43, 5.9), text=c(paste(c("n=",BI_ClOy.LW)), paste(c("n=",BI_ClOy.17)), paste(c(ML_ClOy.LW, "g")), paste(c(ML_ClOy.17, "g"))))
mtext(side=1, at=c(2,5), line=1.5, text=c("Block Island", "Mainland"))
dev.off()


#Figure 4: Deer Index (Mainland)----
jpeg("DeerIndex_ML.jpg", height=4, width=3.5, units="in", res=300)
periods <- c("Late Woodland", "17th Century")
barplot(MLPropDeer, col=c("grey90","grey50"), beside=TRUE, legend=periods, ylim=c(0,.8), ylab="Deer Index", axisnames=F, args.legend=list(x="topright", cex=.75, inset=c(-0.1, -0)))
LWci.Deer <- index.ci(A=Mainfaun$Deer[1], B=(Mainfaun$SubClass[1]-Mainfaun$Deer[1]), nsim=500, propsamp=.8)
points(c(.7, .7), c(LWci.Deer[1], LWci.Deer[2]), pch="-", cex=2)
segments(.7, LWci.Deer[1], .7, LWci.Deer[2], lwd=2)
Sevci.Deer <- index.ci(Mainfaun$Deer[2], (Mainfaun$SubClass[2]-Mainfaun$Deer[2]), 500, .8)
points(c(1.9, 1.9), c(Sevci.Deer[1], Sevci.Deer[2]), pch="-", cex=2)
segments(1.9, Sevci.Deer[1], 1.9, Sevci.Deer[2], lwd=2)
ML_Deer.LW <- toString(sum(Mainfaun[1,]$Deer, Mainfaun[1,]$SubClass))
ML_Deer.17 <- toString(sum(Mainfaun[2,]$Deer, Mainfaun[2,]$SubClass))
mtext(side=1, at=c(.55, .8, 1.8, 2.05), text=c(paste(c("n=",ML_Deer.LW)), paste(c("n=",ML_Deer.17))))
dev.off()


#Figure 5: Population Reconstructions for New England----
whitepop <- data.frame("Date"=seq(1620,1730,by=10) , "Sutherland"=c(102,4000,13484,22452,32574,51561,67992,86011,91083,115094,170893,217351), "Rossiter"=c(99,2200,17605,26820,36238,45135,60530,81050,104320,126500,152500,208950))
nativepop <- data.frame("Date"=c(1600,1650), "Pop"=c(172650,25650))

nativeglm <- glm(Pop~Date, data=nativepop, family=poisson)
plot(nativeglm)
nd <- data.frame("Date"=seq(1600,1650,by=10))
nativepred <- predict(nativeglm, newdata=nd, type="response")

whitepopavg <- (whitepop$Sutherland+whitepop$Rossiter)/2
empty <- c(0,0)
whitepopavg <- c(empty, whitepopavg)

empty <- c(0,0,0,0,0,0,0,0)
nativepoppred <- c(nativepred, empty)

totalpop <- data.frame("Date"=seq(1600,1730,by=10), "Pop"=c(whitepopavg+nativepoppred))

#Plot population trajectories
jpeg("PopLevels.jpg", height=5, width=7, units="in", res=300)
plot(0, xlim=c(1600,1730), ylim=c(0,175000), ylab="Population", xlab="Year AD")
lines(Sutherland~Date, data=whitepop, lty=2, type="l", pch=4)
lines(Rossiter~Date, data=whitepop, lty=3, type="l", pch=3)
lines(nativepred~nd$Date, lty=4, type="b", pch=4)
lines(Pop~Date, data=totalpop, lty=1, type="b", pch=16)
legend(1698,35000,c("European (Sutherland)", "European (Rossiter)", "Native (Snow)", "Total"), lty=c(2,3,4,1), pch=c(1,1,4,16), cex=.65, bty="n", pt.cex=c(0,0,1,1))
dev.off()