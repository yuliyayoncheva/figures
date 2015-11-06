#Yuliya Yoncheva 11/5/2015  
#Color figures are colorblind safe, print friendly and photocopy safe with colors selected from http://colorbrewer2.org/

setwd("~/Desktop/Figures")
library("lattice")

# Fig 2 ------------------------------------------------------------------

#historgrams of individual values for 2 groups on the same x, y scales
#labels, tickmarks, scales, colors - all manually changed 
#legends

ad <-read.table("Finad.csv", sep=',', header=TRUE)
png(file="Final_2a.png",width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
all<-read.table("Finad.csv", sep=',', header=TRUE)
mode <- matrix(all$MO, nrow=1)
MO_CON_ad_tbss <- subset(mode, select =c(1:65))
MO_ADHD_ad_tbss <- subset(mode, select =c(66:107))
p1 <- hist(MO_CON_ad_tbss, breaks=20)
p2 <- hist(MO_ADHD_ad_tbss, breaks=20)
plot(p2, col=rgb(0.545,0,0,3/4), xlim=c(0,1), ylim=c(0,12),  yaxt="n", xlab="Mode of anisotropy", cex.lab=1.25, main=" ")
plot(p1, col=rgb(0,0,0,3/4),  xlim=c(0,1),ylim=c(0,12) ,density=7, angle=45, yaxt="n", xlab="Mode of anisotropy", cex.lab=1.25, main=" ", add=T)
a=c(0, 3, 6, 9, 12)
labels <- format(a,scientific=FALSE)
axis(side=2, cex=1.25, at=c(0, 3, 6, 9, 12), labels)
text(0.6,11, "ADHD", cex=1.25, adj=0,  col=rgb(0.545,0,0))
text(0.6,10, "NTA",  cex=1.25, adj=0, col=rgb(0,0,0))
mtext("A.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig 2B ------------------------------------------------------------------


#kid 
png(file="Final_2b.png",width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
all<-read.table("finalkids.csv", sep=',', header=TRUE)
mode <- matrix(all$MO, nrow=1)
MO_CON_ad_tbss <- subset(mode, select =c(1:80))
MO_ADHD_ad_tbss <- subset(mode, select =c(81:162))
p1 <- hist(MO_CON_ad_tbss, breaks=20)
p2 <- hist(MO_ADHD_ad_tbss, breaks=20)
plot(p2, col=rgb(0.545,0,0,3/4), xlim=c(0,1), ylim=c(0,20),  yaxt="n", xlab="Mode of anisotropy", cex.lab=1.25, main=" ")
plot(p1, col=rgb(0,0,0,3/4),  xlim=c(0,1),ylim=c(0,20) ,density=7, angle=45, yaxt="n", xlab="Mode of anisotropy", cex.lab=1.25, main=" ", add=T)
a=c(0, 5, 10, 15, 20)
labels <- format(a,scientific=FALSE)
axis(side=2, cex=1.25, at=c(0, 5, 10, 15, 20), labels)
text(0.6,18.3333, "ADHD", cex=1.25, adj=0,  col=rgb(0.545,0,0))
text(0.6,16.6667, "NTC",  cex=1.25, adj=0, col=rgb(0,0,0))
mtext("B.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig S2 ------------------------------------------------------------------
#scatterplot, 2 groups, separate regression line for each group 
#adults
ac <-rep(1,63)
aa <-rep(19,42)
ac1 <-rep(1,2)
shpad <-c(ac,aa,ac1)

all<-read.table("Finadm.csv", sep=',', header=TRUE)
png(file="Final_S2a.png",width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
plot(all$mot,all$MO, col=as.character(all$color),pch=shpad, xaxt="n", xlab="Motion (mm)", ylab="Mode of anisotropy",cex.lab=1.25, ylim=c(0.3,0.8), xlim=c(1,4.5))
text(1,0.8, "ADHD", adj=0,  col="darkred")
text(1,0.775, "NTA",  adj=0)
axis(side=1, at=c(1, 2, 3, 4), cex.lab=1.25, labels=c(1, 2, 3, 4))
mod <-lm(all$MO ~ all$adhd1/all$mot + 0)
cof <- coef(mod)
abline(a=cof[1], b=cof[3])
abline(a=cof[2], b=cof[4], col='darkred')
mtext("A.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig S2B -----------------------------------------------------------------


#analogous figure but for kids
kc <-rep(1,80)
ka <-rep(19,82)
shpkid <-c(kc,ka)

all<-read.table("finalkids.csv", sep=',', header=TRUE)
png(file="Final_S2b.png",width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
plot(all$mot,all$MO, col=as.character(all$color), pch=shpkid,xaxt="n", xlab="Motion (mm)", ylab="Mode of anisotropy", cex.lab=1.25, ylim=c(0.3,0.8), xlim=c(1,3))
text(1,0.8, "ADHD", adj=0,  col="darkred")
text(1,0.775, "NTC",  adj=0)
axis(side=1, at=c(1, 2, 3), cex.lab=1.25, labels=c(1, 2, 3))
mod <-lm(all$MO ~ all$adhd1/all$mot + 0)
cof <- coef(mod)
abline(a=cof[1], b=cof[3])
abline(a=cof[2], b=cof[4], col='darkred')
mtext("B.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig S3 ------------------------------------------------------------------

c <-rep(1,145)
a <-rep(19,124)
shpt <-c(c,a)

all <-read.table("allsubs.csv", sep=',', header=TRUE)
png(file="Final_S3a_tbss.png",width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
plot(all$age,all$MO, col=as.character(all$color), pch=shpt, xlab="Age (years)", ylab="MA: primary analysis using common templates", ylim=c(0.2,0.8), cex.lab=1.25, xlim=c(4, 55))
text(4,0.8, "ADHD", adj=0, col="darkred")
text(4,0.77, "NT",adj=0)
mod <-lm(all$MO ~ all$color/all$age + 0)
cof <- coef(mod)
abline(a=cof[1], b=cof[3], )
abline(a=cof[2], b=cof[4],col='darkred')
mtext("A.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()


# Fig S3 B ----------------------------------------------------------------


cn <-rep(1,145)
an <-rep(19,123)
shpn <-c(cn,an)
all <-read.table("allsubs.csv", sep=',', header=TRUE)
png(file="Final_S3b_native.png",width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
plot(all$age,all$modti0, col=as.character(all$color), pch=shpn, xlab="Age (years)", ylab="MA: confirmatory analysis in native space", ylim=c(0.2,0.8), cex.lab=1.25, xlim=c(4, 55))
text(4,0.8, "ADHD", adj=0, col="darkred")
text(4,0.77, "NT",adj=0)
mod <-lm(all$modti0 ~ all$color/all$age + 0)
cof <- coef(mod)
abline(a=cof[1], b=cof[3])
abline(a=cof[2], b=cof[4],col='darkred')
mtext("B.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()


# Fig 3 ------------------------------------------------------------------
#receiver operating characteristic curves for 4 measures
#colorblind safe, print friendly and photocopy safe with colors selected from http://colorbrewer2.org/

png(filename="Final_3a.png", width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
moad1 <-read.table("adfa.csv", sep=",", header=TRUE)
moad2 <-read.table("adad.csv", sep=",", header=TRUE)
moad3 <-read.table("admd.csv", sep=",", header=TRUE)
moad4 <-read.table("admo.csv", sep=",", header=TRUE)
plot (adadx ~ adady, type ="l",  xlim=c(0,1), ylim=c(0,1), asp=1, xlab="1- Specificity", ylab= "Sensitivity", cex.lab=1.25,col=rgb(178/255,171/255,210/255,1),  moad2)
par(new=T)
plot (admdy ~ admdx, type ="l",  xlim=c(0,1), ylim=c(0,1),asp=1,  xlab="1- Specificity", ylab= "Sensitivity", cex.lab=1.25, col=rgb(253/255,184/255,99/255,1), moad3)
par(new=T)
plot (admoy ~ admox, type ="l",  xlim=c(0,1), ylim=c(0,1),asp=1,  xlab="1- Specificity", ylab= "Sensitivity", cex.lab=1.25, col=rgb(94/255,60/255,153/255,1), lwd=1.5,  moad4)
par(new=T)
plot (adfax ~ adfay, type ="l",  xlim=c(0,1), ylim=c(0,1), asp=1,  xlab="1- Specificity", ylab= "Sensitivity", cex.lab=1.25,col=rgb(230/255,97/255,1/255,1),lwd=1.5, moad1)
abline(0,1)
text(0.01,0.98, "MA", adj=0,  cex=1.25, col=rgb(94/255,60/255,153/255,1))
text(0.01,0.92, "FA",  adj=0, cex=1.25, col=rgb(230/255,97/255,1/255,1))
text(0.01,0.86, "AD", adj=0, cex=1.25, col=rgb(178/255,171/255,210/255,1))
text(0.01,0.80, "MD", adj=0, cex=1.25, col=rgb(253/255,184/255,99/255,1))
mtext("A.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig 3B ------------------------------------------------------------------


png(filename="Final_3b.png", width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
moKID <-read.table("kidmo.csv", sep=",", header=TRUE)
plot (kidmox ~ kidmoy, type ="l",  xlim=c(0,1), ylim=c(0,1), asp=1,  xlab="1- Specificity", ylab= "Sensitivity",cex.lab=1.25, col=rgb(94/255,60/255,153/255,1),lwd=2, moKID)
abline(0,1)
text(0.01,0.98, "MA", adj=0,  cex=1.25, col=rgb(94/255,60/255,153/255,1))
mtext("B.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig S5 ------------------------------------------------------------------

png(filename="Final_S5a.png", width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
soloAD <-read.table("adsolo.csv", sep=",", header=TRUE)
bothAD <-read.table("adboth.csv", sep=",", header=TRUE)
plot (adsolox ~ adsoloy, type ="l",  xlim=c(0,1), ylim=c(0,1), asp=1,  xlab="1- Specificity", ylab= "Sensitivity",cex.lab=1.25,  soloAD)
abline(0,1)
par(new=T)
plot (adbothx ~ adbothy, type ="l",  xlim=c(0,1), ylim=c(0,1),asp=1,  xlab="1- Specificity", ylab= "Sensitivity", col=rgb(0/255,136/255,55/255,1), lwd=1.5,cex.lab=1.25, bothAD)
text(0.15,0.75, "CAARS", cex=1.25, adj=0)
text(0.15,0.82, "MA+CAARS", col=rgb(0/255,136/255,55/255,1), cex=1.25, adj=0)
mtext("A.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig S5 B -----------------------------------------------------------------

png(filename="Final_S5b.png", width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
soloKID <-read.table("kidsolo.csv", sep=",", header=TRUE)
bothKID <-read.table("kidboth.csv", sep=",", header=TRUE)
plot (kidsolox ~ kidsoloy, type ="l",  xlim=c(0,1), ylim=c(0,1), asp=1,  xlab="1- Specificity", ylab= "Sensitivity", cex.lab=1.25, soloKID)
abline(0,1)
par(new=T)
plot (kidbothx ~ kidbothy, type ="l",  xlim=c(0,1), ylim=c(0,1), asp=1, xlab="1- Specificity", ylab= "Sensitivity", col=rgb(0/255,136/255,55/255,1), lwd=1.5,cex.lab=1.25,  bothKID)
text(0.15,0.75, "CPRS-R:LV", cex=1.25, adj=0)
text(0.15,0.82, "MA+CPRS-R:LV", col=rgb(0/255,136/255,55/255,1), cex=1.25, adj=0)
mtext("B.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig S4 ------------------------------------------------------------------
#scatterplot 2 groups, 1 regression line
acc <-rep(1,60)
aac <-rep(19,37)
shpadcar <-c(acc,aac)
all<-read.table("adcaars.csv", sep=',', header=TRUE)
png(file="Final_S4a.png",width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
plot(all$caars_to,all$MO, col=as.character(all$color), pch=shpadcar, xlab="CAARS", ylab="Mode of anisotropy", cex.lab=1.25, ylim=c(0.33,0.83), xlim=c(25,90))
text(25,0.82, "ADHD", cex=1.25, adj=0,  col="darkred")
text(25,0.79, "NTA", cex=1.25,  adj=0)
#axis(side=1, at=c(1, 2, 3), cex.lab=1.25, labels=c(1, 2, 3))
# mod <-lm(all$MO ~ all$adhd1/all$mot + 0)
# cof <- coef(mod)
# abline(a=cof[1], b=cof[3])
# abline(a=cof[2], b=cof[4], col='darkred')
abline(lm(all$MO~all$caars_to))
mtext("A.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig S4 B ----------------------------------------------------------------

kcc <-rep(1,74)
kac <-rep(19,66)
shpkidcar <-c(kcc,kac)
all<-read.table("kidcaars.csv", sep=',', header=TRUE)
png(file="Final_S4b.png",width=3000,height=3000, res=500)
par(oma=c(0,0,3,0))
par(mar=c(4,2,1,1))
par(pty="s")
plot(all$caars_to,all$MO, col=as.character(all$color), pch=shpkidcar, xlab="CPRS-R:LV", ylab="Mode of anisotropy", cex.lab=1.25, ylim=c(0.33,0.83), xlim=c(25,90))
text(25,0.82, "ADHD", cex=1.25, adj=0,  col="darkred")
text(25,0.79, "NTC", cex=1.25,  adj=0)
#axis(side=1, at=c(1, 2, 3), cex.lab=1.25, labels=c(1, 2, 3))
# mod <-lm(all$MO ~ all$adhd1/all$mot + 0)
# cof <- coef(mod)
# abline(a=cof[1], b=cof[3])
# abline(a=cof[2], b=cof[4], col='darkred')
abline(lm(all$MO~all$caars_to))
mtext("B.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()

# Fig 1-------------------------------------------------------------------
#Boxplots, horizontal, manual scaling and tickmarks; 
#outliers based 2.2 x IQR above 75th percentile, below 25th percentile
png(file="Final_1.png",width=7480,height=4000, res=1000)
par(mfrow=c(2,5)) 
par(mar=c(4,3,3,1)+0.1)
boxplot(FA ~ dx, data=ad,  range=2.2,  ylim=c(0.37, 0.63), xaxt = "n", main ="FA",  horizontal=TRUE)
axis(side=1, at=c(0.4, 0.5, 0.6), labels=c(0.4, 0.5, 0.6))
boxplot(AD ~ dx, data=ad, range=2.2,  main ="AD", xaxt = "n", ylim=c(0.9, 1.7), horizontal=TRUE)
axis(side=1, at=c(1.0, 1.3, 1.6), labels=c(1.0, 1.3, 1.6))
boxplot(RD ~ dx, data=ad, range=2.2, main ="RD", ylim=c(0.45, 0.72), xaxt = "n",horizontal=TRUE)
axis(side=1, at=c(0.5, 0.6, 0.7), labels=c(0.5, 0.6, 0.7))
boxplot(MD ~ dx, data=ad, range=2.2, ylim=c(0.67, 0.92), xaxt = "n", main ="MD",horizontal=TRUE)
axis(side=1, at=c(0.7, 0.8, 0.9), labels=c(0.7, 0.8, 0.9))
boxplot(MO ~ dx, data=ad, range=2.2, main ="MA",xaxt = "n",ylim=c(0.3, 0.87), horizontal=TRUE)
axis(side=1, at=c(0.4, 0.6, 0.8), labels=c(0.4, 0.6, 0.8))
boxplot(FA ~ dx, data=kid, range=2.2, ylim=c(0.37, 0.63), xaxt = "n",  main ="FA", horizontal=TRUE)
axis(side=1, at=c(0.4, 0.5, 0.6), labels=c(0.4, 0.5, 0.6))
boxplot(AD ~ dx, data=kid, range=2.2,  ylim=c(0.9, 1.7), xaxt = "n", main ="AD", horizontal=TRUE)
axis(side=1, at=c(1.0, 1.3, 1.6), labels=c(1.0, 1.3, 1.6))
boxplot(RD ~ dx, data=kid, range=2.2, ylim=c(0.45, 0.72),xaxt = "n", main ="RD", horizontal=TRUE)
axis(side=1, at=c(0.5, 0.6, 0.7), labels=c(0.5, 0.6, 0.7))
boxplot(MD ~ dx, data=kid, range=2.2, ylim=c(0.67, 0.92),xaxt = "n", main ="MD", horizontal=TRUE)
axis(side=1, at=c(0.7, 0.8, 0.9), labels=c(0.7, 0.8, 0.9))
boxplot(MO ~ dx, data=kid, range=2.2,  main ="MA", xaxt = "n",ylim=c(0.3, 0.87),horizontal=TRUE)
axis(side=1, at=c(0.4, 0.6, 0.8), labels=c(0.4, 0.6, 0.8))
mtext("A.", line=1, side=3, adj=0.01, cex=2.7, outer=TRUE)
dev.off()



