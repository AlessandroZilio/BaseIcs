library(readr)
Hitters20 <- read_csv("~/Projects/Career/Swings/Hitters20.csv")
Hitters19 <- read_csv("~/Projects/Career/Swings/Hitters19.csv")
Hitters18 <- read_csv("~/Projects/Career/Swings/Hitters18.csv")
Hitters17 <- read_csv("~/Projects/Career/Swings/Hitters17.csv")
library(tidyverse)
library(dplyr)
#Seasons 19-20
#dataset building for 2 seasons with qualified hitters in both
cor1 <- inner_join(Hitters20[-2],Hitters19[-2],by = c("Name","playerid"),suffix = c("20","19"))
library(Hmisc)
#return pure numerical dataset as matrix
yearcorr<-cor1[-c(1,25)]
yearcorr<-as.matrix(yearcorr)
for (i in c(6:18,21,22,29:41,44,45)){
  yearcorr[,i]<- parse_number(yearcorr[,i])/100
}
#compute correlation matrix and plot
onegap<-rcorr(yearcorr)
library(corrplot)
corrplot(onegap$r,type = "upper",p.mat = onegap$P,sig.level = 0.05,insig = "blank",tl.cex = 0.7)
#plot single corr for pair of diff year same var
#WAR
library(ggpubr)
yearcorrdata<-as.data.frame(yearcorr,stringsAsFactors = F)
yearcorrdata<-as.data.frame(sapply(yearcorrdata,as.numeric))
pl1<-ggscatter(yearcorrdata,"WAR19","WAR20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl1,xticks.by = 0.5,yticks.by = 0.5)
#ISO
pl2<-ggscatter(yearcorrdata,"ISO19","ISO20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl2,xticks.by = 0.1,yticks.by = 0.1)
#GB/FB
pl3<-ggscatter(yearcorrdata,"GB/FB19","GB/FB20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl3,xticks.by = 0.25,yticks.by = 0.25)
#wOBA
pl4<-ggscatter(yearcorrdata,"wOBA19","wOBA20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl4,xticks.by = 0.05,yticks.by = 0.05)
#wRC+
pl5<-ggscatter(yearcorrdata,"wRC+19","wRC+20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl5,xticks.by = 25,yticks.by = 25)
#O-Swing%
pl6<-ggscatter(yearcorrdata,"O-Swing%19","O-Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl6,xticks.by = 0.1,yticks.by = 0.1)
#Z-Swing%
pl7<-ggscatter(yearcorrdata,"Z-Swing%19","Z-Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl7,xticks.by = 0.1,yticks.by = 0.1)
#Swing%
pl8<-ggscatter(yearcorrdata,"Swing%19","Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl8,xticks.by = 0.1,yticks.by = 0.1)
#O-Contact%
pl9<-ggscatter(yearcorrdata,"O-Contact%19","O-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl9,xticks.by = 0.1,yticks.by = 0.1)
#Z-Contact%
pl10<-ggscatter(yearcorrdata,"Z-Contact%19","Z-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl10,xticks.by = 0.1,yticks.by = 0.1)
#Contact%
pl11<-ggscatter(yearcorrdata,"Contact%19","Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl11,xticks.by = 0.1,yticks.by = 0.1)
#SwStr%
pl12<-ggscatter(yearcorrdata,"SwStr%19","SwStr%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl12,xticks.by = 0.05,yticks.by = 0.05)
#Pull%
pl13<-ggscatter(yearcorrdata,"Pull%19","Pull%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl13,xticks.by = 0.1,yticks.by = 0.1)
#Cent%
pl14<-ggscatter(yearcorrdata,"Cent%19","Cent%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl14,xticks.by = 0.1,yticks.by = 0.1)
#Oppo%
pl15<-ggscatter(yearcorrdata,"Oppo%19","Oppo%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl15,xticks.by = 0.1,yticks.by = 0.1)
#Soft%
pl16<-ggscatter(yearcorrdata,"Soft%19","Soft%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl16,xticks.by = 0.05,yticks.by = 0.05)
#Med%
pl17<-ggscatter(yearcorrdata,"Med%19","Med%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl17,xticks.by = 0.05,yticks.by = 0.05)
#Hard%
pl18<-ggscatter(yearcorrdata,"Hard%19","Hard%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl18,xticks.by = 0.05,yticks.by = 0.05)
#EV
pl19<-ggscatter(yearcorrdata,"EV19","EV20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl19,xticks.by = 1,yticks.by = 1)
#LA
pl20<-ggscatter(yearcorrdata,"LA19","LA20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl20,xticks.by = 5,yticks.by = 5)
#Barrel%
pl21<-ggscatter(yearcorrdata,"Barrel%19","Barrel%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl21,xticks.by = 0.05,yticks.by = 0.05)
#HardHit%
pl22<-ggscatter(yearcorrdata,"HardHit%19","HardHit%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl22,xticks.by = 0.05,yticks.by = 0.05)
#BONUS
#wRC-WAR
bplot1<-ggscatter(yearcorrdata,"WAR20","wRC+20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot1,xticks.by = 0.5,yticks.by = 25)
#84% of WAR var expressed by wRC+: hitting amounts for +80% of WAR oscillations!
#wOBA-WAR
bplot2<-ggscatter(yearcorrdata,"WAR20","wOBA20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot2,xticks.by = 0.5,yticks.by = 0.05)
#same as wRC+: they are equally good at "explaining" WAR changes
#Contact rates-Barrel%
bplot3<-ggscatter(yearcorrdata,"O-Contact%20","Barrel%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot3,xticks.by = 0.1,yticks.by = 0.05)
bplot4<-ggscatter(yearcorrdata,"Z-Contact%20","Barrel%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot4,xticks.by = 0.1,yticks.by = 0.05)
bplot5<-ggscatter(yearcorrdata,"Contact%20","Barrel%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot5,xticks.by = 0.1,yticks.by = 0.05)
#Contact rates are inversely correlated with barrel rate: selection or power abandon? BUT no corr
#between barrel rate and swing rates: players are swinging for the fences everytime! Same results
#if we look at corrs between contact rates and EV,Hard Cnt and Hard Hit rates.
#Pull rates to contact rate
bplot6<-ggscatter(yearcorrdata,"Pull%20","O-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot6,xticks.by = 0.1,yticks.by = 0.1)
bplot7<-ggscatter(yearcorrdata,"Pull%20","Z-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot7,xticks.by = 0.1,yticks.by = 0.1)
bplot8<-ggscatter(yearcorrdata,"Pull%20","Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot8,xticks.by = 0.1,yticks.by = 0.1)
#general weak neg corr: pulling the ball sacrifices contact in the name of hitting harder?
#Pull to barrel, hard cnt and hit rates
bplot9<-ggscatter(yearcorrdata,"Pull%20","Barrel%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot9,xticks.by = 0.1,yticks.by = 0.05)
bplot10<-ggscatter(yearcorrdata,"Pull%20","Hard%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot10,xticks.by = 0.1,yticks.by = 0.05)
bplot11<-ggscatter(yearcorrdata,"Pull%20","HardHit%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot11,xticks.by = 0.1,yticks.by = 0.05)
#weak positive corrs: not a clear relationship between pull and quality of contact!
#Pull/Oppo rates to BABIP and Oppo to contact rates
bplot12<-ggscatter(yearcorrdata,"Pull%20","BABIP20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot12,xticks.by = 0.1,yticks.by = 0.05)
bplot13<-ggscatter(yearcorrdata,"Oppo%20","BABIP20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot13,xticks.by = 0.1,yticks.by = 0.05)
#going oppo "creates" more luck while pulling takes it away: shifts!?
bplot14<-ggscatter(yearcorrdata,"Oppo%20","O-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot14,xticks.by = 0.1,yticks.by = 0.1)
bplot15<-ggscatter(yearcorrdata,"Oppo%20","Z-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot15,xticks.by = 0.1,yticks.by = 0.1)
bplot16<-ggscatter(yearcorrdata,"Oppo%20","Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot16,xticks.by = 0.1,yticks.by = 0.1)
#general weak pos corrs: going oppo leads to more contact (but look at the cost in terms of ISO!)
#BABIP-WAR
bplot1<-ggscatter(yearcorrdata,"wRC+20","WAR20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot1,xticks.by = 25,yticks.by = 0.5)
bplot2<-ggscatter(yearcorrdata,"wOBA20","WAR20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot2,xticks.by = 0.05,yticks.by = 0.5)
bplot17<-ggscatter(yearcorrdata,"BABIP20","WAR20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(bplot17,xticks.by = 0.05,yticks.by = 0.5)
#positive half corr: luck is a mean of production!
plot23<-ggscatter(yearcorrdata,"BABIP19","BABIP20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(plot23,xticks.by = 0.05,yticks.by = 0.05)
#weak pos corr: luck isn't that solid y2y!

#Seasons 18-20
#dataset building for 2 seasons with qualified hitters in both
cor2 <- inner_join(Hitters20[-2],Hitters18[-2],by = c("Name","playerid"),suffix = c("20","18"))
library(Hmisc)
#return pure numerical dataset as matrix
twoyearcorr<-cor2[-c(1,25)]
twoyearcorr<-as.matrix(twoyearcorr)
for (i in c(6:18,21,22,29:41,44,45)){
  twoyearcorr[,i]<- parse_number(twoyearcorr[,i])/100
}
#compute correlation matrix and plot
twogap<-rcorr(twoyearcorr)
library(corrplot)
corrplot(twogap$r,type = "upper",p.mat = twogap$P,sig.level = 0.05,insig = "blank",tl.cex = 0.7)
#plot single corr for pair of diff year same var
#WAR
library(ggpubr)
twoyearcorrdata<-as.data.frame(twoyearcorr,stringsAsFactors = F)
twoyearcorrdata<-as.data.frame(sapply(twoyearcorrdata,as.numeric))
pl1A<-ggscatter(twoyearcorrdata,"WAR18","WAR20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl1A,xticks.by = 0.5,yticks.by = 0.5)
#ISO
pl2A<-ggscatter(twoyearcorrdata,"ISO18","ISO20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl2A,xticks.by = 0.1,yticks.by = 0.1)
#GB/FB
pl3A<-ggscatter(twoyearcorrdata,"GB/FB18","GB/FB20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl3A,xticks.by = 0.25,yticks.by = 0.25)
#wOBA
pl4A<-ggscatter(twoyearcorrdata,"wOBA18","wOBA20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl4A,xticks.by = 0.05,yticks.by = 0.05)
#wRC+
pl5A<-ggscatter(twoyearcorrdata,"wRC+18","wRC+20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl5A,xticks.by = 25,yticks.by = 25)
#O-Swing%
pl6A<-ggscatter(twoyearcorrdata,"O-Swing%18","O-Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl6A,xticks.by = 0.1,yticks.by = 0.1)
#Z-Swing%
pl7A<-ggscatter(twoyearcorrdata,"Z-Swing%18","Z-Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl7A,xticks.by = 0.1,yticks.by = 0.1)
#Swing%
pl8A<-ggscatter(twoyearcorrdata,"Swing%18","Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl8A,xticks.by = 0.1,yticks.by = 0.1)
#O-Contact%
pl9A<-ggscatter(twoyearcorrdata,"O-Contact%18","O-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl9A,xticks.by = 0.1,yticks.by = 0.1)
#Z-Contact%
pl10A<-ggscatter(twoyearcorrdata,"Z-Contact%18","Z-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl10A,xticks.by = 0.1,yticks.by = 0.1)
#Contact%
pl11A<-ggscatter(twoyearcorrdata,"Contact%18","Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl11A,xticks.by = 0.1,yticks.by = 0.1)
#SwStr%
pl12A<-ggscatter(twoyearcorrdata,"SwStr%18","SwStr%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl12A,xticks.by = 0.05,yticks.by = 0.05)
#Pull%
pl13A<-ggscatter(twoyearcorrdata,"Pull%18","Pull%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl13A,xticks.by = 0.1,yticks.by = 0.1)
#Cent%
pl14A<-ggscatter(twoyearcorrdata,"Cent%18","Cent%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl14A,xticks.by = 0.1,yticks.by = 0.1)
#Oppo%
pl15A<-ggscatter(twoyearcorrdata,"Oppo%18","Oppo%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl15A,xticks.by = 0.1,yticks.by = 0.1)
#Soft%
pl16A<-ggscatter(twoyearcorrdata,"Soft%18","Soft%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl16A,xticks.by = 0.05,yticks.by = 0.05)
#Med%
pl17A<-ggscatter(twoyearcorrdata,"Med%18","Med%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl17A,xticks.by = 0.05,yticks.by = 0.05)
#Hard%
pl18A<-ggscatter(twoyearcorrdata,"Hard%18","Hard%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl18A,xticks.by = 0.05,yticks.by = 0.05)
#EV
pl19A<-ggscatter(twoyearcorrdata,"EV18","EV20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl19A,xticks.by = 1,yticks.by = 1)
#LA
pl20A<-ggscatter(twoyearcorrdata,"LA18","LA20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl20A,xticks.by = 5,yticks.by = 5)
#Barrel%
pl21A<-ggscatter(twoyearcorrdata,"Barrel%18","Barrel%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl21A,xticks.by = 0.05,yticks.by = 0.05)
#HardHit%
pl22A<-ggscatter(twoyearcorrdata,"HardHit%18","HardHit%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl22A,xticks.by = 0.05,yticks.by = 0.05)
#General loss of 0.05-0.1 corr but nothing major! Tendencies and hitting profiles held through a 
#2y gap, with some inconsistencies in the hitting direction (pull,cent,oppo). This consistent loss
#of correlation could be due to less sample size (225 vs 276) as less players qualified from 18 to
#20.Same season shenanigans hold too: direction of relationship is solid as we go back in time,while
#power lowers for all (time series scenario?)

#Seasons 17-20
#dataset building for 2 seasons with qualified hitters in both
cor3 <- inner_join(Hitters20[-2],Hitters17[-2],by = c("Name","playerid"),suffix = c("20","17"))
library(Hmisc)
#return pure numerical dataset as matrix
threeyearcorr<-cor3[-c(1,25)]
threeyearcorr<-as.matrix(threeyearcorr)
for (i in c(6:18,21,22,29:41,44,45)){
  threeyearcorr[,i]<- parse_number(threeyearcorr[,i])/100
}
#compute correlation matrix and plot
threegap<-rcorr(threeyearcorr)
library(corrplot)
corrplot(threegap$r,type = "upper",p.mat = threegap$P,sig.level = 0.05,insig = "blank",tl.cex = 0.7)
#plot single corr for pair of diff year same var
#WAR
library(ggpubr)
threeyearcorrdata<-as.data.frame(threeyearcorr,stringsAsFactors = F)
threeyearcorrdata<-as.data.frame(sapply(threeyearcorrdata,as.numeric))
pl1B<-ggscatter(threeyearcorrdata,"WAR17","WAR20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl1B,xticks.by = 0.5,yticks.by = 0.5)
#ISO
pl2B<-ggscatter(threeyearcorrdata,"ISO17","ISO20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl2B,xticks.by = 0.1,yticks.by = 0.1)
#GB/FB
pl3B<-ggscatter(threeyearcorrdata,"GB/FB17","GB/FB20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl3B,xticks.by = 0.25,yticks.by = 0.25)
#wOBA
pl4B<-ggscatter(threeyearcorrdata,"wOBA17","wOBA20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl4B,xticks.by = 0.05,yticks.by = 0.05)
#wRC+
pl5B<-ggscatter(threeyearcorrdata,"wRC+17","wRC+20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl5B,xticks.by = 25,yticks.by = 25)
#O-Swing%
pl6B<-ggscatter(threeyearcorrdata,"O-Swing%17","O-Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl6B,xticks.by = 0.1,yticks.by = 0.1)
#Z-Swing%
pl7B<-ggscatter(threeyearcorrdata,"Z-Swing%17","Z-Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl7B,xticks.by = 0.1,yticks.by = 0.1)
#Swing%
pl8B<-ggscatter(threeyearcorrdata,"Swing%17","Swing%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl8B,xticks.by = 0.1,yticks.by = 0.1)
#O-Contact%
pl9B<-ggscatter(threeyearcorrdata,"O-Contact%17","O-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl9B,xticks.by = 0.1,yticks.by = 0.1)
#Z-Contact%
pl10B<-ggscatter(threeyearcorrdata,"Z-Contact%17","Z-Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl10B,xticks.by = 0.1,yticks.by = 0.1)
#Contact%
pl11B<-ggscatter(threeyearcorrdata,"Contact%17","Contact%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl11B,xticks.by = 0.1,yticks.by = 0.1)
#SwStr%
pl12B<-ggscatter(threeyearcorrdata,"SwStr%17","SwStr%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl12B,xticks.by = 0.05,yticks.by = 0.05)
#Pull%
pl13B<-ggscatter(threeyearcorrdata,"Pull%17","Pull%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl13B,xticks.by = 0.1,yticks.by = 0.1)
#Cent%
pl14B<-ggscatter(threeyearcorrdata,"Cent%17","Cent%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl14B,xticks.by = 0.1,yticks.by = 0.1)
#Oppo%
pl15B<-ggscatter(threeyearcorrdata,"Oppo%17","Oppo%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl15B,xticks.by = 0.1,yticks.by = 0.1)
#Soft%
pl16B<-ggscatter(threeyearcorrdata,"Soft%17","Soft%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl16B,xticks.by = 0.05,yticks.by = 0.05)
#Med%
pl17B<-ggscatter(threeyearcorrdata,"Med%17","Med%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl17B,xticks.by = 0.05,yticks.by = 0.05)
#Hard%
pl18B<-ggscatter(threeyearcorrdata,"Hard%17","Hard%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl18B,xticks.by = 0.05,yticks.by = 0.05)
#EV
pl19B<-ggscatter(threeyearcorrdata,"EV17","EV20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl19B,xticks.by = 1,yticks.by = 1)
#LA
pl20B<-ggscatter(threeyearcorrdata,"LA17","LA20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl20B,xticks.by = 5,yticks.by = 5)
#Barrel%
pl21B<-ggscatter(threeyearcorrdata,"Barrel%17","Barrel%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl21B,xticks.by = 0.05,yticks.by = 0.05)
#HardHit%
pl22B<-ggscatter(threeyearcorrdata,"HardHit%17","HardHit%20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl22B,xticks.by = 0.05,yticks.by = 0.05)
#there are no big changes in the dimension and direction of correlations wrt 2yg,around +- 0.5, 
#with 0.7 and 0.9 losses of corr for Swing% and WAR as top. Watch out for WAR: loss of corr is
#not due to same for wOBA and/or wRC+ as they remain stable (not hitting, defense?).Intra-year
#relationship hold up, indicating some solid bonus points to acknowledge. Consider also another
#loss of sample size, from original 276 to 186 as possible factor of distorsions.
plot23A<-ggscatter(twoyearcorrdata,"BABIP18","BABIP20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(plot23A,xticks.by = 0.05,yticks.by = 0.05)
plot23B<-ggscatter(threeyearcorrdata,"BABIP17","BABIP20",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(plot23B,xticks.by = 0.05,yticks.by = 0.05)
#BONUS: what if the problem is the 2020 shortened season?
#18-19 Season corrs
#dataset building for 2 seasons with qualified hitters in both
cor4 <- inner_join(Hitters19[-2],Hitters18[-2],by = c("Name","playerid"),suffix = c("19","18"))
library(Hmisc)
#return pure numerical dataset as matrix
fullyearcorr<-cor4[-c(1,25)]
fullyearcorr<-as.matrix(fullyearcorr)
for (i in c(6:18,21,22,29:41,44,45)){
  fullyearcorr[,i]<- parse_number(fullyearcorr[,i])/100
}
#compute correlation matrix and plot
fullgap<-rcorr(fullyearcorr)
library(corrplot)
corrplot(fullgap$r,type = "upper",p.mat = fullgap$P,sig.level = 0.05,insig = "blank",tl.cex = 0.7)
#relationship are similar for underliners, stronger corrs for cumulative stats (WAR.wOBA,wRC+,ISO)
library(ggpubr)
fullyearcorrdata<-as.data.frame(fullyearcorr,stringsAsFactors = F)
fullyearcorrdata<-as.data.frame(sapply(fullyearcorrdata,as.numeric))
#wOBA
pl4F<-ggscatter(fullyearcorrdata,"wOBA18","wOBA19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl4F,xticks.by = 0.05,yticks.by = 0.05)
#wRC+
pl5F<-ggscatter(fullyearcorrdata,"wRC+18","wRC+19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl5F,xticks.by = 25,yticks.by = 25)
#WAR
pl1F<-ggscatter(fullyearcorrdata,"WAR18","WAR19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl1F,xticks.by = 0.5,yticks.by = 0.5)
#ISO
pl2F<-ggscatter(fullyearcorrdata,"ISO18","ISO19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl2F,xticks.by = 0.1,yticks.by = 0.1)
#GB/FB
pl3F<-ggscatter(fullyearcorrdata,"GB/FB18","GB/FB19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl3F,xticks.by = 0.25,yticks.by = 0.25)
#O-Swing%
pl6F<-ggscatter(fullyearcorrdata,"O-Swing%18","O-Swing%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl6F,xticks.by = 0.1,yticks.by = 0.1)
#Z-Swing%
pl7F<-ggscatter(fullyearcorrdata,"Z-Swing%18","Z-Swing%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl7F,xticks.by = 0.1,yticks.by = 0.1)
#Swing%
pl8F<-ggscatter(fullyearcorrdata,"Swing%18","Swing%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl8F,xticks.by = 0.1,yticks.by = 0.1)
#O-Contact%
pl9F<-ggscatter(fullyearcorrdata,"O-Contact%18","O-Contact%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl9F,xticks.by = 0.1,yticks.by = 0.1)
#Z-Contact%
pl10F<-ggscatter(fullyearcorrdata,"Z-Contact%18","Z-Contact%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl10F,xticks.by = 0.1,yticks.by = 0.1)
#Contact%
pl11F<-ggscatter(fullyearcorrdata,"Contact%18","Contact%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl11F,xticks.by = 0.1,yticks.by = 0.1)
#SwStr%
pl12F<-ggscatter(fullyearcorrdata,"SwStr%18","SwStr%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl12F,xticks.by = 0.05,yticks.by = 0.05)
#Pull%
pl13F<-ggscatter(fullyearcorrdata,"Pull%18","Pull%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl13F,xticks.by = 0.1,yticks.by = 0.1)
#Cent%
pl14F<-ggscatter(fullyearcorrdata,"Cent%18","Cent%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl14F,xticks.by = 0.1,yticks.by = 0.1)
#Oppo%
pl15F<-ggscatter(fullyearcorrdata,"Oppo%18","Oppo%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl15F,xticks.by = 0.1,yticks.by = 0.1)
#Soft%
pl16F<-ggscatter(fullyearcorrdata,"Soft%18","Soft%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl16F,xticks.by = 0.05,yticks.by = 0.05)
#Med%
pl17F<-ggscatter(fullyearcorrdata,"Med%18","Med%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl17F,xticks.by = 0.05,yticks.by = 0.05)
#Hard%
pl18F<-ggscatter(fullyearcorrdata,"Hard%18","Hard%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl18F,xticks.by = 0.05,yticks.by = 0.05)
#EV
pl19F<-ggscatter(fullyearcorrdata,"EV18","EV19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl19F,xticks.by = 1,yticks.by = 1)
#LA
pl20F<-ggscatter(fullyearcorrdata,"LA18","LA19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl20F,xticks.by = 5,yticks.by = 5)
#Barrel%
pl21F<-ggscatter(fullyearcorrdata,"Barrel%18","Barrel%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl21F,xticks.by = 0.05,yticks.by = 0.05)
#HardHit%
pl22F<-ggscatter(fullyearcorrdata,"HardHit%18","HardHit%19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(pl22F,xticks.by = 0.05,yticks.by = 0.05)
#BABIP
plot23F<-ggscatter(fullyearcorrdata,"BABIP18","BABIP19",color = "blue",add = "reg.line", conf.int = T,cor.coef = T,cor.method = "pearson")
ggpar(plot23F,xticks.by = 0.05,yticks.by = 0.05)
#WAR logreg on steadiest underliner as per corr
war20i<-yearcorrdata$WAR20
cnt19i<-yearcorrdata$`Contact%19`
hard19i<-yearcorrdata$`Hard%19`
ev19i<-yearcorrdata$EV19
barr19i<-yearcorrdata$`Barrel%19`
hhit19i<-yearcorrdata$`HardHit%19`
wrc19i<-yearcorrdata$`wRC+19`
gbfb19i<-yearcorrdata$`GB/FB19`
osw19i<-yearcorrdata$`O-Swing%19`
zsw19i<-yearcorrdata$`Z-Swing%19`
sw19i<-yearcorrdata$`Swing%19`
swstr19i<-yearcorrdata$`SwStr%19`
ocnt19i<-yearcorrdata$`O-Contact%19`
zcnt19i<-yearcorrdata$`Z-Contact%19`
la19i<-yearcorrdata$LA19
model1i<-lm(formula = war20i ~ gbfb19i + osw19i + zsw19i + sw19i 
            + ocnt19i + zcnt19i + cnt19i + swstr19i + hard19i + ev19i + barr19i + hhit19i)
summary(model1i)
#contact? Yet no solid y2y WAR to indicators relationship: predicting just on underlying stats
#is not enough!
Hit20<-Hitters20[-c(1,2,26)]
Hit20<-as.matrix(Hit20)
for (i in c(6:18,21,22)){
  Hit20[,i]<- parse_number(Hit20[,i])/100
}
Hit20<-as.data.frame(Hit20,stringsAsFactors = F)
Hit20<-as.data.frame(sapply(Hit20,as.numeric))
war20<-Hit20$WAR
gbfb20<-Hit20$`GB/FB`
iso20<-Hit20$ISO
wRC20<-Hit20$`wRC+`
osw20<-Hit20$`O-Swing%`
zsw20<-Hit20$`Z-Swing%`
sw20<-Hit20$`Swing%`
swstr20<-Hit20$`SwStr%`
ocnt20<-Hit20$`O-Contact%`
zcnt20<-Hit20$`Z-Contact%`
cnt20<-Hit20$`Contact%`
hard20<-Hit20$`Hard%`
pull20<-Hit20$`Pull%`
ev20<-Hit20$EV
la20<-Hit20$LA
barr20<-Hit20$`Barrel%`
hhit20<-Hit20$`HardHit%`
model2<-lm(formula = war20 ~ gbfb20 + osw20 + zsw20 + sw20 + swstr20 + 
             ocnt20 + zcnt20 + cnt20 + hard20 + ev20 + la20 + barr20 + hhit20)
summary(model2)
#Bleah! But look at barrel%: significative and steady! Positive corrs with EV,Hard and HHit% but
#also SwStr: sacrificing discipline for major damage (ISO). Try to focus on those data and WAR.
model3<-lm(formula = war20 ~ wRC20)
summary(model3)
model4<-lm(formula = wRC20 ~ iso20)
summary(model4)
model5<-lm(formula = iso20 ~ barr20 + pull20)
summary(model5)
model6<-lm(formula = barr20 ~ hhit20 + la20 + swstr20)
summary(model6)
model7<-lm(formula = hhit20 ~ ev20)
summary(model7)
#look upside down for chain of relationships. What about some luck?
babip20<-Hit20$BABIP
model3a<-lm(formula = war20 ~ wRC20 + babip20)
summary(model3a)
model4a<-lm(formula = wRC20 ~ iso20 + babip20) #is this the babip level?
summary(model4a)
model5a<-lm(formula = iso20 ~ barr20 + babip20)
summary(model5a)
model6a<-lm(formula = barr20 ~ hard20 + hhit20 + babip20)
summary(model6a)
model7a<-lm(formula = hhit20 ~ hard20 + ev20 + babip20)
summary(model7a)
#it is: the "luck" factor counts with power production to give a better look at wRC+, and not
#a little better, a bunch! +0.3 Rsq = 30% of variability explanation!Does it hold for 2019 as 
#full regular season?
Hit19<-Hitters19[-c(1,2,26)]
Hit19<-as.matrix(Hit19)
for (i in c(6:18,21,22)){
  Hit19[,i]<- parse_number(Hit19[,i])/100
}
Hit19<-as.data.frame(Hit19,stringsAsFactors = F)
Hit19<-as.data.frame(sapply(Hit19,as.numeric))
war19<-Hit19$WAR
babip19<-Hit19$BABIP
gbfb19<-Hit19$`GB/FB`
iso19<-Hit19$ISO
wRC19<-Hit19$`wRC+`
osw19<-Hit19$`O-Swing%`
zsw19<-Hit19$`Z-Swing%`
sw19<-Hit19$`Swing%`
swstr19<-Hit19$`SwStr%`
ocnt19<-Hit19$`O-Contact%`
zcnt19<-Hit19$`Z-Contact%`
cnt19<-Hit19$`Contact%`
soft19<-Hit19$`Soft%`
med19<-Hit19$`Med%`
hard19<-Hit19$`Hard%`
pull19<-Hit19$`Pull%`
oppo19<-Hit19$`Oppo%`
ev19<-Hit19$EV
la19<-Hit19$LA
barr19<-Hit19$`Barrel%`
hhit19<-Hit19$`HardHit%`
model2B<-lm(formula = war19 ~ gbfb19 + osw19 + zsw19 + sw19 + swstr19 + 
             ocnt19 + zcnt19 + cnt19 + soft19 + med19 + hard19 + pull19 + oppo19 +
              ev19 + la19 + barr19 + hhit19)
summary(model2B)
#if we exclude wRC and ISO, barrels are still the most significant amongst all indicators
model3B<-lm(formula = war19 ~ wRC19)
summary(model3B)
model4B<-lm(formula = wRC19 ~ iso19 + babip19)
summary(model4B)
model5B<-lm(formula = iso19 ~ barr19 + pull19)
summary(model5B)
model6B<-lm(formula = barr19 ~ hhit19 + la19 + swstr19)
summary(model6B)
model7B<-lm(formula = hhit19 ~ ev19)
summary(model7B)
#the chain of Rsq expl holds, with higher Rsq values wrt 20 halted season. Babip?
model3c<-lm(formula = war19 ~ wRC19 + babip19)
summary(model3c)
model4c<-lm(formula = wRC19 ~ iso19 + babip19) 
summary(model4c)
model4a<-lm(formula = wRC20 ~ iso20 + babip20) 
summary(model4a)
#Rsq is the same as the 2020 model BUT babip contribution is less: on a complete season
#"luck" counts less and hard facts more! Now that we understood the value of considering
#a full season of 162 games, what about the underliners as predictors?
war19c<-fullyearcorrdata$WAR19
wrc18c<-fullyearcorrdata$`wRC+18`
iso18c<-fullyearcorrdata$ISO18
gbfb18c<-fullyearcorrdata$`GB/FB18`
osw18c<-fullyearcorrdata$`O-Swing%18`
zsw18c<-fullyearcorrdata$`Z-Swing%18`
sw18c<-fullyearcorrdata$`Swing%18`
swstr18c<-fullyearcorrdata$`SwStr%18`
ocnt18c<-fullyearcorrdata$`O-Contact%18`
zcnt18c<-fullyearcorrdata$`Z-Contact%18`
cnt18c<-fullyearcorrdata$`Contact%18`
pull18c<-fullyearcorrdata$`Pull%18`
oppo18c<-fullyearcorrdata$`Oppo%18`
soft18c<-fullyearcorrdata$`Soft%18`
med18c<-fullyearcorrdata$`Med%18`
hard18c<-fullyearcorrdata$`Hard%18`
ev18c<-fullyearcorrdata$EV18
la18c<-fullyearcorrdata$LA18
barr18c<-fullyearcorrdata$`Barrel%18`
hhit18c<-fullyearcorrdata$`HardHit%18`
model1c<-lm(formula = war19c ~ wrc18c + iso18c + gbfb18c + osw18c + zsw18c + sw18c + swstr18c +
            ocnt18c + zcnt18c + cnt18c + pull18c + oppo18c + soft18c + med18c + hard18c + 
              ev18c + la18c + barr18c + hhit18c)
summary(model1c)
#same old same old! Underliners are not enough to predict a player's value BUT by going back we
#can have some idea on a player's possible future ISO and therefore at the end a good 30% of his 
#future WAR, not great but it's something!
#End this with a look at the predictive value of wRC+
model9<-lm(formula = war20i ~ wrc19i)
summary(model9)
model9a<-lm(formula = war19c ~ wrc18c)
summary(model9a)
#in the full season case it amounts to 20%, s.t a fifth of the future WAR var can be explored with
#past wRC+...not that easy eh!?