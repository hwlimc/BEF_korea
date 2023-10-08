source('/Users/hyli0001/Documents/Rwork/functions.R')
setwd('/Users/hyli0001/Documents/Korea/Biomass/Rwork')
bm<-read.table("tree_harv.txt",header=TRUE)
bmpd<-bm[bm$sp=='PD',]
colnames(bmpd)<-c('ext_no','sp','plot','no','age','d','h','Bst','Bbr','Bf','Bcr','wd','Vst','Vst.5')
bmpd$ba<-(bmpd$d/2)^2*pi
bmpd$bef1<-(bmpd$Bst+bmpd$Bbr+bmpd$Bf)/bmpd$Bst
bmpd$bef2<-(bmpd$Bst+bmpd$Bbr+bmpd$Bf+bmpd$Bcr)/bmpd$Bst
bmpd$bef3<-(bmpd$Bst+bmpd$Bbr+bmpd$Bf+bmpd$Bcr)/bmpd$Vst


si1<-read.table("site_info_1.txt",header=TRUE)
si2<-read.table("site_info_2.txt",header=TRUE)

sipd<-si1[si1$ext_no%in%unique(bmpd$ext_no),]
sipd$sp<-'PD'
sipd$ba<-(sipd$dbh/200)^2*pi

sim1<-summaryBy(ba+h~ext_no+plot_size,sipd,FUN=md)
sim2<-summaryBy(ba~ext_no+plot_size,sipd,FUN=n)
sim2$std<-sim2$ba.n/sim2$plot_size

sim<-merge(merge(sim1,si2,by='ext_no'),sim2[,c('ext_no','std')],by='ext_no')
sim$rsd<-((sim$std/2.4711)*(sqrt(sim$ba.m/pi)*2*100/25.4)^1.605)/400


bms<-merge(bmpd,sim,by=c('ext_no'))

quartz()
par(mfrow=c(1,2))
plot(bef2~rsd,bms,ylim=c(1,2.35),bg=2,pch=21)
points(bef1~rsd,bms,ylim=c(1,2.35))
plot(bef2~age,bms,ylim=c(1,2.35),bg=2,pch=21)
points(bef1~age,bms,ylim=c(1,2.35))
