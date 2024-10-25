rm(list =ls())
setwd("")
library(metafor)

library(readxl)
d1 <- read_excel("aerobic_chemoheterotrophy.xlsx")


###
d2<-escalc(measure="ROM",data=d1,m1i=treatment_mean,sd1i=treatment_sd,n1i=treatment_n,m2i=control_mean,sd2i=control_sd,n2i=control_n)




#
r11<-rma(yi,vi, data=subset(d2,group1=="A"), method="REML")

#
summary(r11)

##
fsn(yi,vi, data=subset(d2,group1=="A")) 

windowsFonts(RMN=windowsFont("Times New Roman"))

regtest(r11, model="rma") 
par(family = "RMN")  # 


#
funnel(r11, main = "Ureolysis", cex.main = 1.5,  xlab = "LnRR", ylab = "Standard Error", cex.lab = 1.5, font.lab = 2, font.main = 2, cex.axis = 1.2, font.axis = 2)
text(x = max(d2$yi+0.12), y = max(d2$vi)-0.0150, labels = "P =0.0465" , pos = 1, font = 2, cex = 1.2)
text(x = min(d2$yi-0.05), y = max(d2$vi)-0.0150, labels = "(d)", pos = 1, font = 2, cex = 1.5)


#
funnel(r11, main="Standard Error")
funnel(r11, yaxis="vi", main="Sampling Variance")
funnel(r11, yaxis="seinv", main="Inverse Standard Error")
funnel(r11, yaxis="vinv", main="Inverse Sampling Variance")


#
summary(r11)#
t<-trimfill(r11,side="left" )#
t##


