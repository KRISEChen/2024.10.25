rm(list=ls())#clear Global Environment
library(vegan)
library(ggrepel)
library(ggplot2)

design<-read.csv("group.csv", row.names = 1, header=T, sep=",",check.names = FALSE)
otu<-read.csv("OTU.csv", row.names = 1, header=T, sep=",",check.names = FALSE)
#
sampledata <- t(otu)
sampledata <- decostand(sampledata,method = "hellinger")
group <- design[,2]
env = design[,4:12]
dca = decorana(veg = sampledata)
dca
#
RDA = rda(t(sampledata[,1:1561]),design[,4:12], scale = TRUE)
env = design[,4:12]



var_cca <- cca(sampledata, env, scale = TRUE)#将生物丰度数据和环境数据一起分析，为约束分析
summary(var_cca)



#CCA
library(vegan)
library(venn)

var_cca_score <- scores(var_cca)#
var_cca_score$sites#
var_cca$CCA$biplot#
var_cca_score$species#


# 
CCAE <- as.data.frame(var_cca$CCA$biplot[,1:2])#
CCAS1 <- var_cca_score$sites[,1]*0.5#
CCAS2 <- var_cca_score$sites[,2]*0.5#
plotdata <- data.frame(rownames(var_cca_score$sites), CCAS1, CCAS2)#
colnames(plotdata) <- c("sample","CCAS1","CCAS2")#

library(tidyverse)
plotdata <- plotdata %>%
  mutate(
    group = group)#
plotdata #


col <- c('#909291','#8D2F25', '#4E1945', '#8CBF87', '#3E608D' ) # 
cca1 <- round(var_cca$CCA$eig[1]/sum(var_cca$CCA$eig)*100,2)#
cca2 <- round(var_cca$CCA$eig[2]/sum(var_cca$CCA$eig)*100,2)#

library(ggrepel)
library(ggpubr)

#
plotdata$group <- factor(plotdata$group, levels = c("Control", "Low nitrogen", "Medium nitrogen", "High nitrogen"))
CCA_site <- ggplot(plotdata, aes(CCAS1, CCAS2)) +
  geom_point(size = 3,aes(fill = group,color=group))+#
  scale_color_manual(values = col)+#
  xlab(paste("CCA1 ( ",cca1,"%"," )", sep = "")) +
  ylab(paste("CCA2 ( ",cca2,"%"," )", sep = "")) +
  geom_segment(data = CCAE,
               aes(x = 0, y = 0, xend = CCAE[,1]*1.7, yend = CCAE[,2]*1.7),
               colour = "red", linewidth = 1,#
               arrow = arrow(angle = 10,length = unit(0.5, "cm"))) +#
  geom_text_repel(data = CCAE, segment.colour = "black", family = 'Times New Roman',#
                  aes(x = CCAE[,1], y = CCAE[,2], # 
                      label = rownames(CCAE)),size=5) +#
  geom_vline(aes(xintercept = 0), linetype = "dashed",size=1,color= "grey60") +#
  geom_hline(aes(yintercept = 0), linetype = "dashed",size=1,color= "grey60") +#
CCA_site
library(ggExtra)

p2 = ggMarginal(CCA_site,type="density",size=4,margins="both",groupColour=TRUE,groupFill=TRUE)
p2 


envfit <- envfit(var_cca, env ,permutations  = 999)#
envfit

# 
results <- as.data.frame(scores(envfit, display = "vectors"))
results$r_squared <- envfit$vectors$r
results$p_value <- envfit$vectors$p

# 
significant_results <- results[results$p_value < 0.05, ]

# 
write.csv(significant_results, "envfit_results.csv", row.names = FALSE)

# 
print(significant_results)
#
r <- as.matrix(envfit$vectors$r)
p <- as.matrix(envfit$vectors$pvals)
env.p <- cbind(r,p)
colnames(env.p) <- c("r2","p-value")
r2_P <- as.data.frame(env.p)
r2_P