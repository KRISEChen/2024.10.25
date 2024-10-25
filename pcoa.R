rm(list = ls())#
getwd()
setwd("")
library(multcompView)
library(patchwork)
library(readr)
library(dplyr)
library(stringr)
#数据加载和清洗
df <- read_csv("OTU1.csv") %>% 
  dplyr::rename(sample = "") %>% 
  mutate(across(.cols = "", .fns = ~ str_replace(., "", "")))
pcoa <- cmdscale(vegdist(df %>% column_to_rownames(var = "") %>% 
                           dplyr::select(-Type), method = "bray"), 
                 k = 2, eig = TRUE)
pcoadata <- data.frame(PC1 = pcoa$points[,1], PC2 = pcoa$points[,2]) %>%
     rownames_to_column(var = "sample") %>%
     left_join(., df %>% dplyr::select(sample, Type), by = "sample")

pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)

##################################################


      plot <- ggplot(pcoadata, aes(PC1, PC2)) +
        geom_point(aes(colour=Type,fill=Type),size=2)+
        labs(x=paste0("(PCoA1: ",round( pcoa_eig[1]*100,2),"%)"),
             y=paste0("(PCoA2: ",round( pcoa_eig[2]*100,2),"%)") )+
        geom_vline(aes(xintercept = 0),linetype="dotted")+
        theme_bw()
plot 
      
      