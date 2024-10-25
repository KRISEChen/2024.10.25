rm(list=ls())#clear Global Environment
setwd('')#
library(linkET)
library(ggplot2)
library(dplyr)
library(cols4all)
#
varespec <- read.csv("factor.csv", row.names = 1, header=T, sep=",",check.names = FALSE)
varechem <- read.csv("map.csv", row.names = 1, header=T, sep=",",check.names = FALSE)

cor2 <- correlate(varechem, method = "spearman")
corr2 <- cor2 %>% as_md_tbl()

cor3 <- correlate(varespec, varechem, method = "pearson")
corr3 <- cor3 %>% as_md_tbl()
 
r.p.data.plot <- corr3 %>% 
  mutate(r.sign = cut(r, breaks = c(-Inf, 0, Inf), 
                      labels = c("Negative", "Positive")),
         p.sign = cut(p, breaks = c(0, 0.05, Inf), 
                      labels = c("P<0.05", "P>=0.05"),
                      include.lowest = TRUE,#
                      right = FALSE), #
         r.abs = cut(abs(r), breaks = c(0, 0.25, 0.5, 1),
                     labels = c("<0.25","0.25-0.5", "0.5-1"),
                     include.lowest = TRUE,#
                     right = FALSE),# 
  )  

#首先，绘制相关性热图:
p4 <- qcorrplot(cor2,
                grid_col = "gray50",#
                grid_size = 0.4,#
                type = "upper",#
                diag = F) +#
  geom_square() +#
  scale_fill_gradientn(colours = rev(c4a('rd_bu',30)),#
                       limits = c(-1, 1))
p4

#添加显著性标签：
p5 <- p4 +
  geom_mark(size = 4,#
            only_mark = T,
            sig_level = c(0.05, 0.01, 0.001),#
            sig_thres = 0.05,#
            colour = 'black')#
p5

p6 <- p5 +
  geom_couple(data = r.p.data.plot,   #
              aes(colour = r.sign,#
                  size = r.abs,#
                  linetype = p.sign), #
              nudge_x = 0.15,#
              curvature = 0.2,#
              label.fontface=1,#
              label.family = "serif",#
              label.size = 4)#
p6

#继续美化连线:
p7 <- p6 +
  scale_size_manual(values = c("<0.25" = 0.5,
                               "0.25-0.5" = 1.5,
                               "0.5-1" = 4.5)) +  #
  scale_colour_manual(values = c("Negative" = "#009f76",
                                 "Positive" = "#d95e27"))+ 
  scale_linetype_manual(values = c("P<0.05" = "solid",
                                   "P>=0.05" = "dashed"))+
  scale_fill_gradientn(colours = rev(c("#990004", "#e4795d", "#fbfbf7", "#4895c7", "#1e405f")),#
                       breaks = seq(-1, 1, 0.5),
                       limits = c(-1, 1))+ #设置图例范围
  #geom_diag_label()+
  guides(
    fill = guide_colorbar(title = "Spearman's r",barwidth = 1,barheight = 8, order = 1), 
    linetype = guide_legend(title = NULL,override.aes = list(size = 6,linewidth = 0.6, order = 3)),
    colour = guide_legend(title = NULL,override.aes = list(size = 1,linewidth = 0.6, order = 4)),
    size = guide_legend(title ="pearson's r" ,override.aes = list(colour = "black",size = 1),
                        order = 2)  #此处均为标题设置
  )
p7
