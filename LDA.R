rm(list=ls())
setwd("")

library(tidyverse)
library(microeco)
library(magrittr)
#读取数据
otu <-  read.csv("ASV.csv", row.names = 1)
group <-  read.csv("group-LDA.csv", row.names = 1)
tax <-  read.csv("TAXA.csv", row.names = 1)

otu1 = otu[!(rownames(otu) %in% otu), ]  
freq = apply(otu1, 1, function(x) sum(x > 0)/length(x))
keep = freq >= 4/5 
otu2 = otu1[keep, ] 

dataset <- microtable$new(sample_table = group,
                          otu_table = otu2, 
                          tax_table = tax)
dataset
dataset$otu_table



#开始LEfse
lefse <- trans_diff$new(dataset = dataset, 
                        method = "lefse", 
                        group = "group", 
                        alpha = 0.01, 
                        lefse_subgroup = NULL,
                        only_group_level = "genus")
# 查看分析结果
head(lefse$res_diff)
# 设置字体
windowsFonts(RMN=windowsFont("Times New Roman"))
# 绘制前30个具有最高LDA（log10）的分类单元的差异特征柱状图
lefse$plot_diff_bar(use_number = 1:30, 
                    width = 0.8, 
                    group_order = c("Woodland-N0", "Woodland-N", "Grassland-N0","Grassland-N", "Cropland-N0","Cropland-N"))+ 
  scale_fill_manual(values = c("Woodland-N0" = "#6897cf", "Woodland-N" = "#669a38", "Grassland-N0" = "#f58c86", "Grassland-N" = "#f6d586", "Cropland-N0"= "#f3ed99","Cropland-N"= "#ededef"))
