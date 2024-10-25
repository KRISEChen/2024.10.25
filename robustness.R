rm(list=ls())
#packages <- c("meconetcomp", "rgexf", "pheatmap", "aplot", "agricolae")
# Now check or install
#for(x in packages){
  #if(!require(x, character.only = TRUE)) {
  #  install.packages(x, dependencies = TRUE)
  #}
#}

library(microeco)
library(meconetcomp)
# use pipe operator in magrittr package
library(magrittr)
library(igraph)
library(ggplot2)


setwd("C:/Users/啦啦啦/Desktop/文章1/网络图绘制数据/施氮年限/0-5鲁棒性")


otu <- read.csv("ASV.csv",row.names = 1)
sam <- read.csv("group.csv",row.names = 1)
tax <- read.csv("TAX.csv",row.names = 1)



dataset <- microtable$new(sample_table = sam,
                          otu_table = otu,
                          tax_table = tax,
                          #phylo_tree = phylo_tree_16S,
                          auto_tidy = F)

dataset



#鲁棒性
# first create a list
soil_amp_network <- list()
# select samples of "IW" group
# use clone to get a deep copy of soil_amp (R6 object)
tmp <- clone(dataset)
# change sample_table directly
tmp$sample_table %<>% subset(group == "N0")
# trim all files in the object
tmp$tidy_dataset()
# use filter_thres parameter to filter the feature with low relative abundance
tmp <- trans_network$new(dataset = tmp, cor_method = "spearman", filter_thres = 0.001)
# COR_p_thres represents the p value threshold
# COR_cut denotes the correlation coefficient threshold
tmp$cal_network(COR_p_thres = 0.05, COR_cut = 0.56)
# put the network into the list
soil_amp_network$N0 <- tmp
# select samples of "TW" group
tmp <- clone(dataset)
tmp$sample_table %<>% subset(group == "N")
tmp$tidy_dataset()
tmp <- trans_network$new(dataset = tmp, cor_method = "spearman", filter_thres = 0.001)
tmp$cal_network(COR_p_thres = 0.05, COR_cut = 0.56)
soil_amp_network$`N` <- tmp
# select samples of "TW" group
tmp <- clone(dataset)
tmp$sample_table %<>% subset(group == "Medium nitrogen")
tmp$tidy_dataset()
tmp <- trans_network$new(dataset = tmp, cor_method = "spearman", filter_thres = 0.001)
tmp$cal_network(COR_p_thres = 0.05, COR_cut = 0.51)
soil_amp_network$`Medium nitrogen` <- tmp
# select samples of "TW" group
tmp <- clone(dataset)
tmp$sample_table %<>% subset(group == "High nitrogen")
tmp$tidy_dataset()
tmp <- trans_network$new(dataset = tmp, cor_method = "spearman", filter_thres = 0.001)
tmp$cal_network(COR_p_thres = 0.05, COR_cut = 0.65)
soil_amp_network$`High nitrogen` <- tmp


#Robustness of network
tmp <- robustness$new(soil_amp_network, remove_strategy = c("edge_rand", "edge_strong", "node_degree_high"), 
                     # remove_ratio = seq(0, 0.99, 0.1), measure = c("Eff", "Eigen", "Pcr"), run = 10)
                     remove_ratio = seq(0, 0.99, 0.1), measure = c("Eff", "Eigen"), run = 100)
View(tmp$res_table)

tmp$res_table$remove_strategy <- gsub("edge_rand",  "Edge rand", tmp$res_table$remove_strategy)
tmp$res_table$remove_strategy <- gsub("edge_strong", "Edge strong", tmp$res_table$remove_strategy)
tmp$res_table$remove_strategy <- gsub("node_degree_high", "Node degree high", tmp$res_table$remove_strategy)
tmp$res_table

#tmp$plot(linewidth = 1) 
##################
#'#8D2F25', '#4E1945', '#CB9475', '#8CBF87', '#3E608D', '#909291'
custom_order <- c("Control", "Low nitrogen", "Medium nitrogen","High nitrogen" )
custom_colors <- c('#8D2F25', '#4E1945', '#CB9475', '#3E608D')
custom_labels <- c("edge_rand", "edge_strong", "node_degree_high")  # 自定义的条带标签
names(custom_labels) <- c("Edge rand", "Edge strong", "Node degree high")  # 假设你的因子水平为 A, B, C

p <- tmp$plot(linewidth = 1)+  theme(
  panel.border = element_rect(color = "black", size = 1),
  legend.position = "bottom",
  text = element_text(family = "Times New Roman"),  # 设置文本字体
  axis.text = element_text(face = "bold",size = 12 , color = "black"),  # 设置坐标轴文本大小
  axis.title = element_text(face = "bold",size = 14, color = "black"),  # 设置坐标轴标题大小
  legend.text = element_text(face = "bold",size = 12, color = "black"),  # 设置图例文本大小
  legend.title = element_blank(),
  strip.text = element_text(size = 14, family = "Times New Roman", face = "bold"), # 设置分面条带背景为透明
  strip.background = element_rect(color = "white", fill = "gray80", size = 1), #element_blank()  设置分面条带文本样式# 设置图例标题大小
  strip.margin = unit(c(1, 1, 1, 1), "cm") )+ # 调整分面条带与面板边框的距离，顺序为上、右、下、左)
 scale_x_continuous(name = "Remove ratio")+
  labs(color = "Typ")+
  scale_color_manual(values = custom_colors, 
                     breaks = custom_order) 
p
##################################
custom_order <- c("N0", "N" )
custom_colors <- c('#8D2F25',  '#3E608D')
custom_labels <- c("edge_rand", "edge_strong", "node_degree_high")  # 自定义的条带标签
names(custom_labels) <- c("Edge rand", "Edge strong", "Node degree high")  # 假设你的因子水平为 A, B, C

p <- tmp$plot(linewidth = 1)+  theme(
  panel.border = element_rect(color = "black", size = 1),
  legend.position = "bottom",
  text = element_text(family = "Times New Roman"),  # 设置文本字体
  axis.text = element_text(face = "bold",size = 12 , color = "black"),  # 设置坐标轴文本大小
  axis.title = element_text(face = "bold",size = 14, color = "black"),  # 设置坐标轴标题大小
  legend.text = element_text(face = "bold",size = 12, color = "black"),  # 设置图例文本大小
  legend.title = element_blank(),
  strip.text = element_text(size = 14, family = "Times New Roman", face = "bold"), # 设置分面条带背景为透明
  strip.background = element_rect(color = "white", fill = "gray80", size = 1), #element_blank()  设置分面条带文本样式# 设置图例标题大小
  strip.margin = unit(c(1, 1, 1, 1), "cm") )+ # 调整分面条带与面板边框的距离，顺序为上、右、下、左)
  scale_x_continuous(name = "Remove ratio")+
  labs(color = "Typ")+
  scale_color_manual(values = custom_colors, 
                     breaks = custom_order) 
p
#Vulnerability of nodes
vul_table <- vulnerability(soil_amp_network)
View(vul_table)