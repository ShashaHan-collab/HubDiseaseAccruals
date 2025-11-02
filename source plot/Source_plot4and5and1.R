rm(list = ls())
library(ggplot2)
library(tidyr)
library(reshape2)
library(gridExtra)
library(readxl)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggrepel)
library(tidyverse)
setwd("D:/RA/共病轨迹/预处理代码/预处理代码/github")
load('Figure4and5.RData')
index<-c('visits','duration','cost','zffy')
lab<-c('Hospital visits (No.)', 'Lengths of stay (Days)', 'Total cost ($)', 'Out-of-pocket spendings ($)')
ps<-function(scen,key=F,weighted=F){
  assign('male_scen',get(paste0('male_scen',scen)))
  assign('female_scen',get(paste0('female_scen',scen)))
  male_combined_datas<-left_join(male_combined_data,male_scen,by='nodes')
  female_combined_datas<-left_join(female_combined_data,female_scen,by='nodes')
  if(key){
    male_combined_datas<-male_combined_datas[male_combined_datas$types=='not_key',]
    female_combined_datas<-female_combined_datas[female_combined_datas$types=='not_key',]
  }
  
  
  lim<-matrix(0,4,2)
  if(scen!=3){
    lim[1,]<-c(0,12)
    lim[2,]<-c(0,140)
    lim[3,]<-c(0,16000)
    lim[4,]<-c(0,4000)
    br<-c(2,20,2000,500)
  }
  
  if(scen==3){
    lim[1,]<-c(0,16)
    lim[2,]<-c(0,180)
    lim[3,]<-c(0,32000)
    lim[4,]<-c(0,7000)
    br<-c(2,20,4000,1000)
  }
  
  #df<-male_combined_datas
  
  bar<-function(df){
    custom_colors <- c("#1b9e77","#1b9e77", "#d95f02", "#d95f02", "#7570b3","#7570b3","#7570b3")
    # custom_colors <- c("1" = '#9de093', "2" = "#7BBCB0FF", "3" = "#FFAD0AFF",'4'='#FF0000FF','5'='#C0AED9FF',
    #             '7'='#751C6DFF','6'='#8C37E5FF')
    subbar<-function(k) {
      note<-index[k]
      notes<-colnames(df)[grep(note, colnames(df))]
      summary_df <- df %>%
        group_by(category) %>%
        summarise(
          weighted_value = sum(!!sym(notes[1]) * num) / sum(num),
          weighted_iqr1 = sum(!!sym(notes[2]) * num) / sum(num),
          weighted_iqr2 = sum(!!sym(notes[3]) * num) / sum(num),
          median_value1 = median(!!sym(notes[1])),
          Q1_value1 = quantile(!!sym(notes[1]), 0.25),
          Q3_value1 = quantile(!!sym(notes[1]), 0.75),
          IQR_value = IQR(!!sym(notes[1])),
          .groups = "drop"
        ) %>% 
        rowwise() %>%
        mutate(
          # 计算须线最小值（排除异常值）
          whisker_min = min(df[[notes[1]]][df$category == category & 
                                             df[[notes[1]]] >= (Q1_value1 - 1.5 * IQR_value)], na.rm = TRUE),
          # 计算须线最大值（排除异常值）
          whisker_max = max(df[[notes[1]]][df$category == category & 
                                             df[[notes[1]]] <= (Q3_value1 + 1.5 * IQR_value)], na.rm = TRUE)
        ) %>% 
        ungroup()
      df_complete <- data.frame(matrix(NA,3,ncol = ncol(summary_df)))
      colnames(df_complete)<-colnames(summary_df)
      df_complete$category<-c('5','6','7')
      if(nrow(summary_df)==4) summary_df<-rbind(summary_df,df_complete)
      if(weighted){y1='weighted_value';y2='weighted_iqr1';y3='weighted_iqr2'
      }else{y1='median_value1';y2='Q1_value1';y3='Q3_value1'}
      size=1
      # p<-ggplot(summary_df, aes_string(x = 'category', y = y1,fill='category')) +
      #   geom_bar(stat = "identity") +
      #   theme(
      #     plot.title = element_text(hjust = 0.3, face = "bold"),
      #     axis.title.x = element_text(face = "bold"),
      #     axis.title.y = element_text(face = "bold")
      #   )+
      #   geom_errorbar(aes_string(x = 'category', ymin=y2, ymax=y3, width=0.75),size = 0.75)+
      #   #geom_boxplot(width = 0.8, outlier.shape = NA, color = "black", position = position_dodge(width = 0.75)) +
      #   #stat_summary(fun.data = boxplot_stats, geom = "text", position = position_dodge(width = 0.75), vjust = c(-1), size = 3.5, color = "black", hjust = 0.5) +
      #   scale_fill_manual(values = custom_colors) +
      #   theme_minimal() +
      #   theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
      #         panel.grid.minor = element_blank(),
      #         #axis.line.x = element_line(color = "darkgray"), # 显示x轴线
      #         # axis.line.y = element_line(color = "black"), # 显示y轴线
      #         # axis.ticks = element_line(color = "black"), # 显示刻度线
      #         # axis.ticks.length = unit(0.25, "cm")
      #         )+
      #   labs(title = "", x = "", y = lab[k])+
      #   scale_y_continuous(limits = lim[k,],breaks = seq(0,lim[k,2],br[k])) +
      #   scale_x_discrete(labels = c("Hyper", "Isch", 
      #                                                "Hist",'After',
      #                                                'Diab','COPD','Kidn'))+
      #   geom_segment(aes(x =1, xend = 7, y = -Inf, yend = -Inf), color = "darkgray",size=size) +
      #   geom_segment(aes(y = 0, yend = lim[k,2], x = -Inf, xend = -Inf), color = "darkgray",size=size)+
      #   theme(
      #     axis.text.x = element_text(size = 18, color = "black"),
      #     axis.ticks.x = element_line(color = "darkgray"), 
      #     axis.title.x = element_text(size = 13, color = "black", face = "bold"),
      #     axis.text.y = element_text(size = 18, color = "black"),
      #     axis.ticks.y = element_line(color = "darkgray"), 
      #     axis.title.y = element_text(size = 22, color = "black", face = "bold"),
      #     axis.ticks.length = unit(0.25, "cm")
      #   )
      
      p <- ggplot(summary_df, aes_string(x = 'category', fill = 'category')) +
        # 箱线图主体（隐藏须线）
        geom_boxplot(
          aes_string(
            lower = y2,
            upper = y3,
            middle = y1,
            ymin = 'whisker_min',
            ymax = 'whisker_max'
          ),
          stat = "identity",
          width = 0.8,
          outlier.shape = NA,
          color = "black"
        ) +
        # 手动添加上须线横线
        geom_segment(aes_string(x = 'as.numeric(category) - 0.1', 
                                xend = 'as.numeric(category) + 0.1',
                                y = 'whisker_max', 
                                yend = 'whisker_max'),
                     color = "black", size = 0.75) +
        # 手动添加下须线横线
        geom_segment(aes_string(x = 'as.numeric(category) - 0.1', 
                                xend = 'as.numeric(category) + 0.1',
                                y = 'whisker_min', 
                                yend = 'whisker_min'),
                     color = "black", size = 0.75) +
        theme(
          plot.title = element_text(hjust = 0.3, face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")
        ) +
        scale_fill_manual(values = custom_colors) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 18, color = "black"),
          axis.ticks.x = element_line(color = "darkgray"), 
          axis.title.x = element_text(size = 13, color = "black", face = "bold"),
          axis.text.y = element_text(size = 18, color = "black"),
          axis.ticks.y = element_line(color = "darkgray"), 
          axis.title.y = element_text(size = 22, color = "black", face = "bold"),
          axis.ticks.length = unit(0.25, "cm")
        ) +
        labs(title = "", x = "", y = lab[k]) +
        scale_y_continuous(limits = lim[k,], breaks = seq(0, lim[k,2], br[k])) +
        scale_x_discrete(labels = c("Hyper", "Isch", "Hist", 'After', 'Diab', 'COPD', 'Kidn')) +
        geom_segment(aes(x = 1, xend = 7, y = -Inf, yend = -Inf), color = "darkgray", size = size) +
        geom_segment(aes(y = 0, yend = lim[k,2], x = -Inf, xend = -Inf), color = "darkgray", size = size)
      # var_name <- paste0('p','',k)
      # assign(var_name,p)
      return(p)
      
    }
    p1<-subbar(1)
    p2<-subbar(2)
    p3<-subbar(3)
    p4<-subbar(4)
    return(list(p1,p2,p3,p4))
  }
  
  p_male<-bar(male_combined_datas)
  p_female<-bar(female_combined_datas)
  p<-plot_grid(p_female[[1]], p_female[[2]], p_female[[3]],p_female[[4]],p_male[[1]], p_male[[2]], p_male[[3]],p_male[[4]], 
               nrow = 2,labels=c('a','','','','b'),label_size = 30)
  return(p)
}

wid=20
hei=10

p1<-ps(1,F,F)
p2<-ps(2,F,F)
p3<-ps(3,F,F)

drop_hub<-function(scen){
  lab<-c('Reduction proportions\nin hospital visits (%)', 'Reduction proportions\nin lengths of stay (%)', 'Reduction proportions\nin total cost (%)', 'Reduction proportions\nin out-of-pocket spendings (%)')
  assign('male_scen',get(paste0('male_scen',scen)))
  assign('female_scen',get(paste0('female_scen',scen)))
  male_combined_datas<-left_join(male_combined_data,male_scen,by='nodes')
  female_combined_datas<-left_join(female_combined_data,female_scen,by='nodes')
  
  drop_male_combined_datas<-male_combined_datas[male_combined_datas$types=='not_key',]
  drop_female_combined_datas<-female_combined_datas[female_combined_datas$types=='not_key',]
  
  #df1<-male_combined_datas;df2<-drop_male_combined_datas
  bar<-function(df1,df2){
    custom_colors <- c("#1b9e77","#1b9e77", "#d95f02", "#d95f02", "#7570b3","#7570b3","#7570b3")
    # custom_colors <- c("1" = '#9de093', "2" = "#7BBCB0FF", "3" = "#FFAD0AFF",'4'='#FF0000FF','5'='#C0AED9FF',
    #             '7'='#751C6DFF','6'='#8C37E5FF')
    subbar<-function(k) {
      note<-index[k]
      notes<-colnames(df1)[grep(note, colnames(df1))]
      summary_df1 <- df1 %>%
        group_by(category) %>%
        summarise(
          weighted_value = sum(!!sym(notes[1])* num) ,
          weighted_iqr1 = sum(!!sym(notes[2])* num) ,
          weighted_iqr2 = sum(!!sym(notes[3])* num) )
      
      summary_df2 <- df2 %>%
        group_by(category) %>%
        summarise(
          weighted_value = sum(!!sym(notes[1])* num) ,
          weighted_iqr1 = sum(!!sym(notes[2])* num) ,
          weighted_iqr2 = sum(!!sym(notes[3])* num) )
      
      summary_df<-summary_df1
      summary_df$weighted_value<-summary_df2$weighted_value/summary_df1$weighted_value
      summary_df$weighted_iqr1<-summary_df2$weighted_iqr1/summary_df1$weighted_value
      summary_df$weighted_iqr2<-summary_df2$weighted_iqr2/summary_df1$weighted_value
      df_complete <- data.frame(matrix(NA,3,ncol = ncol(summary_df)))
      colnames(df_complete)<-colnames(summary_df)
      df_complete$category<-c('5','6','7')
      summary_df<-rbind(summary_df[1:4,],df_complete)
      y1='weighted_value';y2='weighted_iqr1';y3='weighted_iqr2'
      size=1
      p<-ggplot(summary_df, aes_string(x = 'category', y = y1,fill='category')) +
        geom_bar(stat = "identity") +
        theme(
          plot.title = element_text(hjust = 0.3, face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold")
        )+
        #geom_errorbar(aes_string(x = 'category', ymin=y2, ymax=y3, width=0.75),size = 0.75)+
        
        #geom_boxplot(width = 0.8, outlier.shape = NA, color = "black", position = position_dodge(width = 0.75)) +
        #stat_summary(fun.data = boxplot_stats, geom = "text", position = position_dodge(width = 0.75), vjust = c(-1), size = 3.5, color = "black", hjust = 0.5) +
        scale_fill_manual(values = custom_colors) +
        theme_minimal() +
        theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
              panel.grid.minor = element_blank(),
              #axis.line.x = element_line(color = "darkgray"), # 显示x轴线
              # axis.line.y = element_line(color = "black"), # 显示y轴线
              # axis.ticks = element_line(color = "black"), # 显示刻度线
              # axis.ticks.length = unit(0.25, "cm")
        )+
        labs(title = "", x = "", y = lab[k])+
        scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.2),labels = seq(0,100,20)) +
        scale_x_discrete(labels =  c("Hyper", "Isch", 
                                     "Hist",'After',
                                     '','',''))+
        geom_segment(aes(x =1, xend = 7, y = -Inf, yend = -Inf), color = "darkgray",size=size) +
        geom_segment(aes(y = 0, yend = 1, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
        theme(
          axis.text.x = element_text(size = 18, color = "black"),
          axis.ticks.x = element_line(color = "darkgray"), 
          axis.title.x = element_text(size = 13, color = "black", face = "bold"),
          axis.text.y = element_text(size = 18, color = "black"),
          axis.ticks.y = element_line(color = "darkgray"), 
          axis.title.y = element_text(size = 22, color = "black", face = "bold"),
          axis.ticks.length = unit(0.25, "cm")
        )
      # var_name <- paste0('p','',k)
      # assign(var_name,p)
      return(p)
      
    }
    p1<-subbar(1)
    p2<-subbar(2)
    p3<-subbar(3)
    p4<-subbar(4)
    return(list(p1,p2,p3,p4))
  }
  p_male<-bar(male_combined_datas,drop_male_combined_datas)
  p_female<-bar(female_combined_datas,drop_female_combined_datas)
  p<-plot_grid(p_female[[1]], p_female[[2]], p_female[[3]],p_female[[4]],p_male[[1]], p_male[[2]], p_male[[3]],p_male[[4]], 
               nrow = 2,labels=c('c','','','','d'),label_size = 30)
}
p1T<-drop_hub(1)
p2T<-drop_hub(2)
p3T<-drop_hub(3)

P11T<-plot_grid(p1,p1T,nrow=2)
P22T<-plot_grid(p2,p2T,nrow=2)
P33T<-plot_grid(p3,p3T,nrow=2)

ggsave(filename = "Figure 4.pdf", plot = P11T, width = wid+5, height =2*hei)
ggsave(filename = "Supplementary Figure 3.pdf", plot = P22T, width = wid+5, height =2*hei)
ggsave(filename = "Supplementary Figure 4.pdf", plot = P33T, width = wid+5, height =2*hei)

##指标对比
male1<-left_join(male_combined_data,male_scen1,by='nodes')
female1<-left_join(female_combined_data,female_scen1,by='nodes')

male_key<-male1[male1$types!='not_key',]
male_keep<-male1[male1$types=='not_key',]

female_key<-female1[female1$types!='not_key',]
female_keep<-female1[female1$types=='not_key',]

male_key$category[male_key$category %in% c('1','2')]='hub_a_b'
male_key$category[male_key$category %in% c('3','4')]='hub_c_d'
male_key <- distinct(male_key)

female_key$category[female_key$category %in% c('1','2')]='hub_a_b'
female_key$category[female_key$category %in% c('3','4')]='hub_c_d'
female_key <- distinct(female_key)

male<-rbind(male_key,male_keep)
female<-rbind(female_key,female_keep)
labels<-c('1'="Hypertension",'2'= "Ischaemic", '3'="Neoplasm History",'4'='Neoplasm Aftercare',
          '5'='Diabetes','6'='COPD','7'='Kidney','hub_a_b'='Ischaemic-hypertension hub','hub_c_d'='Aftercare-history hub')
colours<- c("1" = '#9de093', "2" = "#7BBCB0FF", "3" = "#FFAD0AFF",'4'='#FF0000FF','5'='#C0AED9FF',
            '7'='#751C6DFF','6'='#8C37E5FF','hub_a_b'='#123F5AFF','hub_c_d'='darkred')
lab<-c('Hospital visits (No.)', 'Lengths of stay (Days)', 'Total cost ($1000)', 'Out-of-pocket spendings ($1000)')
female$cost<-female$cost/1000;female$zffy<-female$zffy/1000
male$cost<-male$cost/1000;male$zffy<-male$zffy/1000
size=1
p2 <- ggplot(female, aes(x = cost, y = zffy, color = category,size=num)) +
  geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
  scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
        panel.grid.minor = element_blank(),
        #axis.line.x = element_line(color = "black"), # 显示x轴线
        #axis.line.y = element_line(color = "black"), # 显示y轴线
        #axis.ticks = element_line(color = "black"), # 显示刻度线
        #axis.ticks.length = unit(0.25, "cm")
  )+
  xlab(lab[3])+   # 设置x轴标签
  ylab(lab[4])+   # 设置y轴标签
  scale_color_manual(values =colours,
                     labels = labels)+
  guides(color = FALSE, size = 'none')+
  scale_x_continuous(limits = c(0,16),breaks = seq(0,16,2))+
  scale_y_continuous(limits = c(0,5),breaks = seq(0,5,1))+
  geom_segment(aes(x = 0, xend = 16, y = -Inf, yend = -Inf), color = "darkgray",size=size) +
  geom_segment(aes(y = 0, yend = 5, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
  theme(
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )

p4 <-ggplot(male, aes(x = cost, y = zffy, color = category,size=num)) +
  geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
  scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
        panel.grid.minor = element_blank(),
        #axis.line.x = element_line(color = "black"), # 显示x轴线
        #axis.line.y = element_line(color = "black"), # 显示y轴线
        #axis.ticks = element_line(color = "black"), # 显示刻度线
        #axis.ticks.length = unit(0.25, "cm")
  )+
  xlab(lab[3])+   # 设置x轴标签
  ylab(lab[4])+   # 设置y轴标签
  scale_color_manual(values =colours,
                     labels = labels)+
  guides(color = FALSE, size = 'none')+
  scale_x_continuous(limits = c(0,16),breaks = seq(0,16,2))+
  scale_y_continuous(limits = c(0,5),breaks = seq(0,5,1))+
  geom_segment(aes(x = 0, xend = 16, y = -Inf, yend = -Inf), color = "darkgray",size=size) +
  geom_segment(aes(y = 0, yend = 5, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
  theme(
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )

p1 <- ggplot(female, aes(x = visits, y = duration, color = category,size=num)) +
  geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
  scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
        panel.grid.minor = element_blank(),
        # axis.line.x = element_line(color = "black"), # 显示x轴线
        # axis.line.y = element_line(color = "black"), # 显示y轴线
        # axis.ticks = element_line(color = "black"), # 显示刻度线
        # axis.ticks.length = unit(0.25, "cm")
  )+
  xlab(lab[1])+   # 设置x轴标签
  ylab(lab[2])+   # 设置y轴标签
  scale_color_manual(values = colours,
                     labels = c("1" ="hypertension", "2" ="ischaemic", "3" ="neoplasm history\ncluster",'4'='neoplasm aftercare\ncluster',
                                '5'='diabetes cluster','6'='COPD cluster','7'='kidney cluster','hub_a_b'='ischaemic-to-hypertension hub','hub_c_d'='aftercare-to-neoplasm-history hub'))+
  guides(color = FALSE, size = 'none')+
  scale_x_continuous(limits = c(0,12),breaks = seq(0,12,2))+
  scale_y_continuous(limits = c(0,150),breaks = seq(0,150,30))+
  geom_segment(aes(x = 0, xend = 12, y = -Inf, yend = -Inf), color = "darkgray",size=size) +
  geom_segment(aes(y = 0, yend = 150, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
  theme(
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
p3 <- ggplot(male, aes(x = visits, y = duration, color = category,size=num)) +
  geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
  scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
        panel.grid.minor = element_blank(),
        # axis.line.x = element_line(color = "black"), # 显示x轴线
        # axis.line.y = element_line(color = "black"), # 显示y轴线
        # axis.ticks = element_line(color = "black"), # 显示刻度线
        # axis.ticks.length = unit(0.25, "cm")
  )+
  xlab(lab[1])+   # 设置x轴标签
  ylab(lab[2])+   # 设置y轴标签
  scale_color_manual(values = colours,
                     labels = c("1" ="hypertension", "2" ="ischaemic", "3" ="neoplasm history\ncluster",'4'='neoplasm aftercare\ncluster',
                                '5'='diabetes cluster','6'='COPD cluster','7'='kidney cluster','hub_a_b'='ischaemic-to-hypertension hub','hub_c_d'='aftercare-to-neoplasm-history hub'))+
  guides(color = FALSE, size = 'none')+
  scale_x_continuous(limits = c(0,12),breaks = seq(0,12,2))+
  scale_y_continuous(limits = c(0,150),breaks = seq(0,150,30))+
  geom_segment(aes(x = 0, xend = 12, y = -Inf, yend = -Inf), color = "darkgray",size=size) +
  geom_segment(aes(y = 0, yend = 150, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
  theme(
    axis.text.x = element_text(size = 13, color = "black"),
    axis.ticks.x = element_line(color = "darkgray"), 
    axis.title.x = element_text(size = 13, color = "black", face = "bold"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.ticks.y = element_line(color = "darkgray"), 
    axis.title.y = element_text(size = 13, color = "black", face = "bold")
  )
p<-plot_grid(p1,p2,p3,p4, nrow = 2,labels = "auto",label_size = 15)
# 创建带图例的气泡图
legend_plot <- ggplot(male, aes(x = visits, y = duration, color = category,size=num)) +
  geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
  scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
  theme_minimal() +
  theme(axis.title = element_text(size = 14),  # 调整轴标题大小
        axis.text = element_text(size = 12))+
  xlab(lab[1])+   # 设置x轴标签
  ylab(lab[2])+   # 设置y轴标签
  scale_color_manual(values = colours,
                     labels =labels ,breaks = c("1", "2", "hub_a_b",'3','4','hub_c_d','5','6','7'))+
  labs(
    color = "Cluster",  # 更改颜色图例名称
    size = "Number of Patients"    # 更改大小图例名称
  )

legend <- get_legend(legend_plot)
psum<-plot_grid(p, legend, ncol = 2, rel_widths = c(4, 1))
ggsave(filename = "Figure 5.pdf", plot = psum, width = 12, height =9)

num=17
p_sex<-function(index1,index2) {
  maxi1<-ceiling(max(all[,index[index1]]))
  maxi2<-ceiling(max(all[,index[index2]]))
  if(index1==1) {b1<-3;maxi1=12}
  if(index1==2) {b1<-50;maxi1=150}
  if(index1==3) b1<-4
  if(index1==4) b1<-1
  
  if(index2==1) {b2<-3;maxi2=12}
  if(index2==2) {b2<-50;maxi2=150}
  if(index2==3) b2<-4
  if(index2==4) b2<-1
  p<-ggplot(all, aes_string(x = index[index1], y = index[index2], color = 'sex', size = 'num')) +
    geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
    scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
    theme_minimal() +
    scale_color_manual(values = c("Male" = '#355c7d',  "Female" = '#D32F2F'))+
    theme(axis.title = element_text(size = 14),  # 调整轴标题大小
          axis.text = element_text(size = 12))+  # 调整轴文本大小  
    xlab(lab[index1])+   # 设置x轴标签
    ylab(lab[index2])+
    theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
          panel.grid.minor = element_blank(),
          # axis.line.x = element_line(color = "black"), # 显示x轴线
          # axis.line.y = element_line(color = "black"), # 显示y轴线
          # axis.ticks = element_line(color = "black"), # 显示刻度线
          # axis.ticks.length = unit(0.25, "cm")
    )+
    scale_x_continuous(limits = c(0,maxi1),breaks = seq(0,maxi1,b1))+
    scale_y_continuous(limits = c(0,maxi2),breaks = seq(0,maxi2,b2))+
    geom_segment(aes(x = 0, xend =maxi1 , y = -Inf, yend = -Inf), color = "darkgray",size=size) +
    geom_segment(aes(y = 0, yend =maxi2, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
    theme(
      axis.text.x = element_text(size = num, color = "black"),
      axis.ticks.x = element_line(color = "darkgray"), 
      axis.title.x = element_text(size = num, color = "black", face = "bold"),
      axis.text.y = element_text(size = num, color = "black"),
      axis.ticks.y = element_line(color = "darkgray"), 
      axis.title.y = element_text(size = num, color = "black", face = "bold")
    )
  p<-ggExtra::ggMarginal(p,type = "density", margins = "both", size = 10,groupColour = T,groupFill = T)
  return(p)
}
p1<-p_sex(1,2)
p2<-p_sex(1,3)
p3<-p_sex(2,3)
p4<-p_sex(3,4)
p<-plot_grid(p1,p2,p3,p4, nrow = 2)
legend_plot <-ggplot(all, aes_string(x = index[1], y = index[2], color = 'sex', size = 'num')) +
  geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
  scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
  theme_minimal() +
  scale_color_manual(values = c("Male" = '#355c7d',  "Female" = '#D32F2F'))+
  theme(axis.title = element_text(size = 14),  # 调整轴标题大小
        axis.text = element_text(size = 12))+  # 调整轴文本大小  
  xlab(lab[1])+   # 设置x轴标签
  ylab(lab[2])+
  labs(color = "Sex",size = "Number of Patients")
legend <- get_legend(legend_plot)
psum<-plot_grid(p,legend, ncol = 2, rel_widths = c(4, 1))
e_fig4<-p

intersect_all<-all[all$nodes%in% intersect_name,]
#intersect_all$label<-gsub( "→","->", intersect_all$nodes)
intersect_all$label<-intersect_all$nodes
p_sex2<-function(index1,index2) {
  maxi1<-ceiling(max(intersect_all[,index[index1]]))
  maxi2<-ceiling(max(intersect_all[,index[index2]]))
  if(index1==1) {b1<-3;maxi1=12}
  if(index1==2) {b1<-25;maxi1=125}
  if(index1==3) b1<-5
  if(index1==4) b1<-1
  
  if(index2==1) {b2<-3;maxi2=12}
  if(index2==2) {b2<-25;maxi2=125}
  if(index2==3) b2<-5
  if(index2==4) b2<-1
  p<-ggplot(intersect_all, aes_string(x = index[index1], y = index[index2], color = 'sex', size = 'num')) +
    geom_line(aes(group = label),alpha = 0.8,size=1,color='lightgray')+
    geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
    scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
    theme_minimal() +
    scale_color_manual(values = c("Male" = '#355c7d',  "Female" = '#D32F2F'))+
    geom_text_repel(data = subset(intersect_all, sex == "Male"),aes(label = label),cex=2.5,force=2,color='black')+
    theme(axis.title = element_text(size = 14),  # 调整轴标题大小
          axis.text = element_text(size = 12))+  # 调整轴文本大小  
    xlab(lab[index1])+   # 设置x轴标签
    ylab(lab[index2])+
    theme(legend.position = "none",panel.grid.major = element_blank(), # 去除主网格线
          panel.grid.minor = element_blank(),
          # axis.line.x = element_line(color = "black"), # 显示x轴线
          # axis.line.y = element_line(color = "black"), # 显示y轴线
          # axis.ticks = element_line(color = "black"), # 显示刻度线
          # axis.ticks.length = unit(0.25, "cm")
    )+
    scale_x_continuous(limits = c(0,maxi1),breaks = seq(0,maxi1,b1))+
    scale_y_continuous(limits = c(0,maxi2),breaks = seq(0,maxi2,b2))+
    geom_segment(aes(x = 0, xend =maxi1 , y = -Inf, yend = -Inf), color = "darkgray",size=size) +
    geom_segment(aes(y = 0, yend =maxi2, x = -Inf, xend = -Inf), color = "darkgray",size=size)+
    theme(
      axis.text.x = element_text(size = num, color = "black"),
      axis.ticks.x = element_line(color = "darkgray"), 
      axis.title.x = element_text(size = num, color = "black", face = "bold"),
      axis.text.y = element_text(size = num, color = "black"),
      axis.ticks.y = element_line(color = "darkgray"), 
      axis.title.y = element_text(size = num, color = "black", face = "bold")
    )
  p<-ggExtra::ggMarginal(p,type = "density", margins = "both", size = 10,groupColour = T,groupFill = T)
  return(p)
}
p1<-p_sex2(1,2)
p2<-p_sex2(1,3)
p3<-p_sex2(2,3)
p4<-p_sex2(3,4)
p<-plot_grid(p1,p2,p3,p4, nrow = 2)
legend_plot <-ggplot(intersect_all, aes_string(x = index[1], y = index[2], color = 'sex', size = 'num')) +
  geom_point(alpha = 0.7) +  # alpha用于设置透明度，使图表更易读
  scale_size_continuous(range = c(1, 10)) +  # 控制点的大小范围
  theme_minimal() +
  scale_color_manual(values = c("Male" = '#355c7d',  "Female" = '#D32F2F'))+
  theme(axis.title = element_text(size = 14),  # 调整轴标题大小
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17))+  # 调整轴文本大小  
  xlab(lab[1])+   # 设置x轴标签
  ylab(lab[2])+
  labs(color = "Sex",size = "Number of\n patients")
legend <- get_legend(legend_plot)
psum<-plot_grid(legend,p, ncol = 2, rel_widths = c(1, 6))
e_fig5<-psum

load('Figure1.RData')

circle_bar<-function(xb,index){
  var1<-paste0(xb,'_',index);var2<-paste0(xb,'_',index,2)
  data <-cbind(rownames(get(var1)),get(var1)$median1,get(var2)$median1)
  colnames(data)<-c('name','ourscene','standard')
  data<-data.frame(data)[,]
  data$ourscene<-as.numeric(data$ourscene)
  data$standard<-as.numeric(data$standard)
  
  # Set a number of 'empty bar'
  empty_bar <- ifelse(xb=='male',10,101)
  
  # Add lines to the initial dataset
  to_add <- matrix(NA, empty_bar, ncol(data))
  colnames(to_add) <- colnames(data)
  data <- rbind(data, to_add)
  data$id <- seq(1, nrow(data))
  
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  data$name[is.na(data$name)]<-paste0('ZZ',data$id[is.na(data$name)])
  posi<-180
  cexs<-6
  ticks<-c()
  ticks[1]<-max(data$standard,na.rm = T)
  if(index=='cost'){ ticks[2:7]<-seq(0,15000,3000);ticks[8]<-15580}
  #if(index=='cost'& xb=='female'){ ticks[2:6]<-seq(0,12000,3000);ticks[7]<-15580}
  
  if(index=='zffy'){ ticks[2:10]<-seq(0,4800,600);ticks[10]<-4625}
  #if(index=='zffy'& xb=='female'){ ticks[2:7]<-seq(0,3000,600);ticks[8]<-4625}
  
  if(index=='visits'){ ticks[2:7]<-seq(0,10,2);ticks[8]<-11}
  
  if(index=='duration'){ ticks[2:6]<-seq(0,120,30);ticks[7]<-137}
  
  ticks<-round(ticks,0)
  p<-data %>% 
    ggplot() +
    geom_hline(
      aes(yintercept = y),
      data.frame(y = ticks[-c(1,length(ticks))]),
      color = "lightgray"
    )+
    geom_hline(
      aes(yintercept = y),
      data.frame(y = ticks[1]),
      color = "#d94f04"
    )+
    annotate(
      x = posi,
      y = ticks[2:(length(ticks)-1)],
      label = ticks[2:(length(ticks)-1)],
      geom = "text",
      color = "gray12",
      cex=cexs
    ) +
    # annotate(
    #   x = posi, 
    #   y = ticks[2], 
    #   label = ticks[2], 
    #   geom = "text", 
    #   color = "gray12",
    #   cex=cexs
    # ) +
    # annotate(
    #   x = posi, 
    #   y =ticks[3], 
  #   label = ticks[3], 
  #   geom = "text", 
  #   color = "gray12",
  #   cex=cexs
  # )+
  # annotate(
  #   x = posi, 
  #   y =ticks[4], 
  #   label = ticks[4], 
  #   geom = "text", 
  #   color = "gray12",
  #   cex=cexs
  # )+
  # annotate(
  #   x = posi, 
  #   y =ticks[5], 
  #   label = ticks[5], 
  #   geom = "text", 
  #   color = "gray12",
  #   cex=cexs
  # )+
  # annotate(
  #   x = posi, 
  #   y =ticks[6], 
  #   label = ticks[6], 
  #   geom = "text", 
  #   color = "gray12",
  #   cex=cexs
  # )+
  geom_col(aes(x=id, y=ourscene), 
           fill="#025259", width = 0.75) +
    geom_col(aes(x=id, y=standard), 
             fill="#d94f04", width = 0.35)  +
    scale_y_continuous(expand = c(0, 0),
                       #breaks = c(0, 1000, 2000, 3000),
                       limits = c(-ticks[length(ticks)]/8, ticks[length(ticks)])) +
    theme_minimal() +
    coord_polar(start = 0)+
    # theme(plot.background = element_rect(fill='white', color='white'),
    #       axis.title = element_blank(),
    #       panel.grid.minor  = element_blank()
    #       #,axis.text.x = element_text(angle = 45, hjust = 1)
    #       )+ 
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(0,4), "cm"),
      panel.grid.minor  = element_blank()
    )+
    geom_text(data=label_data, aes(x=id, y=ticks[length(ticks)], label=name,hjust=hjust), color="black", fontface="bold", size=4, angle= label_data$angle, inherit.aes = FALSE )
  
  return(p)
}
p_male_visits<-circle_bar('male','visits')
p_male_duration<-circle_bar('male','duration')
p_male_cost<-circle_bar('male','cost')
p_male_zffy<-circle_bar('male','zffy')

p_female_visits<-circle_bar('female','visits')
p_female_duration<-circle_bar('female','duration')
p_female_cost<-circle_bar('female','cost')
p_female_zffy<-circle_bar('female','zffy')
psum<-plot_grid(p_female_cost,p_male_cost,labels = c("Female", "Male"),
                label_size=20,hjust = -2, vjust = 2 )
fig1<-psum

size=30
psum<-plot_grid(fig1,plot_grid(e_fig4,e_fig5,ncol=2,labels=c('b','c'),label_size = size,rel_widths = c(6,7)),nrow=2,labels = c('a',''),label_size = size)

ggsave(filename = "Figure 1.pdf", plot = psum, width = 24, height =24,device = cairo_pdf)

