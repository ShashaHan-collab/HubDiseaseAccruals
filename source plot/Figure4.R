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
load('Figure4.RData')
index<-c('visits','duration','cost','zffy')
lab<-c('Hospital visits (No.)', 'Lengths of stay (Days)', 'Total cost ($)', 'Out-of-pocket spendings ($)')
# Define nested function to generate box plots
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
  
  bar<-function(df){
    custom_colors <- c("#1b9e77","#1b9e77", "#d95f02", "#d95f02", "#7570b3","#7570b3","#7570b3")
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
          whisker_min = min(df[[notes[1]]][df$category == category & 
                                             df[[notes[1]]] >= (Q1_value1 - 1.5 * IQR_value)], na.rm = TRUE),
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
      p <- ggplot(summary_df, aes_string(x = 'category', fill = 'category')) +
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
        geom_segment(aes_string(x = 'as.numeric(category) - 0.1', 
                                xend = 'as.numeric(category) + 0.1',
                                y = 'whisker_max', 
                                yend = 'whisker_max'),
                     color = "black", size = 0.75) +
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
      return(p)
      
    }
    p1<-subbar(1)
    p2<-subbar(2)
    p3<-subbar(3)
    p4<-subbar(4)
    return(list(p1,p2,p3,p4))
  }
  # Generate plots for male and female
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
# Define function to generate reduction proportion plots
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

