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


load('Figure5.RData')


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
  geom_point(alpha = 0.7) + # alpha sets transparency to make the chart more readable
  scale_size_continuous(range = c(1, 10)) +  # controls the size range of points
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  )+
  xlab(lab[3])+  
  ylab(lab[4])+   
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
  geom_point(alpha = 0.7) +  
  scale_size_continuous(range = c(1, 10)) +  
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),

  )+
  xlab(lab[3])+   
  ylab(lab[4])+  
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
  geom_point(alpha = 0.7) + 
  scale_size_continuous(range = c(1, 10)) + 
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),

  )+
  xlab(lab[1])+   
  ylab(lab[2])+  
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
  geom_point(alpha = 0.7) +  
  scale_size_continuous(range = c(1, 10)) +  
  theme_minimal() +
  theme(legend.position = "none",panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
  )+
  xlab(lab[1])+  
  ylab(lab[2])+  
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

# Bubble chart 
legend_plot <- ggplot(male, aes(x = visits, y = duration, color = category,size=num)) +
  geom_point(alpha = 0.7) + 
  scale_size_continuous(range = c(1, 10)) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12))+
  xlab(lab[1])+   
  ylab(lab[2])+   
  scale_color_manual(values = colours,
                     labels =labels ,breaks = c("1", "2", "hub_a_b",'3','4','hub_c_d','5','6','7'))+
  labs(
    color = "Cluster",  
    size = "Number of Patients"   
  )

legend <- get_legend(legend_plot)
psum<-plot_grid(p, legend, ncol = 2, rel_widths = c(4, 1))
ggsave(filename = "Figure 5.pdf", plot = psum, width = 12, height =9)

female$sex='Female'
male$sex='Male'
all<-rbind(male,female)


