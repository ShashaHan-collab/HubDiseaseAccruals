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

load('Figure1a.RData')

circle_bar<-function(xb,index){
  var1<-paste0(xb,'_',index);var2<-paste0(xb,'_',index,2)
  data <-cbind(rownames(get(var1)),get(var1)$median1,get(var2)$median1)
  colnames(data)<-c('name','ourscene','standard')
  data<-data.frame(data)[,]
  data$ourscene<-as.numeric(data$ourscene)
  data$standard<-as.numeric(data$standard)
  
  # Set a number of 'empty bars' to create gaps for annotations
  empty_bar <- ifelse(xb=='male',10,101)
  
  # Add lines to the initial dataset
  to_add <- matrix(NA, empty_bar, ncol(data))
  colnames(to_add) <- colnames(data)
  data <- rbind(data, to_add)
  data$id <- seq(1, nrow(data))
  
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  data$name[is.na(data$name)]<-paste0('ZZ',data$id[is.na(data$name)])
  posi<-180
  cexs<-6
  ticks<-c()
  ticks[1]<-max(data$standard,na.rm = T)
  if(index=='cost'){ ticks[2:7]<-seq(0,15000,3000);ticks[8]<-15580}
  
  if(index=='zffy'){ ticks[2:10]<-seq(0,4800,600);ticks[10]<-4625}
  
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
    geom_col(aes(x=id, y=ourscene), 
             fill="#025259", width = 0.75) +
    geom_col(aes(x=id, y=standard), 
             fill="#d94f04", width = 0.35)  +
    scale_y_continuous(expand = c(0, 0),
                       #breaks = c(0, 1000, 2000, 3000),
                       limits = c(-ticks[length(ticks)]/8, ticks[length(ticks)])) +
    theme_minimal() +
    coord_polar(start = 0)+
    
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
load('Figure1b.RData')
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
    geom_point(alpha = 0.7) + 
    scale_size_continuous(range = c(1, 10)) +  
    theme_minimal() +
    scale_color_manual(values = c("Male" = '#355c7d',  "Female" = '#D32F2F'))+
    theme(axis.title = element_text(size = 14),  
          axis.text = element_text(size = 12))+   
    xlab(lab[index1])+   
    ylab(lab[index2])+
    theme(legend.position = "none",panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          # axis.line.x = element_line(color = "black"), 
          # axis.line.y = element_line(color = "black"), 
          # axis.ticks = element_line(color = "black"), 
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
  geom_point(alpha = 0.7) + 
  scale_size_continuous(range = c(1, 10)) +  
  theme_minimal() +
  scale_color_manual(values = c("Male" = '#355c7d',  "Female" = '#D32F2F'))+
  theme(axis.title = element_text(size = 14),  
        axis.text = element_text(size = 12))+  
  xlab(lab[1])+   
  ylab(lab[2])+
  labs(color = "Sex",size = "Number of Patients")
legend <- get_legend(legend_plot)
psum<-plot_grid(p,legend, ncol = 2, rel_widths = c(4, 1))
e_fig4<-p

intersect_all<-all[all$nodes%in% intersect_name,]
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
    geom_point(alpha = 0.7) +  
    scale_size_continuous(range = c(1, 10)) +  
    theme_minimal() +
    scale_color_manual(values = c("Male" = '#355c7d',  "Female" = '#D32F2F'))+
    geom_text_repel(data = subset(intersect_all, sex == "Male"),aes(label = label),cex=2.5,force=2,color='black')+
    theme(axis.title = element_text(size = 14),  
          axis.text = element_text(size = 12))+  
    xlab(lab[index1])+   
    ylab(lab[index2])+
    theme(legend.position = "none",panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          # axis.line.x = element_line(color = "black"), 
          # axis.line.y = element_line(color = "black"), 
          # axis.ticks = element_line(color = "black"), 
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
  geom_point(alpha = 0.7) +  
  scale_size_continuous(range = c(1, 10)) + 
  theme_minimal() +
  scale_color_manual(values = c("Male" = '#355c7d',  "Female" = '#D32F2F'))+
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17))+ 
  xlab(lab[1])+   
  ylab(lab[2])+
  labs(color = "Sex",size = "Number of\n patients")
legend <- get_legend(legend_plot)
psum<-plot_grid(legend,p, ncol = 2, rel_widths = c(1, 6))
e_fig5<-psum

size=30
psum<-plot_grid(fig1,plot_grid(e_fig4,e_fig5,ncol=2,labels=c('b','c'),label_size = size,rel_widths = c(6,7)),nrow=2,labels = c('a',''),label_size = size)

ggsave(filename = "Figure 1.pdf", plot = psum, width = 24, height =24,device = cairo_pdf)


