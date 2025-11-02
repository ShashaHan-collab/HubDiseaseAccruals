rm(list = ls())
setwd("D:/RA/共病轨迹/预处理代码/预处理代码")
library(ggplot2)
library(reshape2)
library(devtools)
library(dplyr)
library(grid)
library(cowplot)
library(stringr)
library(igraph)
library(mlbench)    
library(psych)
library(RColorBrewer)
library(circlize)
library(dendextend)
library(openxlsx)
library(readr)
library(ggraph)
library(tidygraph)
library(RcmdrMisc)
# save(char_D1_D2_D3_more20,label3,class_level,discription,disease_trace_3_more20,file='github/Figure2.RData')
Fig_2and3<-function(XB_aim){
if(XB_aim==0) load('github/Figure3.RData')
if(XB_aim==1) load('github/Figure2.RData')

for (k in 1:length(class_level)) {
  var_name <- paste("p", k, sep="")  
  temp_list1<-temp_list2 <- vector(mode = "list", length = 2)
  subcluster<-class_level[[k]]
  subcluster1<-subcluster[subcluster$types!='not_key',]
  subcluster2<-subcluster[subcluster$types=='not_key',]
  logi1 <- char_D1_D2_D3_more20 %in% subcluster1$nodes
  logi2 <- char_D1_D2_D3_more20 %in% subcluster2$nodes
  if(any(logi1)) {
    disease_trace_3_more20_D1 <- disease_trace_3_more20[logi1, ]
    for(j in 1:2) {
      temp <- disease_trace_3_more20_D1[, c(j:(j+1), 4)]
      colnames(temp) <- c("from", "to", "weight")
      temp_list1[[j]] <- temp
    }
  }
  if(any(logi2)) {
    disease_trace_3_more20_D1 <- disease_trace_3_more20[logi2, ]
    for(j in 1:2) {
      temp <- disease_trace_3_more20_D1[, c(j:(j+1), 4)]
      colnames(temp) <- c("from", "to", "weight")
      temp_list2[[j]] <- temp
    }
  }
  ed<-function(temp_list){
    edges <- do.call("rbind", temp_list)
    rownames(edges) <- as.character(1:length(edges[,1]))
    weight <- edges %>%
      group_by(from, to) %>%
      summarise(Sum = sum(weight), .groups = 'drop')
    counts<-edges %>%
      group_by(from, to) %>%
      summarise(Count = n(), .groups = 'drop')
    edges<-merge(counts,weight)
    return(edges)}
  e1<-data.frame(character(),
                 character(),
                 integer(),
                 factor())
  if(any(logi1)) {e1<-ed(temp_list1);e1$key<-1}
  e2<-ed(temp_list2)
  e2$key<-2
  e<-rbind(e1,e2)
  
  
  if(any(logi1)){
    exclu<-intersect(e2[,1:2],e1[,1:2])
    e$interaction = interaction(e$from, e$to)
    exclu$interaction = interaction(exclu$from, exclu$to)
    e$key[e$key==2& e$interaction %in% exclu$interaction]<-0
  }
  total_from <- e %>%
    group_by(from) %>%
    summarise(Total = sum(Count)) %>%
    rename(Point = from)
  
  total_to <- e %>%
    group_by(to) %>%
    summarise(Total = sum(Count)) %>%
    rename(Point = to)
  
  total_points <- bind_rows(total_from, total_to) %>%
    group_by(Point) %>%
    summarise(Final_Total = sum(Total))
  nodes<-total_points
  colnames(nodes)<-c('label','degree')
  nodes$icd<-nodes$label
  nodes$name<-discription[nodes$label]
  nodes$color<-label3[nodes$label]
  net_pc<-graph_from_data_frame(
    d=e,vertices=nodes)
  graph_pc<-as_tbl_graph(net_pc)
  maxi<-100
  if(XB_aim==1 & k==4) maxi<-3
  p<- ggraph(graph_pc, layout = 'sugiyama',hgap = 0.75,vgap=2,maxiter = maxi, attributes = "all")+
    geom_edge_link(aes(filter= key==2,edge_width=Sum),color="gray",
                   arrow = arrow(length = unit(3.5, 'mm')),
                   end_cap=circle(0.35,'inches'),start_cap =circle(0.35,'inches'),show.legend = FALSE)+
    geom_edge_link(color="#1b9e77",aes(filter= key==1,edge_width=Sum),end_cap=circle(0.35,'inches'),start_cap =circle(0.35,'inches'),
                   show.legend = FALSE,arrow = arrow(length = unit(3.5, 'mm')))+
    geom_node_label(aes(label = icd,size=degree), color = nodes$color,family = "serif", fontface = "bold",show.legend = FALSE)+
    scale_size(range = c(3.5, 8))+
    geom_node_text(seed=1,aes(label = name), family = "sans",cex=4, fontface = "italic",repel = TRUE,box.padding=0.6,segment.color=NA)+
    theme(panel.background = element_rect(fill='white'), plot.margin = margin(t = 0, r = 0, b = 0,l = 0,unit = "cm"))+
    scale_x_continuous(expand = c(0, 0.35))+
    scale_y_continuous(expand = c(0, 0.35))+ scale_y_reverse()+coord_flip()
  
  assign(var_name, p) 
  
}
labelsize=30
if(XB_aim==0){
prow0<-plot_grid(p1,p2,rel_widths =c(1,1), labels = "auto", hjust = 0, vjust = 1,label_size = labelsize,nrow=1)
prow1<-plot_grid(p3,p4,rel_widths =c(1,1), labels = c('c','d'), hjust = 0, vjust = 1,label_size = labelsize,nrow=1)
prow2<-plot_grid(p5,p6,p7,rel_widths =c(1,1,1), labels = c('Diabetes\ncluster','COPD\ncluster','Kidney\ncluster'), hjust = 0, vjust = 1,label_size = 20,nrow=1)
prowsum<-plot_grid(prow0,prow1, prow2, nrow=3)
ggsave(paste0("github/", "Figure3.pdf"),prowsum, width = 24, height =36,limitsize = FALSE)
}

if(XB_aim==1){
  prow0<-plot_grid(p1,p2,rel_widths =c(1,1), labels = "auto", hjust = 0, vjust = 1,label_size = labelsize,nrow=1)
  prow1<-plot_grid(p3,p4,rel_widths =c(1,1), labels = c('c','d'), hjust = 0, vjust = 1,label_size = labelsize,nrow=1)
  prowsum<-plot_grid(prow0,prow1, nrow=2)
  ggsave(paste0("github/", "Figure2.pdf"),prowsum, width = 24, height =24,limitsize = FALSE)
}
}
Fig_2and3(0)
Fig_2and3(1)