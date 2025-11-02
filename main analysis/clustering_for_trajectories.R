library(MCL)
library(igraph)
library(RcmdrMisc)
#XB_aim <- 0; ## XB = 0: 男； XB = 1： 女

trace_all <- vector(mode = "list", length = length(disease_trace_3_more20[,2]))
temp3 <- as.matrix(disease_trace_3_more20[,1:3])
l <- 1
if(length(disease_trace_3_more20[,1]) > 0) {
  for(i in 1:length(disease_trace_3_more20[,1])) {
    trace_all[[l]] <- temp3[i,]
    l <- l + 1
  }
}

adjacency <- matrix(0, nrow = nrow(disease_trace_3_more20), ncol = nrow(disease_trace_3_more20))
colnames(adjacency)<-rownames(adjacency)<-char_D1_D2_D3_more20
for (i in 1:(nrow(disease_trace_3_more20)-1)) {
  trace1<-trace_all[[i]]
  for (j in (i+1):nrow(disease_trace_3_more20)) {
    trace2<-trace_all[[j]]
    adjacency[i,j]<-adjacency[j,i]<-sum(trace2 %in% trace1)
  }
}
del<-names(colSums(adjacency)[(colSums(adjacency)==0)])
dels<-which(colnames(adjacency)==del)
if(!identical(dels, integer(0))) adjacency<-adjacency[-dels,-dels]
diag(adjacency)<-ncol(temp3)
adjacencyexp<-ifelse(adjacency==ncol(temp3),1,ifelse(adjacency==(ncol(temp3)-1),(ncol(temp3)-1)/( ncol(temp3)-1+2),ifelse(adjacency==(ncol(temp3)-2),(ncol(temp3)-2)/(ncol(temp3)-2+4),0)))
adjacencyexp<-exp(-adjacencyexp)
## 先用 kmeans 算法聚成 2 类，然后将大类轨迹提出， 再次用 kmeans 算法聚类
set.seed(1234)
KMeans_result_undirect_0 <- RcmdrMisc::KMeans(adjacencyexp, centers = 2, iter.max = 100, 
                                              num.seeds = 10)
clust <- KMeans_result_undirect_0$cluster
A1 <- names(clust) [clust == 1]
A2 <- names(clust) [clust == 2]

Hierarchical_KMeans1 <- function(A, adjacency,adjacencyexp,threshold=15, tau1 = 0.1, tau2 = 1) {
  mins <- round(length(A)*tau1)
  set.seed(1234)
  adjacency_A <- adjacency[A, A]
  mini<-min(colSums(adjacency_A==0))
  logi<- mini<mins
  part1 <- substring(A,1,7)
  part2 <- substring(A,5,11)
  subedge<-unique(c(part1,part2))
  M<-matrix(0,nrow = length(A),ncol = length(subedge))
  colnames(M)<-subedge
  rownames(M)<- A
  for (n in 1:length(subedge)) {
    M[,n]<-(grepl(substring(subedge,1,3)[n],A) | grepl(substring(subedge,5,7)[n],A))
  }
  hub<-names(which(colSums(M)>=length(A)*(1-tau1)))
  cores<-c()
  if(length(hub)>0){
    for (k in 1:length(hub)) {
      cores<-c(cores,which(grepl(hub[k],A)))
    }
    cores<-unique(cores)
    names(cores)<-A[cores]
  }
  flag <- T
  del<-names(colSums(adjacency_A)[(colSums(adjacency_A)-3)==0])
  dels<-which(colnames(adjacency_A)==del)
  if(!identical(dels, integer(0))) {A<-A[-dels]}
  if(length(cores)>=length(A)*tau2) flag <- F
  if(flag){
    
    A_kernal <- names(cores)
    A_rm <- setdiff(A, A_kernal)
    KMeans_result <- RcmdrMisc::KMeans(adjacencyexp[A_rm, A_rm], centers = 2, iter.max = 100, 
                                       num.seeds = 10)
    A_rm_clust <- KMeans_result$cluster
    
    A1 <- names(A_rm_clust) [A_rm_clust == 1]
    cluster1 <- vector(mode = "list", length = 2); names(cluster1) <- c("nodes", "subcluster")
    cluster1$nodes <- A1
    if(length(A1) > threshold) {
      cluster1$subcluster <- Hierarchical_KMeans1(A1, adjacency,adjacencyexp,threshold = threshold, tau1 = tau1, tau2 = tau2)
    } else {
      cluster1$subcluster <- list("flag" = F)
    }
    A2 <- names(A_rm_clust) [A_rm_clust == 2]
    cluster2 <- vector(mode = "list", length = 2); names(cluster2) <- c("nodes", "subcluster")
    cluster2$nodes <- A2
    if(length(A2) > threshold) {
      cluster2$subcluster <- Hierarchical_KMeans1(A2, adjacency,adjacencyexp, threshold = threshold, tau1 = tau1, tau2 = tau2)
    } else {
      cluster2$subcluster <- list("flag" = F)
    }
    
    result <- vector(mode = "list", length = 4)
    names(result) <- c("flag", "keynodes", "cluster1", "cluster2")
    result$flag <- flag
    result$keynodes <- A_kernal
    result$hub <- hub
    result$cluster1 <- cluster1
    result$cluster2 <- cluster2
  } else {##  没有核心
    result <- list("flag" = F)
  }  
  
  
  
  return(result)
}

C1<-Hierarchical_KMeans1(A1, adjacency,adjacencyexp,threshold=15, tau1 = 0.1, tau2 = 1)
C2<-Hierarchical_KMeans1(A2, adjacency,adjacencyexp,threshold=15, tau1 = 0.1, tau2 = 1)
cluster1 <- list("nodes" = A1, "subcluster" = C1)
cluster2 <- list("nodes" = A2, "subcluster" = C2)
result <- list("flag" = T, "keynodes" = NULL, "cluster1" =cluster1, "cluster2" = cluster2)

obtain_class_code <- function(A, cluster, level = NULL) {
  ##  level1: C0, C1, C2
  ##  level2: C10, C11, C12, C20, C21, C22
  temp_func <- function(A, cluster) {
    class_of_A <- rep("", length(A)); names(class_of_A) <- A
    if(cluster$flag) {
      if(!is.null(cluster$keynodes)) {
        class_of_A[cluster$keynodes] <- "0"
      }
      if(is.null(cluster$cluster1)) warning("'cluster1' is NULL when 'flag' is true!")
      cluster1 <- cluster$cluster1$nodes
      class_of_A[cluster1] <- "1"
      class_of_A[cluster1] <- paste0(class_of_A[cluster1], temp_func(cluster1, cluster$cluster1$subcluster))
      if(is.null(cluster$cluster2)) warning("'cluster2' is NULL when 'flag' is true!")
      cluster2 <- cluster$cluster2$nodes
      class_of_A[cluster2] <- "2"
      class_of_A[cluster2] <- paste0(class_of_A[cluster2], temp_func(cluster2, cluster$cluster2$subcluster))
      return(class_of_A)
    } else {
      return(class_of_A)
    }
  }
  class_code <- paste0("C", temp_func(A, cluster))
  names(class_code) <- A
  
  if(is.null(level)) return(class_code)
  class_code <- substr(class_code, 1, level + 1)
  return(class_code)
}
class <- obtain_class_code(A = colnames(adjacency), cluster = result)
sort(unique(class))
obtain_class <- function(class_code, level = max(nchar(class_code))-1) {
  ## 按 level 截断
  class_code <- substr(class_code, 1, level + 1)
  class_code_uni <- sort(unique(class_code))
  temp <- nchar(class_code_uni)
  logi <- substr(class_code_uni, temp, temp) == "0"
  key_codes <- class_code_uni[logi]##  核心的类代码
  class_code_true <- class_code_uni[!logi]##  真实类代码
  nodes_in_key <- vector(mode = 'list', length = length(key_codes))
  names(nodes_in_key) <- key_codes
  for(code in key_codes) {
    temp <- names(class_code) [class_code == code]
    char <- paste0("key_level", 1:level) [nchar(code) - 1]
    temp_type <- rep(char, length(temp))
    nodes_in_key[[code]] <- data.frame("nodes" = temp, "types" = temp_type, stringsAsFactors = F)
  }
  nodes_in_true_class <- vector(mode = 'list', length = length(class_code_true))
  names(nodes_in_true_class) <- class_code_true
  for(code in class_code_true) {
    temp <- names(class_code) [class_code == code]
    temp_type <- rep("not_key", length(temp))
    nodes_in_true_class[[code]] <- data.frame("nodes" = temp, "types" = temp_type, stringsAsFactors = F)
  }
  for(code in key_codes) {
    code_rm0 <- substr(code, 1, nchar(code) - 1)
    new_code <- code_rm0
    while(T) {
      new_code <- c(paste0(new_code, 1), paste0(new_code, 2))
      logi <- nchar(new_code) - 1 <= level
      new_code <- new_code[logi]
      if(length(new_code) == 0) break
      logi <- new_code %in% class_code_true
      if(any(logi)) {
        for(code_treat in new_code[logi]) {
          temp <- rbind(nodes_in_true_class[[code_treat]], nodes_in_key[[code]])
          nodes_in_true_class[[code_treat]] <- temp
        }
      }
      new_code <- new_code[!logi]
      if(length(new_code) == 0) break
    }
  }
  ## 整理，排序
  for(i in 1:length(nodes_in_true_class)) {
    temp <- nodes_in_true_class[[i]]
    nodes_in_true_class[[i]] <- temp[order(temp[, 1]), ]
  }
  return(nodes_in_true_class)
}
class_level1 <- obtain_class(class_code = class, level = 1)
class_level2 <- obtain_class(class_code = class, level = 2)
class_level3 <- obtain_class(class_code = class, level = 3)
level_max <- max(nchar(class))-1
library(openxlsx)
wb <- createWorkbook()
wb2 <- createWorkbook()
class_level <- obtain_class(class_code = class, level = level_max)
class_name<-names(class_level)
modi<-c()
for (j in 1:length(class_name)) {
  if(any(grepl(class_name[j], class_name[-j]))) {
    modi<-c(modi,class_name[j])}
}
if(!is.null(modi)){
  modi_trace<-class_level[[modi]]
  class_level<-class_level[-which(class_name%in%modi)]
  for (j in 1:nrow(modi_trace)) {
    t<-modi_trace$nodes[j]
    ranks<-c()
    for (k in 1:length(class_level)) {
      tem<-class_level[[k]]$nodes
      ranks[k]<-sum(adjacency[t,colnames(adjacency) %in% tem])
    }
    add<-which.max(ranks)
    class_level[[add]]<-rbind(class_level[[add]],modi_trace[j,])
  }
}

for (sheet_name in names(class_level)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, class_level[[sheet_name]])
  addWorksheet(wb2, sheet_name)
  split_vector <- unique(unlist(strsplit(class_level[[sheet_name]]$nodes, split = "_")))
  writeData(wb2, sheet_name, split_vector)
}
