library(MCL)
library(igraph)
library(RcmdrMisc)
#XB_aim <- 0; ## XB = 0: Male; XB = 1: Female

# Store all 3-length diagnosis trajectories
trace_all <- vector(mode = "list", length = length(disease_trace_3_more20[,2]))
temp3 <- as.matrix(disease_trace_3_more20[,1:3])  # Extract D1, D2, D3 columns as matrix
l <- 1
if(length(disease_trace_3_more20[,1]) > 0) {
  for(i in 1:length(disease_trace_3_more20[,1])) {
    trace_all[[l]] <- temp3[i,]  # Store each trajectory
    l <- l + 1
  }
}

# Build adjacency matrix for trajectory similarity
adjacency <- matrix(0, nrow = nrow(disease_trace_3_more20), ncol = nrow(disease_trace_3_more20))
colnames(adjacency) <- rownames(adjacency) <- char_D1_D2_D3_more20  # Use trajectory IDs as row/column names
for (i in 1:(nrow(disease_trace_3_more20)-1)) {
  trace1 <- trace_all[[i]]  # Get i-th trajectory
  for (j in (i+1):nrow(disease_trace_3_more20)) {
    trace2 <- trace_all[[j]]  # Get j-th trajectory
    # Calculate similarity: number of shared diseases between two trajectories
    adjacency[i,j] <- adjacency[j,i] <- sum(trace2 %in% trace1)
  }
}

# Remove isolated nodes (trajectories with no similarity to others)
del <- names(colSums(adjacency)[(colSums(adjacency)==0)])
dels <- which(colnames(adjacency)==del)
if(!identical(dels, integer(0))) adjacency <- adjacency[-dels,-dels]

# Set diagonal to trajectory length (3) since a trajectory is perfectly similar to itself
diag(adjacency) <- ncol(temp3)

# Transform adjacency values to similarity scores using Jaccard index with exponential decay
adjacencyexp <- ifelse(adjacency == ncol(temp3), 1, 
                       ifelse(adjacency == (ncol(temp3)-1), (ncol(temp3)-1)/(ncol(temp3)-1+2), 
                              ifelse(adjacency == (ncol(temp3)-2), (ncol(temp3)-2)/(ncol(temp3)-2+4), 0)))
adjacencyexp <- exp(-adjacencyexp)  # Convert to similarity via exponential decay

## First, use k-means to cluster into 2 groups, then extract the larger group and re-cluster recursively
set.seed(1234)  # For reproducibility
KMeans_result_undirect_0 <- RcmdrMisc::KMeans(adjacencyexp, centers = 2, iter.max = 100, 
                                              num.seeds = 10)  # Initial 2-cluster k-means
clust <- KMeans_result_undirect_0$cluster
A1 <- names(clust)[clust == 1]  # Trajectories in cluster 1
A2 <- names(clust)[clust == 2]  # Trajectories in cluster 2

# Hierarchical k-means clustering function with core detection
Hierarchical_KMeans1 <- function(A, adjacency, adjacencyexp, threshold = 15, tau1 = 0.1, tau2 = 1) {
  mins <- round(length(A) * tau1)  # Minimum number of connections for non-isolated nodes
  set.seed(1234)
  adjacency_A <- adjacency[A, A]  # Subset adjacency matrix to current cluster A
  mini <- min(colSums(adjacency_A == 0))  # Minimum number of zero connections
  logi <- mini < mins  # Check if any node has fewer connections than threshold
  
  # Extract sub-edges (pairs of consecutive diseases in trajectories)
  part1 <- substring(A, 1, 7)  # First two diseases (e.g., "D1_D2" from "D1_D2_D3")
  part2 <- substring(A, 5, 11)  # Last two diseases (e.g., "D2_D3" from "D1_D2_D3")
  subedge <- unique(c(part1, part2))  # Unique sub-edges
  
  # Build matrix indicating which trajectories contain each sub-edge
  M <- matrix(0, nrow = length(A), ncol = length(subedge))
  colnames(M) <- subedge
  rownames(M) <- A
  for (n in 1:length(subedge)) {
    # Check if trajectory contains the sub-edge (either as first two or last two diseases)
    M[,n] <- (grepl(substring(subedge, 1, 3)[n], A) | grepl(substring(subedge, 5, 7)[n], A))
  }
  
  # Identify hub sub-edges (present in most trajectories)
  hub <- names(which(colSums(M) >= length(A) * (1 - tau1)))
  
  # Identify core trajectories (those containing hub sub-edges)
  cores <- c()
  if(length(hub) > 0) {
    for (k in 1:length(hub)) {
      cores <- c(cores, which(grepl(hub[k], A)))  # Trajectories with hub sub-edges
    }
    cores <- unique(cores)
    names(cores) <- A[cores]
  }
  
  flag <- TRUE  # Flag to indicate if further clustering is needed
  
  # Remove isolated nodes from current cluster
  del <- names(colSums(adjacency_A)[(colSums(adjacency_A) - 3) == 0])  
  dels <- which(colnames(adjacency_A) == del)
  if(!identical(dels, integer(0))) {A <- A[-dels]}
  
  # Stop clustering if core trajectories dominate the cluster
  if(length(cores) >= length(A) * tau2) flag <- FALSE
  
  if(flag) {
    # Split into core and non-core trajectories
    A_kernal <- names(cores)  # Core trajectories
    A_rm <- setdiff(A, A_kernal)  # Non-core trajectories
    
    # Recursively cluster non-core trajectories
    KMeans_result <- RcmdrMisc::KMeans(adjacencyexp[A_rm, A_rm], centers = 2, iter.max = 100, 
                                       num.seeds = 10)
    A_rm_clust <- KMeans_result$cluster
    
    # Process first sub-cluster
    A1 <- names(A_rm_clust)[A_rm_clust == 1]
    cluster1 <- vector(mode = "list", length = 2); names(cluster1) <- c("nodes", "subcluster")
    cluster1$nodes <- A1
    # Recurse if sub-cluster size exceeds threshold
    if(length(A1) > threshold) {
      cluster1$subcluster <- Hierarchical_KMeans1(A1, adjacency, adjacencyexp, threshold = threshold, tau1 = tau1, tau2 = tau2)
    } else {
      cluster1$subcluster <- list("flag" = FALSE)
    }
    
    # Process second sub-cluster
    A2 <- names(A_rm_clust)[A_rm_clust == 2]
    cluster2 <- vector(mode = "list", length = 2); names(cluster2) <- c("nodes", "subcluster")
    cluster2$nodes <- A2
    if(length(A2) > threshold) {
      cluster2$subcluster <- Hierarchical_KMeans1(A2, adjacency, adjacencyexp, threshold = threshold, tau1 = tau1, tau2 = tau2)
    } else {
      cluster2$subcluster <- list("flag" = FALSE)
    }
    
    # Return clustering result
    result <- vector(mode = "list", length = 5)
    names(result) <- c("flag", "keynodes", "hub", "cluster1", "cluster2")
    result$flag <- flag
    result$keynodes <- A_kernal  # Core trajectories
    result$hub <- hub  # Hub sub-edges
    result$cluster1 <- cluster1
    result$cluster2 <- cluster2
  } else {
    # No further clustering needed
    result <- list("flag" = FALSE)
  }  
  return(result)
}

# Perform hierarchical clustering on initial k-means clusters
C1 <- Hierarchical_KMeans1(A1, adjacency, adjacencyexp, threshold = 15, tau1 = 0.1, tau2 = 1)
C2 <- Hierarchical_KMeans1(A2, adjacency, adjacencyexp, threshold = 15, tau1 = 0.1, tau2 = 1)
cluster1 <- list("nodes" = A1, "subcluster" = C1)
cluster2 <- list("nodes" = A2, "subcluster" = C2)
result <- list("flag" = TRUE, "keynodes" = NULL, "cluster1" = cluster1, "cluster2" = cluster2)

# Generate class codes for each trajectory based on clustering hierarchy
obtain_class_code <- function(A, cluster, level = NULL) {
  ## level1: C0, C1, C2 (0 = core, 1/2 = subclusters)
  ## level2: C10, C11, C12, C20, C21, C22 (nested subclusters)
  temp_func <- function(A, cluster) {
    class_of_A <- rep("", length(A)); names(class_of_A) <- A
    if(cluster$flag) {
      # Label core nodes with "0"
      if(!is.null(cluster$keynodes)) {
        class_of_A[cluster$keynodes] <- "0"
      }
      # Process first subcluster (label "1")
      if(is.null(cluster$cluster1)) warning("'cluster1' is NULL when 'flag' is true!")
      cluster1 <- cluster$cluster1$nodes
      class_of_A[cluster1] <- "1"
      class_of_A[cluster1] <- paste0(class_of_A[cluster1], temp_func(cluster1, cluster$cluster1$subcluster))
      # Process second subcluster (label "2")
      if(is.null(cluster$cluster2)) warning("'cluster2' is NULL when 'flag' is true!")
      cluster2 <- cluster$cluster2$nodes
      class_of_A[cluster2] <- "2"
      class_of_A[cluster2] <- paste0(class_of_A[cluster2], temp_func(cluster2, cluster$cluster2$subcluster))
      return(class_of_A)
    } else {
      return(class_of_A)
    }
  }
  class_code <- paste0("C", temp_func(A, cluster))  # Prefix with "C" for class
  names(class_code) <- A
  
  if(is.null(level)) return(class_code)
  # Truncate to specified hierarchy level
  class_code <- substr(class_code, 1, level + 1)
  return(class_code)
}

# Generate class codes for all trajectories
class <- obtain_class_code(A = colnames(adjacency), cluster = result)
sort(unique(class))  # Show unique class codes

# Organize trajectories into classes based on hierarchy level
obtain_class <- function(class_code, level = max(nchar(class_code)) - 1) {
  ## Truncate class codes to specified level
  class_code <- substr(class_code, 1, level + 1)
  class_code_uni <- sort(unique(class_code))
  
  # Identify core classes (ending with "0") and non-core classes
  temp <- nchar(class_code_uni)
  logi <- substr(class_code_uni, temp, temp) == "0"
  key_codes <- class_code_uni[logi]  # Core class codes
  class_code_true <- class_code_uni[!logi]  # Non-core class codes
  
  # Map core trajectories to their classes
  nodes_in_key <- vector(mode = 'list', length = length(key_codes))
  names(nodes_in_key) <- key_codes
  for(code in key_codes) {
    temp <- names(class_code)[class_code == code]
    char <- paste0("key_level", 1:level)[nchar(code) - 1]  # Label core level
    temp_type <- rep(char, length(temp))
    nodes_in_key[[code]] <- data.frame("nodes" = temp, "types" = temp_type, stringsAsFactors = FALSE)
  }
  
  # Map non-core trajectories to their classes
  nodes_in_true_class <- vector(mode = 'list', length = length(class_code_true))
  names(nodes_in_true_class) <- class_code_true
  for(code in class_code_true) {
    temp <- names(class_code)[class_code == code]
    temp_type <- rep("not_key", length(temp))  # Label non-core
    nodes_in_true_class[[code]] <- data.frame("nodes" = temp, "types" = temp_type, stringsAsFactors = FALSE)
  }
  
  # Merge core trajectories into their associated non-core classes
  for(code in key_codes) {
    code_rm0 <- substr(code, 1, nchar(code) - 1)  # Remove "0" suffix
    new_code <- code_rm0
    while(TRUE) {
      new_code <- c(paste0(new_code, 1), paste0(new_code, 2))  # Generate child class codes
      logi <- nchar(new_code) - 1 <= level  # Filter by level
      new_code <- new_code[logi]
      if(length(new_code) == 0) break
      logi <- new_code %in% class_code_true  # Check if child classes exist
      if(any(logi)) {
        for(code_treat in new_code[logi]) {
          # Merge core nodes into child class
          temp <- rbind(nodes_in_true_class[[code_treat]], nodes_in_key[[code]])
          nodes_in_true_class[[code_treat]] <- temp
        }
      }
      new_code <- new_code[!logi]
      if(length(new_code) == 0) break
    }
  }
  
  # Sort trajectories within each class
  for(i in 1:length(nodes_in_true_class)) {
    temp <- nodes_in_true_class[[i]]
    nodes_in_true_class[[i]] <- temp[order(temp[, 1]), ]
  }
  return(nodes_in_true_class)
}

# Generate classes at different hierarchy levels
class_level1 <- obtain_class(class_code = class, level = 1)
class_level2 <- obtain_class(class_code = class, level = 2)
class_level3 <- obtain_class(class_code = class, level = 3)
