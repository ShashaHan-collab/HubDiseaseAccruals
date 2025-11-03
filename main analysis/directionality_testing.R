rm(list = ls())
load("p_value and RR for binomial model_par_XBsep.RData")


for(XB_aim in 0:1) {
  csvname_all <- paste0("D1_to_D2_XB", XB_aim, ".csv")
  csvname_Nsame_rm <- paste0("D1_to_D2_XB", XB_aim, "Nsame_rm.csv")
  csvname_Nsame_sep <- paste0("D1_to_D2_XB", XB_aim, "Nsame_sep.csv")
  all_index_get_disease <- get(paste0("all_index_get_disease_XB", XB_aim))
  p_value_binom <- get(paste0("p_value_binom_XB", XB_aim))
  RR_binom <- get(paste0("RR_binom_XB", XB_aim))
  
  logi <- RR_binom > 1 & p_value_binom <= 0.05
  #length(var_disease_code_chosen)
  #dim(RR_binom)
  temp1 <- matrix(var_disease_code_chosen, ncol = length(var_disease_code_chosen), nrow = length(var_disease_code_chosen))
  temp2 <- t(temp1)
  D1 <- temp1[logi]; D2 <- temp2[logi]## ------------- D1 -> D2
  D1_to_D2 <- data.frame("V1" = D1, "V2" = D2, stringsAsFactors = F)
  D2_to_D1 <- data.frame("V1" = D2, "V2" = D1, stringsAsFactors = F)
  temp <- rbind(D1_to_D2, D2_to_D1)
  temp1_temp2 <- paste(temp[,1], temp[,2], sep = "_")
  temp1_temp2_tab <- table(temp1_temp2)
  char <- names(temp1_temp2_tab) [temp1_temp2_tab > 1]
  logi <- paste(D1_to_D2[,1], D1_to_D2[,2], sep = "_") %in% char
  D1_to_D2_double <- D1_to_D2[logi,]
  D1_to_D2_single <- D1_to_D2[!logi,]
  logi <- D1_to_D2_double[,1] < D1_to_D2_double[,2]
  D1_to_D2_double_uni <- D1_to_D2_double[logi,]
  
  D1_to_D2_double_uni_list <- vector(mode = "list", length = length(D1_to_D2_double_uni[,1]))
  D1_to_D2_single_list <- vector(mode = "list", length = length(D1_to_D2_single[,1]))
  D1_to_D2_double_uni_Nsame_rm_list <- D1_to_D2_double_uni_Nsame_sep_list <- D1_to_D2_double_uni_list
  D1_to_D2_single_Nsame_rm_list <- D1_to_D2_single_Nsame_sep_list <- D1_to_D2_single_list
  
  ## D1_to_D2_single, D1_to_D2_double_uni
  for(i in 1:length(D1_to_D2_double_uni[,1])) {
    D1 <- D1_to_D2_double_uni[i,1]; D2 <- D1_to_D2_double_uni[i,2]
    ## D1 -> D2, D1 <- D2, D1 <-> D2
    index_contain_D1 <- all_index_get_disease[[D1]]
    index_contain_D2 <- all_index_get_disease[[D2]]
    index_D1 <- index[index_contain_D1]
    index_D2 <- index[index_contain_D2]
    var_match_D1 <- names(index_D1)##  患有D1的病人编号
    var_match_D2 <- names(index_D2)##  患有D2的病人编号
    ## 只保留同时患有D1，D2的病人编号
    var_match_common <- intersect(var_match_D1, var_match_D2)
    N <- length(var_match_common)
    index_common_D1 <- index_D1[var_match_common]
    index_common_D2 <- index_D2[var_match_common]
    N_D1 <- sum(index_common_D1 < index_common_D2)
    N_D2 <- sum(index_common_D1 > index_common_D2)
    N_same <- sum(index_common_D1 == index_common_D2)
    #### N_same 不特殊处理
    p_value_D1_to_D2 <- binom.test(N_D1, N, p = 0.5, alternative = "greater")$p.value
    p_value_D2_to_D1 <- binom.test(N_D2, N, p = 0.5, alternative = "greater")$p.value
    if(p_value_D1_to_D2 < 0.025) {
      D1_to_D2_double_uni_list[[i]] <- data.frame("D1" = D1, "D2" = D2, stringsAsFactors = F)
    } else if(p_value_D2_to_D1 < 0.025) {
      D1_to_D2_double_uni_list[[i]] <- data.frame("D1" = D2, "D2" = D1, stringsAsFactors = F)
    }
    #### N_same 特殊处理
    ## N_same 直接删除
    if(N_D1+N_D2 > 0) {
      p_value_D1_to_D2_Nsame_rm <- binom.test(N_D1, N_D1+N_D2, p = 0.5, alternative = "greater")$p.value
      p_value_D2_to_D1_Nsame_rm <- binom.test(N_D2, N_D1+N_D2, p = 0.5, alternative = "greater")$p.value
    } else {
      p_value_D1_to_D2_Nsame_rm <- p_value_D2_to_D1_Nsame_rm <- 1
    }
    if(p_value_D1_to_D2_Nsame_rm < 0.025) {
      D1_to_D2_double_uni_Nsame_rm_list[[i]] <- data.frame("D1" = D1, "D2" = D2, stringsAsFactors = F)
    } else if(p_value_D2_to_D1_Nsame_rm < 0.025) {
      D1_to_D2_double_uni_Nsame_rm_list[[i]] <- data.frame("D1" = D2, "D2" = D1, stringsAsFactors = F)
    }
    ## N_same 按比例分割
    if(N_D1+N_D2 > 0) {
      temp1 <- round(N_same * (N_D1 / (N_D1 + N_D2)), digits = 0)
      temp2 <- N_same - temp1
    } else {
      temp1 <- round(N_same * 0.5, digits = 0)
      temp2 <- N_same - temp1
    }
    N_D1 <- N_D1 + temp1
    N_D2 <- N_D2 + temp2
    p_value_D1_to_D2_Nsame_sep <- binom.test(N_D1, N, p = 0.5, alternative = "greater")$p.value
    p_value_D2_to_D1_Nsame_sep <- binom.test(N_D2, N, p = 0.5, alternative = "greater")$p.value
    if(p_value_D1_to_D2_Nsame_sep < 0.025) {
      D1_to_D2_double_uni_Nsame_sep_list[[i]] <- data.frame("D1" = D1, "D2" = D2, stringsAsFactors = F)
    } else if(p_value_D2_to_D1_Nsame_sep < 0.025) {
      D1_to_D2_double_uni_Nsame_sep_list[[i]] <- data.frame("D1" = D2, "D2" = D1, stringsAsFactors = F)
    }
  }
  
  for(i in 1:length(D1_to_D2_single[,1])) {
    D1 <- D1_to_D2_single[i,1]; D2 <- D1_to_D2_single[i,2]
    ## D1 -> D2, D1 <- D2, D1 <-> D2
    index_contain_D1 <- all_index_get_disease[[D1]]
    index_contain_D2 <- all_index_get_disease[[D2]]
    index_D1 <- index[index_contain_D1]
    index_D2 <- index[index_contain_D2]
    var_match_D1 <- names(index_D1)##  患有D1的病人编号
    var_match_D2 <- names(index_D2)##  患有D2的病人编号
    ## 只保留同时患有D1，D2的病人编号
    var_match_common <- intersect(var_match_D1, var_match_D2)
    N <- length(var_match_common)
    index_common_D1 <- index_D1[var_match_common]
    index_common_D2 <- index_D2[var_match_common]
    N_D1 <- sum(index_common_D1 < index_common_D2)
    N_D2 <- sum(index_common_D1 > index_common_D2)
    N_same <- sum(index_common_D1 == index_common_D2)
    #### N_same 不特殊处理
    p_value_D1_to_D2 <- binom.test(N_D1, N, p = 0.5, alternative = "greater")$p.value
    p_value_D2_to_D1 <- binom.test(N_D2, N, p = 0.5, alternative = "greater")$p.value
    if(p_value_D1_to_D2 < 0.025) {
      D1_to_D2_single_list[[i]] <- data.frame("D1" = D1, "D2" = D2, stringsAsFactors = F)
    } else if(p_value_D2_to_D1 < 0.025) {
      D1_to_D2_single_list[[i]] <- data.frame("D1" = D2, "D2" = D1, stringsAsFactors = F)
    }
    #### N_same 特殊处理
    ## N_same 直接删除
    if(N_D1+N_D2 > 0) {
      p_value_D1_to_D2_Nsame_rm <- binom.test(N_D1, N_D1+N_D2, p = 0.5, alternative = "greater")$p.value
      p_value_D2_to_D1_Nsame_rm <- binom.test(N_D2, N_D1+N_D2, p = 0.5, alternative = "greater")$p.value
    } else {
      p_value_D1_to_D2_Nsame_rm <- p_value_D2_to_D1_Nsame_rm <- 1
    }
    if(p_value_D1_to_D2_Nsame_rm < 0.025) {
      D1_to_D2_single_Nsame_rm_list[[i]] <- data.frame("D1" = D1, "D2" = D2, stringsAsFactors = F)
    } else if(p_value_D2_to_D1_Nsame_rm < 0.025) {
      D1_to_D2_single_Nsame_rm_list[[i]] <- data.frame("D1" = D2, "D2" = D1, stringsAsFactors = F)
    }
    ## N_same 按比例分割
    if(N_D1+N_D2 > 0) {
      temp1 <- round(N_same * (N_D1 / (N_D1 + N_D2)), digits = 0)
      temp2 <- N_same - temp1
    } else {
      temp1 <- round(N_same * 0.5, digits = 0)
      temp2 <- N_same - temp1
    }
    N_D1 <- N_D1 + temp1
    N_D2 <- N_D2 + temp2
    p_value_D1_to_D2_Nsame_sep <- binom.test(N_D1, N, p = 0.5, alternative = "greater")$p.value
    p_value_D2_to_D1_Nsame_sep <- binom.test(N_D2, N, p = 0.5, alternative = "greater")$p.value
    if(p_value_D1_to_D2_Nsame_sep < 0.025) {
      D1_to_D2_single_Nsame_sep_list[[i]] <- data.frame("D1" = D1, "D2" = D2, stringsAsFactors = F)
    } else if(p_value_D2_to_D1_Nsame_sep < 0.025) {
      D1_to_D2_single_Nsame_sep_list[[i]] <- data.frame("D1" = D2, "D2" = D1, stringsAsFactors = F)
    }
  }
  D1_to_D2_all <- do.call("rbind", c(D1_to_D2_double_uni_list, D1_to_D2_single_list))
  nodes <- sort(unique(c(D1_to_D2_all[,1], D1_to_D2_all[,2])))
  D1_to_D2_Nsame_rm_all <- do.call("rbind", c(D1_to_D2_double_uni_Nsame_rm_list, D1_to_D2_single_Nsame_rm_list))
  nodes_Nsame_rm <- sort(unique(c(D1_to_D2_Nsame_rm_all[,1], D1_to_D2_Nsame_rm_all[,2])))
  D1_to_D2_Nsame_sep_all <- do.call("rbind", c(D1_to_D2_double_uni_Nsame_sep_list, D1_to_D2_single_Nsame_sep_list))
  nodes_Nsame_sep <- sort(unique(c(D1_to_D2_Nsame_sep_all[,1], D1_to_D2_Nsame_sep_all[,2])))
  
  write.csv(D1_to_D2_all, csvname_all, row.names = F)
  write.csv(D1_to_D2_Nsame_rm_all, csvname_Nsame_rm, row.names = F)
  write.csv(D1_to_D2_Nsame_sep_all, csvname_Nsame_sep, row.names = F)
}
