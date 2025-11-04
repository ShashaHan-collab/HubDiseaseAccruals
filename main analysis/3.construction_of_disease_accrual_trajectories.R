rm(list = ls())
load("p_value and RR for binomial model_par_XBsep.RData")
superp = 5  ## Set the threshold for the number of people in trajectories
XB <- data[,2]
XB <- XB[temp_order]
for (XB_aim in 0:1) {
  txt1 <- paste0("D1_to_D2_XB", XB_aim, ".csv")
  txt2 <- paste0("D1_to_D2_XB", XB_aim, "Nsame_rm.csv")
  txt3 <- paste0("D1_to_D2_XB", XB_aim, "Nsame_sep.csv")
  rdataname1 <- paste0("XB", XB_aim, "_Diagnosis trajectories_OSTrm.RData")
  rdataname2 <- paste0("XB", XB_aim, "_Diagnosis trajectories_OSTrm_Nsame_rm.RData")
  rdataname3 <- paste0("XB", XB_aim, "_Diagnosis trajectories_OSTrm_Nsame_sep.RData")
  txt <- txt2; rdataname <- rdataname2
  D1_to_D2 <- read.csv(txt, header = T)
  D1_to_D2_order <- D1_to_D2[order(D1_to_D2[, 1]), ]
  num_D1_to_D2 <- rep(0, length(D1_to_D2_order[, 1]))
  names(num_D1_to_D2) <- paste0(D1_to_D2_order[, 1], "_", D1_to_D2_order[, 2])
  var_match_D1_to_D2 <- vector(mode = "list", length = length(D1_to_D2_order[, 1]))
  names(var_match_D1_to_D2) <- paste0(D1_to_D2_order[, 1], "_", D1_to_D2_order[, 2])
  num_zhuyuan <- table(table(var_match))
  
  ## D1_to_D2_single, D1_to_D2_double_uni
  for (i in 1:length(D1_to_D2_order[, 1])) {
    D1 <- D1_to_D2_order[i, 1]; D2 <- D1_to_D2_order[i, 2]
    ## D1 -> D2, D1 <- D2, D1 <-> D2
    index_contain_D1 <- all_index_get_disease[[D1]]
    index_contain_D2 <- all_index_get_disease[[D2]]
    index_D1 <- index[index_contain_D1]
    index_D2 <- index[index_contain_D2]
    var_match_D1 <- names(index_D1)  ## Patient IDs with D1
    var_match_D2 <- names(index_D2)  ## Patient IDs with D2
    ## Keep only patient IDs with both D1 and D2
    var_match_common <- intersect(var_match_D1, var_match_D2)
    var_match_common <- intersect(var_match_common, var_match[XB == XB_aim])
    N <- length(var_match_common)
    index_common_D1 <- index_D1[var_match_common]
    index_common_D2 <- index_D2[var_match_common]
    var_match_D1_to_D2[[i]] <- var_match_common[index_common_D1 < index_common_D2]
    num_D1_to_D2[i] <- N_D1 <- sum(index_common_D1 < index_common_D2)
  }
  
  ## Sort by quantity
  D1_to_D2_order_num <- D1_to_D2_order[order(-num_D1_to_D2), ]
  num_D1_to_D2 <- num_D1_to_D2[order(-num_D1_to_D2)]
  temp <- order(D1_to_D2_order_num[, 1])
  D1_to_D2_order_num <- D1_to_D2_order_num[temp, ]
  num_D1_to_D2 <- num_D1_to_D2[temp]
  var_match_D1_to_D2 <- var_match_D1_to_D2[paste0(D1_to_D2_order_num[, 1], "_", D1_to_D2_order_num[, 2])]
  logi <- num_D1_to_D2 >= superp
  D1_to_D2_order_num <- D1_to_D2_order_num[logi, ]
  num_D1_to_D2 <- num_D1_to_D2[logi]
  var_match_D1_to_D2 <- var_match_D1_to_D2[logi]
  
  
  ## Trajectory diagnosis
  #### Diagnosis trajectories of length 3
  trace_3 <- var_match_D1_D2_D3 <- vector(mode = "list", length = (length(D1_to_D2_order_num[, 1])^2))
  char_D1_D2_D3 <- rep("", length(D1_to_D2_order_num[, 1])^2)
  l <- 1
  for (i in 1:length(D1_to_D2_order_num[, 1])) {
    ## D1 -> D2
    D1 <- D1_to_D2_order_num[i, 1]; D2 <- D1_to_D2_order_num[i, 2]
    logi <- D1_to_D2_order_num[, 1] %in% D2
    D3_all <- D1_to_D2_order_num[logi, 2]
    if (length(D3_all) > 0) {
      char1 <- paste0(D1, "_", D2)
      var_match_temp1 <- var_match_D1_to_D2[[char1]]
      index_contain_D1 <- all_index_get_disease[[D1]]
      index_contain_D2 <- all_index_get_disease[[D2]]
      for (D3 in D3_all) {
        char2 <- paste0(D2, "_", D3)
        var_match_temp2 <- var_match_D1_to_D2[[char2]]
        index_contain_D3 <- all_index_get_disease[[D3]]
        var_match_common <- intersect(var_match_temp1, var_match_temp2)  ### Patients with D1, D2, and D3
        var_match_common <- intersect(var_match_common, var_match[XB == XB_aim])
        index_common_D1 <- index_contain_D1[var_match_common]  ### Index of first occurrence of D1 for each patient
        index_common_D2 <- index_contain_D2[var_match_common]  ### Index of first occurrence of D2 for each patient
        index_common_D3 <- index_contain_D3[var_match_common]  ### Index of first occurrence of D3 for each patient
        logi <- index_common_D1 < index_common_D2 & index_common_D2 < index_common_D3
        if (any(logi)) {
          N <- sum(logi)
          var_match_D1_D2_D3[[l]] <- var_match_common[logi]
          trace_3[[l]] <- temp <- data.frame("D1" = D1, "D2" = D2, "D3" = D3, "num" = N)
          char_D1_D2_D3[l] <- paste0(D1, "_", D2, "_", D3)
          l <- l + 1
        }
      }
    }
  }
  trace_3 <- trace_3[1:(l - 1)]; var_match_D1_D2_D3 <- var_match_D1_D2_D3[1:(l - 1)]
  char_D1_D2_D3 <- char_D1_D2_D3[1:(l - 1)]
  names(var_match_D1_D2_D3) <- char_D1_D2_D3
  disease_trace_3 <- do.call("rbind", trace_3)
  logi <- disease_trace_3[, 4] >= superp
  disease_trace_3_more20 <- disease_trace_3[logi, ]
  char_D1_D2_D3_more20 <- char_D1_D2_D3[logi]
  var_match_D1_D2_D3_more20 <- var_match_D1_D2_D3[logi]
  print(sum(logi))
  print(length(unique(c(disease_trace_3_more20$D1, disease_trace_3_more20$D2, disease_trace_3_more20$D3))))
  
  save.image(rdataname)
}
