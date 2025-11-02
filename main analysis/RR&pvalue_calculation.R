library(parallel)

### 将多次住院经历合并
var_disease_code_list_combine_uni <- vector(mode = "list", length = length(unique(var_match)))
names(var_disease_code_list_combine_uni) <- unique(var_match)
var_match_tab <- table(var_match)
logi <- var_match_tab >= 2
var_match_once <- names(var_match_tab) [!logi]
var_match_more <- names(var_match_tab) [logi]
## 单次住院不变
index_once <- index[var_match_once]
var_disease_code_list_combine_uni[var_match_once] <- var_disease_code_list_uni[index_once]
## 多次住院合并
for(i in 1:length(var_match_more)) {
  var_match_more_i <- var_match_more[i]
  index_more_i <- index[names(index) %in% var_match_more_i]
  temp <- sort(unique(do.call("c", var_disease_code_list_uni[index_more_i])))
  var_disease_code_list_combine_uni[[var_match_more_i]] <- temp
}

### 考虑有单次和多次住院经历的病人的疾病代码出现频率
var_disease_code_lib_onceANDmore <- 
  sort(unique(do.call("c", var_disease_code_list_combine_uni)))## 疾病代码库
var_disease_code_tab_onceANDmore <- 
  table(do.call("c", var_disease_code_list_combine_uni))## 疾病代码出现频率
logi <- var_disease_code_tab_onceANDmore >= 100 ## 考虑所有病人中曾患过的次数不少于100次的疾病代码
var_disease_code_chosen <- names(var_disease_code_tab_onceANDmore) [logi]
var_disease_code_chosen <- var_disease_code_chosen[!grepl("^BN", var_disease_code_chosen)]
logi <- (var_disease_code_chosen >= "S00" & var_disease_code_chosen <= "T98") |
  (var_disease_code_chosen >= "V01" & var_disease_code_chosen <= "Y98")
d <- length(var_disease_code_chosen)
RR <- matrix(0, d, d); colnames(RR) <- rownames(RR) <- var_disease_code_chosen
p_value_binom <- p_value <- RR_binom <- RR

get_date <- function(x) {
  x <- paste0(substr(x, 1, 4), "-", substr(x, 5, 6), "-", substr(x, 7, 8))
  as.Date(x)
}

############################ 预处理：创建索引 与 匹配 #################################
RY_date <- get_date(var_time) ## 入院日期
## 将入院时间按月份分为12部分
#RY_month <- months.Date(RY_date)
RY_quarters <- quarters(RY_date)
RY_year <- substr(var_time, 1, 4)##  2013 ~ 2021
XB <- var_num[, 2] ## 性别
NL <- var_num[, 3] ## 年龄
## 将年龄分为4部分： 新生儿（NL == 0），未成年（0 < NL <= 19），青年（20 <= NL < 55），中老年（NL >= 55）
NL <- 0 * (NL == 0) + 1 * (NL > 0 & NL <= 19) + 2 * (NL >= 20 & NL < 55) + 3 * (NL >= 55)

## 有哪些入院记录中会患 对应的病
all_index_get_disease <- vector("list", d); names(all_index_get_disease) <- var_disease_code_chosen
for(code in var_disease_code_chosen) {
  logi <- do.call("c", lapply(var_disease_code_list_uni, function(x, code) {code %in% x}, code = code))
  all_index_get_disease[[code]] <- index[logi]
}

## 同一位病人5年内的入院记录 索引
all_index_within_5 <- vector("list", length(var_match)); names(all_index_within_5) <- var_match
var_match_tab <- table(var_match)
temp <- names(var_match_tab) [var_match_tab > 1]
for(i in which(var_match %in% temp)) { times<-times+1;print(times)
logi_temp_i <- var_match == var_match[i] & as.numeric(RY_date - RY_date[i]) >= 0 &
  as.numeric(RY_date - RY_date[i]) <= 365 * 5
all_index_within_5[[i]] <- index[logi_temp_i]
}
temp <- names(var_match_tab) [var_match_tab == 1]
all_index_within_5[temp] <- as.list(index[temp])

## 为每一条入院记录先做好匹配
all_index_matched <- vector("list", length(var_match))
#char_month <- c("一月", "二月", "三月", "四月", "五月", "六月", "七月", "八月", "九月", 
#                "十月", "十一月", "十二月")
char_quarters <- c("Q1", "Q2", "Q3", "Q4")
char_year <- sort(unique(RY_year))
#char_group <- matrix("", nrow = length(char_year) * 12 * 2 * length(unique(NL)), ncol = 4)
char_group <- matrix("", nrow = length(char_year) * 4 * 2 * length(unique(NL)), ncol = 4)
#colnames(char_group) <- c("year", "month", "sex", "age")
colnames(char_group) <- c("year", "quarters", "sex", "age")
group_num <- 1; group <- rep(0, length(var_match))
for(i_year in char_year) {
  #for(i_month in char_month) {
  for(i_quarters in char_quarters) {
    for(i_XB in 0:1) {
      for(i_NL in sort(unique(NL))) {
        #logi_temp_i <- RY_year == i_year & RY_month == i_month & XB == i_XB & NL == i_NL
        logi_temp_i <- RY_year == i_year & RY_quarters == i_quarters & XB == i_XB & NL == i_NL
        if(!any(logi_temp_i)) next
        group[logi_temp_i] <- group_num
        #char_group[group_num, ] <- c(i_year, i_month, i_XB, i_NL)
        char_group[group_num, ] <- c(i_year, i_quarters, i_XB, i_NL)
        group_num <- group_num + 1; print(group_num)
      }
    }
  }
}
char_group <- char_group[1:(group_num-1), ]
group_tab <- table(group); group_tab; min(group_tab) # 一组至少有 1 条住院记录
write.csv(cbind(table(group), char_group), "匹配后的组数及每组的住院记录数.csv", fileEncoding = "utf8")

##  logi <- group_tab >= 20; 

Pr_D2_all_group <- matrix(0, nrow = length(unique(group)), ncol = length(var_disease_code_chosen))
colnames(Pr_D2_all_group) <- var_disease_code_chosen
for(i_group in sort(unique(group))) {
  index_i <- which(group == i_group)
  n_match_i <- length(index_i) ## n_Match
  ## 利用上述样本获取 Pr(D2)
  Pr_D2_i <- rep(0, length(var_disease_code_chosen))
  names(Pr_D2_i) <- var_disease_code_chosen
  for(j in 1:n_match_i) {
    c_i_D2 <- rep(0, length(var_disease_code_chosen)) ## 上述匹配样本中，5年内患有D2的数量
    names(c_i_D2) <- var_disease_code_chosen
    ## 病人在 5年内 患有D2，则 计数+1
    index_j <- all_index_within_5[[index_i[j]]]
    for(D2 in var_disease_code_chosen) {
      c_i_D2[D2] <- c_i_D2[D2] + any(index_j %in% all_index_get_disease[[D2]])
    }
    Pr_D2_i <- Pr_D2_i + c_i_D2
  }
  Pr_D2_all_group[i_group, ] <- Pr_D2_i / n_match_i
  print(i_group)
}

save.image("match_group.RData")

############################ get RR and p_value #################################
main <- function(D1, var_disease_code_chosen, all_index_get_disease, all_index_within_5, group, 
                 Pr_D2_all_group, index) {
  tryCatch(
    {
      ## 获取所有患有 D1 的样本 （exposed group）
      index_contain_D1 <- all_index_get_disease[[D1]]
      if(is.null(index_contain_D1) | length(index_contain_D1) == 0) {
        p_value_binom_D1 <- rep(-1, length(var_disease_code_chosen)); names(p_value_binom_D1) <- var_disease_code_chosen
        RR_binom_D1 <- rep(-1, length(var_disease_code_chosen)); names(RR_binom_D1) <- var_disease_code_chosen
        return(list("p_value" = p_value_binom_D1, "RR" = RR_binom_D1))
      }
      n_discharges <- length(index_contain_D1) ## 患有 D1 的样本量
      index_D1 <- index[index_contain_D1]
      C_exposed_D2 <- rep(0, length(var_disease_code_chosen) - 1) ## C_exposed
      names(C_exposed_D2) <- setdiff(var_disease_code_chosen, D1)
      Pr_D2_test <- rep(0, length(var_disease_code_chosen) - 1)   ## Pr(D2)_test
      names(Pr_D2_test) <- setdiff(var_disease_code_chosen, D1)
      for(i in 1:n_discharges) {
        ## 暴露组 第i个样本, 5年内是否患有D2
        index_temp_i <- all_index_within_5[[index_D1[i]]]
        for(D2 in setdiff(var_disease_code_chosen, D1)) {
          C_exposed_D2[D2] <- C_exposed_D2[D2] + any(index_temp_i %in% all_index_get_disease[[D2]])
        }
      }
      
      ## 将 n_discharges 按 group 分别计算
      group_contain_D1 <- group[index_contain_D1]
      group_count <- rep(0, length(unique(group)))
      names(group_count) <- as.character(1:length(unique(group)))
      group_tab <- table(group_contain_D1)
      group_count[names(group_tab)] <- as.numeric(group_tab)
      group_count_matrix <- matrix(group_count, nrow = length(group_count), 
                                   ncol = length(var_disease_code_chosen) - 1)
      Pr_D2_test <- colSums(group_count_matrix / n_discharges * 
                              Pr_D2_all_group[, setdiff(var_disease_code_chosen, D1)])
      
      p_value_binom_D1 <- rep(0, length(var_disease_code_chosen)); names(p_value_binom_D1) <- var_disease_code_chosen
      for(D2 in setdiff(var_disease_code_chosen, D1)) {
        p_value_binom_D1[D2] <- binom.test(C_exposed_D2[D2], n_discharges, p = Pr_D2_test[D2], 
                                           alternative = "greater")$p.value
      }
      RR_binom_D1 <- rep(0, length(var_disease_code_chosen)); names(RR_binom_D1) <- var_disease_code_chosen
      RR_binom_D1[setdiff(var_disease_code_chosen, D1)] <- 
        C_exposed_D2 / (n_discharges * Pr_D2_test)
      
      result <- list("p_value" = p_value_binom_D1, "RR" = RR_binom_D1)
      return(result)
    }, error = function(e) {
      p_value_binom_D1 <- rep(-1, length(var_disease_code_chosen)); names(p_value_binom_D1) <- var_disease_code_chosen
      RR_binom_D1 <- rep(-1, length(var_disease_code_chosen)); names(RR_binom_D1) <- var_disease_code_chosen
      return(list("p_value" = p_value_binom_D1, "RR" = RR_binom_D1))
    }, warning = function(w) {
      p_value_binom_D1 <- rep(-1, length(var_disease_code_chosen)); names(p_value_binom_D1) <- var_disease_code_chosen
      RR_binom_D1 <- rep(-1, length(var_disease_code_chosen)); names(RR_binom_D1) <- var_disease_code_chosen
      return(list("p_value" = p_value_binom_D1, "RR" = RR_binom_D1))
    }
  )
}


#################################### 按性别分开处理 ####################################
d <- length(var_disease_code_chosen)
RR <- matrix(0, d, d); colnames(RR) <- rownames(RR) <- var_disease_code_chosen
p_value_binom_XB0 <- RR_binom_XB0 <- p_value_binom_XB1 <- RR_binom_XB1 <- RR
index_XB1 <- index[XB == 1]
index_XB0 <- index[XB == 0]
all_index_get_disease_XB0 <- lapply(all_index_get_disease, FUN = intersect, y = index_XB0)
all_index_get_disease_XB1 <- lapply(all_index_get_disease, FUN = intersect, y = index_XB1)


### XB = 0
t1 <- Sys.time()
cl <- makeCluster(2)
a <- parallel::parLapply(cl=cl, var_disease_code_chosen, main, var_disease_code_chosen = var_disease_code_chosen, 
                         all_index_get_disease = all_index_get_disease_XB0, 
                         all_index_within_5 = all_index_within_5, group = group, 
                         Pr_D2_all_group = Pr_D2_all_group, index = index)
stopCluster(cl); gc()
t2 <- Sys.time(); t2-t1

for(i in 1:length(var_disease_code_chosen)) {
  D1 <- var_disease_code_chosen[i]
  p_value_binom_XB0[D1, ] <- a[[i]] [["p_value"]]
  RR_binom_XB0[D1, ] <- a[[i]] [["RR"]]
}
remove("a")

### XB = 1
t1 <- Sys.time()
cl <- makeCluster(2)
a <- parallel::parLapply(cl=cl, var_disease_code_chosen, main, var_disease_code_chosen = var_disease_code_chosen, 
                         all_index_get_disease = all_index_get_disease_XB1, 
                         all_index_within_5 = all_index_within_5, group = group, 
                         Pr_D2_all_group = Pr_D2_all_group, index = index)
stopCluster(cl); gc()
t2 <- Sys.time(); t2-t1

for(i in 1:length(var_disease_code_chosen)) {
  D1 <- var_disease_code_chosen[i]
  p_value_binom_XB1[D1, ] <- a[[i]] [["p_value"]]
  RR_binom_XB1[D1, ] <- a[[i]] [["RR"]]
}
remove("a")

save.image("p_value and RR for binomial model_par_XBsep.RData")
