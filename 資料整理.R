type1=c("線性","boxcox","ph","aft","weibull","其他","其他2")
type2=c("小","中","大")
type3=c("無設限","輕設限","重設限")
for(v in 1:3){
  for(k in 4:4){
    for(j in 1:3){
    filename=paste0(type1[k],"區間",type2[j])
    censored=type3[v]
    path1=paste0("C:/Users/USER/Desktop/景鴻/0506/300/",censored,"/",filename,".txt")
    lines <- readLines(path1, encoding = "UTF-8")
    #head(lines, 1)
    grep("cindex", lines, value = TRUE)
    # 找欄位名的行
    idx <- grep("cindex", lines)
    
    # 取出欄位名稱與對應數值行
    headers <- lines[idx]
    values <- lines[idx + 1]
    
    # 拆成欄位格式
    split_headers <- strsplit(headers, "\\s+")
    split_headers=lapply(split_headers, function(x) (x[x != ""]))
    split_values <- strsplit(values, "\\s+")
    split_values <- lapply(split_values, function(x) as.numeric(x[x != ""]))
    
    # 合併成 dataframe
    results <- do.call(rbind, lapply(seq_along(split_headers), function(i) {
      setNames(as.numeric(split_values[[i]]), split_headers[[i]])
    }))
    
    # 變成 dataframe
    df <- as.data.frame(results)
    #df=df[-1]
    rownames(df) <- c("IC_tree","ctree","middle_reg","middle_bagging","middle_randomforest","middle_boosting","middle_xgb","middle_bart","middle_ctree","middle_rpart","gam_reg","gam_bagging","gam_randomforest","gam_boosting","gam_xgb","gam_bart","gam_ctree","gam_rpart")
    colnames(df)=c("c_index","mse","mae")
    path=paste0("C:/Users/USER/Desktop/景鴻/0506/300/",censored,"/",filename,".csv")
    write.csv(df,path)
    }
  }
}