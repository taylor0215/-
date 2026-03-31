type1=c("線性","boxcox","ph","aft","weibull","其他","其他2")
type2=c("小","中","大")
type3=c("無設限","輕設限","重設限")
for(v in 1:3){
  for(k in 4:4){
    for(j in 1:3){
      filename=paste0(type1[k],"區間",type2[j])
      censored=type3[v]
      path1=paste0("C:/Users/USER/Desktop/景鴻/0506/200/",censored,"/",filename,".csv")
      path2=paste0("C:/Users/USER/Desktop/景鴻/0506/300/",censored,"/",filename,".csv")
      lines1 <-read.csv(path1, encoding = "UTF-8")
      lines2=read.csv(path2, encoding = "UTF-8")
      cc=NULL
      ss=NULL
      aa=NULL
      for( i in 1:length(lines1$c_index)){
        cc[i]=ifelse(lines1$c_index[i]<lines2$c_index[i],1,0)
        ss[i]=ifelse(lines1$mse[i]>=lines2$mse[i],1,0)
        aa[i]=ifelse(lines1$mae[i]>=lines2$mae[i],1,0)
      }
      df <- data.frame("c_index_constant"=cc,"mse_constant"=ss,"mae_constant"=aa)
      #df=df[-1]
      rownames(df) <- c("IC_tree","ctree","middle_reg","middle_bagging","middle_randomforest","middle_boosting","middle_xgb","middle_bart","middle_ctree","middle_rpart","gam_reg","gam_bagging","gam_randomforest","gam_boosting","gam_xgb","gam_bart","gam_ctree","gam_rpart")
      path=paste0("C:/Users/USER/Desktop/景鴻/0506/constant/",censored,filename,".csv")
      write.csv(df,path)
    }
  }
}
