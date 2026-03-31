type1=c("線性","boxcox","ph","aft","weibull","其他")
type2=c("小","中","大")
type3=c("無設限","輕設限","重設限")
type4=c("linear","boxcox","ph","aft","weibull","other")
type5=c("Small Interval","Medium Interval","Large Interval")
type6=c(200,300)

for(i in 1:6){
  for(j in 1:3){
    for(v in 1:2){
      path1=paste0("C:/Users/lll12/Desktop/景鴻/cd/excel",type6[v],"/無設限/",type1[i],"區間",type2[j],".csv")
      path2=paste0("C:/Users/lll12/Desktop/景鴻/cd/excel",type6[v],"/輕設限/",type1[i],"區間",type2[j],".csv")
      path3=paste0("C:/Users/lll12/Desktop/景鴻/cd/excel",type6[v],"/重設限/",type1[i],"區間",type2[j],".csv")
      df1=read.csv(path1,encoding = "UTF-8")
      df2=read.csv(path2,encoding = "UTF-8")
      df3=read.csv(path3,encoding = "UTF-8")
      #df2$c_index[1]
      
      
      
      content = paste0(
        "\\begin{table}[h!]\n",
        "\\centering\n",
        "\\scriptsize\n",
        "\\caption{",type4[i]," model ",type5[j]," with n=",type6[v],"}\n",
        "\\begin{tabular}{l|ccc|ccc|ccc}\n",
        "\\toprule\n",
        "Method & \\multicolumn{3}{c|}{no-censored} & \\multicolumn{3}{c}{light-censored} & \\multicolumn{3}{c}{heavy-censored} \\\\\n",
        "& C-index & MSE & MAE & C-index & MSE & MAE & C-index & MSE & MAE \\\\\n",
        "\\midrule\n",
        "IC\\_tree &", df1$c_index[1], "&", df1$mse[1], "&", df1$mae[1], "&",
        df2$c_index[1], "&", df2$mse[1], "&", df2$mae[1], "&",
        df3$c_index[1], "&", df3$mse[1], "&", df3$mae[1], "\\\\\n",
        
        "ctree &", df1$c_index[2], "&", df1$mse[2], "&", df1$mae[2], "&",
        df2$c_index[2], "&", df2$mse[2], "&", df2$mae[2], "&",
        df3$c_index[2], "&", df3$mse[2], "&", df3$mae[2], "\\\\\n",
        
        "Middle\\\\\n", 
        "regression &", df1$c_index[3], "&", df1$mse[3], "&", df1$mae[3], "&",
        df2$c_index[3], "&", df2$mse[3], "&", df2$mae[3], "&",
        df3$c_index[3], "&", df3$mse[3], "&", df3$mae[3], "\\\\\n",
        
        "bagging &", df1$c_index[4], "&", df1$mse[4], "&", df1$mae[4], "&",
        df2$c_index[4], "&", df2$mse[4], "&", df2$mae[4], "&",
        df3$c_index[4], "&", df3$mse[4], "&", df3$mae[4], "\\\\\n",
        
        "randomforest &", df1$c_index[5], "&", df1$mse[5], "&", df1$mae[5], "&",
        df2$c_index[5], "&", df2$mse[5], "&", df2$mae[5], "&",
        df3$c_index[5], "&", df3$mse[5], "&", df3$mae[5], "\\\\\n",
        
        "boosting &", df1$c_index[6], "&", df1$mse[6], "&", df1$mae[6], "&",
        df2$c_index[6], "&", df2$mse[6], "&", df2$mae[6], "&",
        df3$c_index[6], "&", df3$mse[6], "&", df3$mae[6], "\\\\\n",
        
        "xgboost &", df1$c_index[7], "&", df1$mse[7], "&", df1$mae[7], "&",
        df2$c_index[7], "&", df2$mse[7], "&", df2$mae[7], "&",
        df3$c_index[7], "&", df3$mse[7], "&", df3$mae[7], "\\\\\n",
        
        "bart &", df1$c_index[8], "&", df1$mse[8], "&", df1$mae[8], "&",
        df2$c_index[8], "&", df2$mse[8], "&", df2$mae[8], "&",
        df3$c_index[8], "&", df3$mse[8], "&", df3$mae[8], "\\\\\n",
        
        "ctree &", df1$c_index[9], "&", df1$mse[9], "&", df1$mae[9], "&",
        df2$c_index[9], "&", df2$mse[9], "&", df2$mae[9], "&",
        df3$c_index[9], "&", df3$mse[9], "&", df3$mae[9], "\\\\\n",
        
        "rpart &", df1$c_index[10], "&", df1$mse[10], "&", df1$mae[10], "&",
        df2$c_index[10], "&", df2$mse[10], "&", df2$mae[10], "&",
        df3$c_index[10], "&", df3$mse[10], "&", df3$mae[10], "\\\\\n",
        
        "XGB-GAM\\\\\n",
        "regression &", df1$c_index[11], "&", df1$mse[11], "&", df1$mae[11], "&",
        df2$c_index[11], "&", df2$mse[11], "&", df2$mae[11], "&",
        df3$c_index[11], "&", df3$mse[11], "&", df3$mae[11], "\\\\\n",
        
        "bagging &", df1$c_index[12], "&", df1$mse[12], "&", df1$mae[12], "&",
        df2$c_index[12], "&", df2$mse[12], "&", df2$mae[12], "&",
        df3$c_index[12], "&", df3$mse[12], "&", df3$mae[12], "\\\\\n",
        
        "randomforest &", df1$c_index[13], "&", df1$mse[13], "&", df1$mae[13], "&",
        df2$c_index[13], "&", df2$mse[13], "&", df2$mae[13], "&",
        df3$c_index[13], "&", df3$mse[13], "&", df3$mae[13], "\\\\\n",
        
        "boosting &", df1$c_index[14], "&", df1$mse[14], "&", df1$mae[14], "&",
        df2$c_index[14], "&", df2$mse[14], "&", df2$mae[14], "&",
        df3$c_index[14], "&", df3$mse[14], "&", df3$mae[14], "\\\\\n",
        
        "xgboost &", df1$c_index[15], "&", df1$mse[15], "&", df1$mae[15], "&",
        df2$c_index[15], "&", df2$mse[15], "&", df2$mae[15], "&",
        df3$c_index[15], "&", df3$mse[15], "&", df3$mae[15], "\\\\\n",
        
        "bart &", df1$c_index[16], "&", df1$mse[16], "&", df1$mae[16], "&",
        df2$c_index[16], "&", df2$mse[16], "&", df2$mae[16], "&",
        df3$c_index[16], "&", df3$mse[16], "&", df3$mae[16], "\\\\\n",
        
        "ctree &", df1$c_index[17], "&", df1$mse[17], "&", df1$mae[17], "&",
        df2$c_index[17], "&", df2$mse[17], "&", df2$mae[17], "&",
        df3$c_index[17], "&", df3$mse[17], "&", df3$mae[17], "\\\\\n",
        
        "rpart &", df1$c_index[18], "&", df1$mse[18], "&", df1$mae[18], "&",
        df2$c_index[18], "&", df2$mse[18], "&", df2$mae[18], "&",
        df3$c_index[18], "&", df3$mse[18], "&", df3$mae[18], "\\\\\n",
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "\\label{tab:performance_comparison}\n",
        "\\end{table}\n"
      )
      
      # 查看輸出
      #cat(content)
      output_path=paste0("C:/Users/lll12/Desktop/景鴻/cd/資料填寫程式碼/",type1[i],type2[j],"n",type6[v],".txt")
      write.table(content,output_path)
    }
  } 
}  
