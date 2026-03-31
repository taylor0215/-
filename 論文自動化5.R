auto=function(gg,n,beta,p,min,max,k,cd,type){
  #套件集
  library(truncnorm)
  library(flexsurv)
  library(LTRCtrees)
  library(survival)
  library(partykit)
  library(icenReg)
  library(mgcv)
  library(car)
  library(mice)
  library(tree)
  library(e1071)
  library(pec)
  #library(caret)
  library(randomForest)
  library(gbm)
  library(xgboost)
  library(BART)
  library(party)
  library(rpart)
  library(stats)
  library(stats)
  library(fitdistrplus)
  library(ICcforest)
  if(type %in%c("linear","boxcox","ph")){
    if(type=="nothing"){
      return("nothing")
    }else if(type=="linear"){
      generate_t=function(n,x1,x2,x3,x4){
        #theta=-(cos((x1+x2)*pi)+sqrt(x1+x2))
        #theta=beta1*x1+beta2*x2+beta3*x3+beta4*x4
        t1=beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,2,1)#線性ok k=30
        t2=beta0+(0.5*(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5))+1)^2#box_cox trainsfer
        t3=beta0+log(exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5)))#ph model ok k=30
        t4=NULL
        for (jj in 1:n) {
          t4[jj]=beta0+min(beta1*x1[jj],beta2*x2[jj],beta3*x3[jj],beta4*x4[jj])+rnorm(1,3,0.5)#複雜模型
        }
        t5=beta0+rexp(n,1.5)*exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4)
        t6=beta0+(beta1*x1+beta2*sin(x2))^0.5+beta3*x3^2+beta4*x2*x3#複雜模型
        return(list(t=t1))
      }
    }else if(type=="boxcox"){
      generate_t=function(n,x1,x2,x3,x4){
        #theta=-(cos((x1+x2)*pi)+sqrt(x1+x2))
        #theta=beta1*x1+beta2*x2+beta3*x3+beta4*x4
        t1=beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,2,1)#線性ok k=30
        t2=beta0+(0.5*(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5))+1)^2#box_cox trainsfer
        t3=beta0+log(exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5)))#ph model ok k=30
        t4=NULL
        for (jj in 1:n) {
          t4[jj]=beta0+min(beta1*x1[jj],beta2*x2[jj],beta3*x3[jj],beta4*x4[jj])+rnorm(1,3,0.5)#複雜模型
        }
        t5=beta0+rexp(n,1.5)*exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4)
        t6=beta0+(beta1*x1+beta2*sin(x2))^0.5+beta3*x3^2+beta4*x2*x3#複雜模型
        return(list(t=t2))
      }
    }else if(type=="ph"){
      generate_t=function(n,x1,x2,x3,x4){
        #theta=-(cos((x1+x2)*pi)+sqrt(x1+x2))
        #theta=beta1*x1+beta2*x2+beta3*x3+beta4*x4
        t1=beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,2,1)#線性ok k=30
        t2=beta0+(0.5*(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5))+1)^2#box_cox trainsfer
        t3=beta0+log(exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5))+1)#ph model ok k=30
        t4=NULL
        for (jj in 1:n) {
          t4[jj]=beta0+min(beta1*x1[jj],beta2*x2[jj],beta3*x3[jj],beta4*x4[jj])+rnorm(1,3,0.5)#複雜模型
        }
        t5=beta0+rexp(n,1.5)*exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4)
        t6=beta0+(beta1*x1+beta2*sin(x2))^0.5+beta3*x3^2+beta4*x2*x3#複雜模型
        return(list(t=t3))
      }
    }
    mae_reg=NULL
    mae_LTR=NULL
    df_pred_r=NULL
    mse_LTR=NULL
    mse_reg=NULL
    cindex_reg=NULL
    cindex_LTR=NULL
    cindex_bag=NULL
    mse_bag=NULL
    mae_bag=NULL
    cindex_rf=NULL
    mse_rf=NULL
    mae_rf=NULL
    cindex_boost=NULL
    mse_boost=NULL
    mae_boost=NULL
    cindex_xgb=NULL
    mse_xgb=NULL
    mae_xgb=NULL
    cindex_ctree=NULL
    mse_ctree=NULL
    mae_ctree=NULL
    cindex_rpart=NULL
    mse_rpart=NULL
    mae_rpart=NULL
    bart_cindex=NULL
    bart_mse= NULL 
    bart_mae= NULL
    cindex_rpart= NULL
    mse_rpart= NULL
    mae_rpart= NULL
    
    
    cindex_reg_norm=NULL
    mse_reg_norm=NULL
    mae_reg_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_boost_norm=NULL
    cindex_rf_norm=NULL
    mse_rf_norm=NULL
    mae_rf_norm=NULL
    mse_boost_norm=NULL
    mae_boost_norm=NULL
    bart_cindex_norm=NULL
    bart_mse_norm=NULL
    bart_mae_norm=NULL
    cindex_xgb_norm=NULL
    mse_xgb_norm=NULL
    mae_xgb_norm=NULL
    cindex_ctree_norm=NULL
    mse_ctree_norm=NULL
    mae_ctree_norm=NULL
    cindex_rpart_norm= NULL
    mse_rpart_norm= NULL
    mae_rpart_norm= NULL
    
    cindex_ctree_middle=NULL
    mse_ctree_middle=NULL
    mae_ctree_middle=NULL
    
    mse_c=NULL
    mae_c=NULL
    cindex_c=NULL
    rate=NULL
    ii=NULL
    #################
    generate_censoring = function(t, k, censoring_rate) {
      #exam_times =seq(0,quantile(t,0.9), length.out = k + 1)
      delta_t =runif(k, min =min, max =max)  # 從 U[0.3, 0.7] 中取值
      tt=0
      exam_times=NULL
      for (i in 1:(k+1)){
        exam_times[i]=tt
        tt=tt+delta_t[i]
      }
      L =sapply(t, function(t) max(exam_times[exam_times <= t]))
      #R=L+delta_t
      R = exam_times[sapply(L,function(x)which(exam_times==x))+1]
      R[is.na(R)]=Inf
      # 將部分觀測值設置為右設限，根據指定比例 censoring_rate
      right_censor <- runif(n) < censoring_rate
      R[right_censor] <-Inf  # 設定為右設限
      return(list(L = L, R = R,status=as.integer(L<t&t<R&R!=Inf)))  # status 表示是否觀測到事件
    }
    ##################
    i=1
    seed_number=i
    censoring_rate =0#突然失聯的人比例
    now=proc.time()
    while (i<=gg){
      #Sys.sleep(1.5)
      set.seed(seed_number+80)
      #set.seed(85)
      #生成xi
      #x1 <- sample(0:1,n,replace=TRUE)
      #x2 <- sample(0:2,n,replace=TRUE)
      #x3 <- sample(0:1,n,replace=TRUE)
      #x4 <- sample(0:4,n,replace=TRUE)
      x1 <- rnorm(n, 4.6, 1)  
      x2 <- sample(seq(0,3,by=0.1),n,replace = TRUE) 
      x3 <- rpois(n,3)
      x4 <- rbinom(n, size = 1, prob = 0.5)
      # 設定係數 beta
      beta0=beta[1]
      beta1= beta[2]
      beta2 = beta[3]
      beta3 = beta[4]
      beta4 = beta[5]
      #製作資料集
      t=generate_t(n,x1,x2,x3,x4)
      censoring= generate_censoring(t$t, k, censoring_rate) 
      d1=data.frame(t=t$t,d=censoring$status,L=censoring$L,R=censoring$R,x1,x2,x3,x4)
      rate[i]=1-(sum(d1$d)/length(d1$L))
      print(type)
      print(i)
      #檢驗資料是否符合coxph assumtion
      #summary(coxph(Surv(t,d) ~ x1+x2+x3+x4, d1)) 
      #sur=survfit(Surv(t,d)~ x1+x2+x3+x4, data=d1)
      #plot(sur)
      #plot(t$t)
      #分組
      s=sample(1:n,0.7*n)
      dtrain=d1[s,]
      dtest=d1[-s,]
      control_params <- partykit::ctree_control(
        minsplit = 20,
        minbucket =5,
        mincriterion = 0.7,
        mtry = 2
      )
      fit=ICtree(Surv(L,R,type="interval2")~x1+x2+x3+x4,dtrain, Control = control_params)
      #plot(fit)
      LTR_pred1=predict(fit, newdata = dtest, type="response")
      LTR_pred2=predict(fit, newdata = dtest, type="prob")
      if(mean((dtest$t-LTR_pred1)^2)!=Inf){
        #plot(fit)
        mse_LTR[i]=mean((dtest$t-LTR_pred1)^2)
        mae_LTR[i]=mean(abs(dtest$t-LTR_pred1))
        cindex_LTR[i]=concordance(t ~ LTR_pred1 , data = dtest)$concordance
        #cat(mse[i])
        #usedtime2[i]=proc.time()-now1
      } else{
        next
      }
      #計算時間
      #now=proc.time()
      #估計參數ic_sp
      cox_fit=ic_sp(Surv(L,R,type = "interval2")~x1+x2+x3+x4,data=dtrain,model = "ph")
      summary(cox_fit)
      b1=cox_fit$coefficients[1]
      b2=cox_fit$coefficients[2]
      b3=cox_fit$coefficients[3]
      b4=cox_fit$coefficients[4]
      c(b1,b2,b3,b4)
      #b5=cox_fit$coefficients[5]
      #b6=cox_fit$coefficients[6]
      #plot(cox_fit)
      x_beta=b1*dtrain$x1+b2*dtrain$x2+b3*dtrain$x3+b4*dtrain$x4
      #x_beta=dtrain$x1+dtrain$x2+dtrain$x3+dtrain$x4
      #x_beta=predict(cox_fit,newdata=dtrain)
      dtrain$x=x_beta
      df_1=subset(dtrain,d==1)
      pred_t=(df_1$L+df_1$R)/2
      df_1$pred_t=pred_t
      df_pred=subset(dtrain,d==0)
      mm=min(df_1$R-df_1$L)
      ma=max(df_1$R-df_1$L)
      m=runif(length(df_pred$t),mm,ma)
      r=range(dtrain$L)[2]-range(dtrain$L)[1]
      gam_l=tryCatch({
        gam(L~s(x,bs="tp"),data=df_1,method = "REML")
      },error=function(e){
        gam_l=NULL
      })
      if(length(gam_l)==0){
        seed_number=seed_number+1
        next
      }
      #gam_r=gam(R~s(x,bs="tp"),data=df_1,method = "REML")
      #gam_t=gam(ht~s(x,bs="tp"),data=df_1,method = "REML")
      #plot(gam_l)
      #plot(gam_r)
      pred_L=predict(gam_l,data.frame(x=df_pred[,"x"]))
      pred_L=pmax(pred_L,df_pred$L)
      #pred_R=predict(gam_r,data.frame(x=df_pred[,"x"]))
      #pred_t=predict(gam_t,data.frame(x=df_pred[,"x"]))
      pred_R=pred_L+m
      df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8])
      if(length(df_pred_r$L)>0){
        df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8],x=0)
      }
      dtrain_1=rbind(df_1[1:9],df_pred_r)
      
      #############################################
      dtrain_2=dtrain_1
      dtrain_2_int=dtrain_2[dtrain_2$d==1,]
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      ###########################################
      #區間的插補
      p=150
      #cforest_pred1=matrix(0,length(dtest$L),p)
      pred_t_n=matrix(0,length(dtrain_2_int$L),p)
      st=NULL
      for(r in 1:length(dtrain_2_int$L)){
        st[r]=runif(1,dtrain_2_int$L[r],dtrain_2_int$R[r])
      }
      for (j in 1:p){
        pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=mean((dtrain_2_int$L+dtrain_2_int$R)/2),sd=sd(st)/3)
        #Cforest <- ICcforest(Surv(L,R,type="interval2")~x1+x2+x3+x4,dtrain_2, Control = control_params)
        #cforest_pred1[,j]=predict(Cforest,dtest, type="response")
      }
      pred_t_norm=rowMeans(pred_t_n)
      dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int)
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      tt=300
      dtrain_2predt=matrix(0,nrow =length(dtrain_2_int$L),ncol = 50 )
      #cforest_pred=rowMeans(cforest_pred1)
      dd=1
      while (dd<cd) {
        pred_t_norm=rowMeans(pred_t_n)
        dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int[-1])
        pred_t2=matrix(0,length(dtrain_2_int$L),tt)
        xgb_norm = xgboost(data = as.matrix(dtrain_2_int [,c(6:9)]),label = as.matrix(dtrain_2_int[,"pred_t"]),nrounds = 200,objective = "reg:squarederror",mtry=1000,verbose = 0)
        xgb_pred_t_norm = predict(xgb_norm,as.matrix(dtrain_2_int[,c(6:9)]))
        pred_t0=ifelse((dtrain_2_int$R>xgb_pred_t_norm&xgb_pred_t_norm>dtrain_2_int$L),xgb_pred_t_norm,dtrain_2_int$pred_t)
        dtrain_2_int$pred_t=pred_t0
        a=1
        while(a<tt){
          
          #cat(dd,a)
          jj=sample(1:length(dtrain_2_int$L),length(dtrain_2_int$L))
          dt=dtrain_2_int[jj,]
          dpred=dtrain_2_int[jj,]
          gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
          pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
          pred_t2[,a]=pred_t1
          b=dpred$pred_t-pred_t1
          dpred$pred_t=pred_t1
          dtrain_2_int=dpred
          if(mean(abs(b))<0.001){
            break
          }
          a=a+1
          #dtrain_2=rbind(dt,dpred)
          dtrain_2predt[,dd]=dtrain_2_int$pred_t
          if(dd>1&mean(abs(dtrain_2predt[,dd]-dtrain_2predt[,(dd-1)]))<0.001){
            break
          }
        }
        if(dd<(tt-1)){
          for (j in 1:p){
            pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=dtrain_2_int$pred_t,sd=sd(dtrain_2_int$pred_t))
          }
          dd=dd+1
        }
      }
      
      ####################################
      #右設限的插補
      #dtrain_2_right
      if(length(df_pred_r$L)>0){
        ff=tryCatch({
          fitdist(dtrain_2$L+10^-10, "gamma")
        },
        error=function(e){
          ff=NULL
        })
        if(length(ff)==0){
          seed_number=seed_number+1
          next
        }
        e1=ff$estimate[1]
        e2=ff$estimate[2]
        e1=(rate[i])/(1-rate[i])*e1
        e2=(rate[i])/(1-rate[i])*e2
        pred_rr=rgamma(10000000,e1,e2)
        limit=dtrain_2_right$L
        pred_r_norm=NULL
        for(kk in 1:length(dtrain_2_right$L)){
          
          limit_samples=c(pred_rr[pred_rr >= limit[kk]&pred_rr <(limit[kk]+(1.5+rate[i]^2*e1/(e2))*ma)],dtrain_2_right$L[kk])
          #plot(limit_samples)
          pred_r_norm[kk]=mean(sample(limit_samples,20,replace = TRUE))
        }
        dtrain_2_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
      }
      dtrain_2=rbind(dtrain_2_int,dtrain_2_right)
      dtrain_2_right=dtrain_2_right[-1]
      
      ####################################
      #最後5行的結果
      #dpred$pred_t=rowMeans(pred_t2[,(a-4):a])
      #jj=sample(1:length(dtrain_2$L),length(dtrain_2$L))
      #dt=dtrain_2[jj,]
      #dpred=dtrain_2[jj,]
      #gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
      #pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
      #dpred$pred_t=pred_t1
      #dtrain_2=dpred
      #dtrain_2=rbind(dt,dpred)
      if(length(df_pred_r$L)>0){
        dtrain_3_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
        #pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=dtrain_1[dtrain_1$d==1,]
        pred_t=(dtrain_3$L+dtrain_3$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_3)
        dtrain_3=rbind(dtrain_3,dtrain_3_right)
      }else{
        dtrain_3=dtrain_1
        pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_1)
      }
      #############################文獻中點法
      dtrain_4=dtrain
      pred_t=ifelse(dtrain_4$d==1,(dtrain_4$L+dtrain_4$R)/2,dtrain_4$L)
      dtrain_4=data.frame(pred_t,dtrain_4)
      dtrain_4=dtrain_4[,-(4:5)]
      ctree_middle=ctree(Surv(pred_t,d)~x1+x2+x3+x4,dtrain_4)
      ctree_middle_pred=predict(ctree_middle,dtest)
      cindex_ctree_middle[i]=concordance(t~ctree_middle_pred,dtest)$concordance
      mse_ctree_middle[i]=mean((dtest$t-ctree_middle_pred)^2)
      mae_ctree_middle[i]=mean(abs(dtest$t-ctree_middle_pred))
      #############################決策樹
      reg=tree(pred_t~x1+x2+x3+x4,dtrain_3)
      reg_norm=tree(pred_t~x1+x2+x3+x4,dtrain_2)
      reg_pred_t = predict(reg,newdata=dtest)
      reg_pred_t_norm=predict(reg_norm,newdata=dtest)
      cindex_reg[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_reg[i]=mean((dtest$t-reg_pred_t)^2)
      mae_reg[i]=mean(abs(dtest$t-reg_pred_t))
      cindex_reg_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_reg_norm[i]=mean((dtest$t-reg_pred_t_norm)^2)
      mae_reg_norm[i]=mean(abs(dtest$t-reg_pred_t_norm))
      
      ############################bagging
      bag = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_3 ,importance = TRUE,mtry = 4)
      bag_pred_t = predict(bag,newdata=dtest)
      cindex_bag[i]=concordance(t~ bag_pred_t,data = dtest)$concordance   #######
      mse_bag[i]=mean((dtest$t-bag_pred_t)^2)
      mae_bag[i]=mean(abs(dtest$t-bag_pred_t))
      bag_norm = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_2 ,importance = TRUE,mtry = 4)
      bag_pred_t_norm = predict(bag_norm,newdata=dtest)
      cindex_bag_norm[i]=concordance(t~ bag_pred_t_norm,data = dtest)$concordance   #######
      mse_bag_norm[i]=mean((dtest$t-bag_pred_t_norm)^2)
      mae_bag_norm[i]=mean(abs(dtest$t-bag_pred_t_norm))
      
      ############################RF
      rf = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_3  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t = predict(rf,newdata=dtest)
      cindex_rf[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_rf[i]=mean((dtest$t-rf_pred_t)^2)
      mae_rf[i]=mean(abs(dtest$t-rf_pred_t))
      rf_norm = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_2  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t_norm = predict(rf_norm,newdata=dtest)
      cindex_rf_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_rf_norm[i]=mean((dtest$t-rf_pred_t_norm)^2)
      mae_rf_norm[i]=mean(abs(dtest$t-rf_pred_t_norm))
      
      ###########################Boosting
      boost = gbm(pred_t~x1+x2+x3+x4,data = dtrain_3 ,
                  distribution = "gaussian",n.trees = 1000,
                  interaction.depth = 4) #"gaussian" 選項,因為這是一個回歸問題
      #interaction.depth = 4 限制了每棵樹的深度
      boost_pred_t = predict(boost,newdata=dtest)
      cindex_boost[i]=concordance(t~ boost_pred_t,data = dtest)$concordance   #######
      mse_boost[i]=mean((dtest$t-boost_pred_t)^2)
      mae_boost[i]=mean(abs(dtest$t-boost_pred_t))
      boost_norm = gbm(pred_t~x1+x2+x3+x4,data = dtrain_2 ,
                       distribution = "gaussian",n.trees = 1000,
                       interaction.depth = 4)
      boost_pred_t_norm = predict(boost_norm,newdata=dtest)
      cindex_boost_norm[i]=concordance(t~ boost_pred_t_norm,data = dtest)$concordance   #######
      mse_boost_norm[i]=mean((dtest$t-boost_pred_t_norm)^2)
      mae_boost_norm[i]=mean(abs(dtest$t-boost_pred_t_norm))
      
      ##########################Xgb
      xgb = xgboost(data = as.matrix(dtrain_3 [,c(6:9)]),label = as.matrix(dtrain_3[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=100,verbose = 0)
      xgb_pred_t = predict(xgb,as.matrix(dtest[,c(5:8)]))
      cindex_xgb[i]=concordance(t~ xgb_pred_t,data = dtest)$concordance   #######
      mse_xgb[i]=mean((dtest$t-xgb_pred_t)^2)
      mae_xgb[i]=mean(abs(dtest$t-xgb_pred_t))
      xgb_norm = xgboost(data = as.matrix(dtrain_2 [,c(6:9)]),label = as.matrix(dtrain_2[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=1000,verbose = 0)
      xgb_pred_t_norm = predict(xgb_norm,as.matrix(dtest[,c(5:8)]))
      cindex_xgb_norm[i]=concordance(t~ xgb_pred_t_norm,data = dtest)$concordance   #######
      mse_xgb_norm[i]=mean((dtest$t-xgb_pred_t_norm)^2)
      mae_xgb_norm[i]=mean(abs(dtest$t-xgb_pred_t_norm))
      
      
      ##########################bart
      x <- dtrain_3[, 6:9]
      y <- dtrain_3[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:8]
      bartfit <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat <- bartfit$yhat.test.mean
      bart_cindex[i] = concordance(t ~ bart_yhat, data = dtest)$concordance
      bart_mse[i] = mean((bart_yhat -dtest$t)^2) 
      bart_mae[i] = mean(abs(bart_yhat -dtest$t))
      x <- dtrain_2[, 6:9]
      y <- dtrain_2[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:8]
      bartfit_norm <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat_norm <- bartfit_norm$yhat.test.mean
      bart_cindex_norm[i] = concordance(t ~ bart_yhat_norm, data = dtest)$concordance
      bart_mse_norm[i] = mean((bart_yhat_norm -dtest$t)^2) 
      bart_mae_norm[i] = mean(abs(bart_yhat_norm -dtest$t))
      
      
      ##########################ctree 
      ctree_m=ctree(pred_t~x1+x2+x3+x4,dtrain_3)
      ctree_pred_t = predict(ctree_m,newdata=dtest)
      cindex_ctree[i]=concordance(t~ ctree_pred_t,data = dtest)$concordance   #######
      mse_ctree[i]=mean((dtest$t-ctree_pred_t)^2)
      mae_ctree[i]=mean(abs(dtest$t-ctree_pred_t))
      ctree_m_norm=ctree(pred_t~x1+x2+x3+x4,dtrain_2)
      ctree_pred_t_norm = predict(ctree_m_norm,newdata=dtest)
      cindex_ctree_norm[i]=concordance(t~ ctree_pred_t_norm,data = dtest)$concordance   #######
      mse_ctree_norm[i]=mean((dtest$t-ctree_pred_t_norm)^2)
      mae_ctree_norm[i]=mean(abs(dtest$t-ctree_pred_t_norm))
      
      ##########################
      rpart=rpart(Surv(pred_t,d)~x1+x2+x3+x4,data = dtrain_3,method = "exp")
      tfit= as.party.rpart(rpart)
      rpart_pred_t = predict(tfit ,newdata =  dtest)
      cindex_rpart[i]=concordance(t~ rpart_pred_t,data = dtest)$concordance   #######
      mse_rpart[i]=mean((dtest$t-rpart_pred_t)^2)
      mae_rpart[i]=mean(abs(dtest$t-rpart_pred_t))
      rpart_norm=rpart(Surv(pred_t,d)~x1+x2+x3+x4,data = dtrain_2,method = "exp")
      tfit_norm= as.party.rpart(rpart_norm)
      rpart_pred_t_norm = predict(tfit_norm ,newdata =  dtest)
      cindex_rpart_norm[i]=concordance(t~ rpart_pred_t_norm,data = dtest)$concordance   #######
      mse_rpart_norm[i]=mean((dtest$t-rpart_pred_t_norm)^2)
      mae_rpart_norm[i]=mean(abs(dtest$t-rpart_pred_t_norm))
      ##########################
      #now1=proc.time()
      #Sys.sleep(1)
      #dtrain$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #dtest$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #mse_c[i]=mean((dtest$t-cforest_pred)^2)
      #mae_c[i]=mean(abs(dtest$t-cforest_pred))
      #cindex_c[i]=concordance(t ~ cforest_pred , data = dtest)$concordance
      ii[i]=ifelse((mean((dtest$t-bart_yhat)^2)>mean((dtest$t-bart_yhat_norm)^2)),1 ,0)
      i=i+1
      seed_number=seed_number+1
      print(c("rate"=1-(sum(d1$d)/length(d1$L)),"LTR"=mean((dtest$t-LTR_pred1)^2),"middel"=mean((dtest$t-bart_yhat)^2),"norm"=mean((dtest$t-bart_yhat_norm)^2)))
    }
  }
  if(type=="linear"){
    linear_result=list(c("type  "='線性',"censored_rate"=mean(rate),"樣本數"=n,"模擬次數"=(i-1),"超越次數"=sum(ii)),
                       "ICtree_result"=list(c("IC_cindex"=mean(cindex_LTR),"IC_mse"=mean(mse_LTR),"IC_mae"=mean(mae_LTR))),
                       "middle_ctree"=list(c("ctree_middle_cindex"=mean(cindex_ctree_middle),"ctree_middle_mse"=mean(mse_ctree_middle),"ctree_middle_mae"=mean(mae_ctree_middle))),
                       "linear_result_middle"=list("regression"=c("reg_cindex"=mean(cindex_reg),"reg_mse"=mean(mse_reg),"reg_mae"=mean(mae_reg))
                                                   ,"bagging"=c("bag_cindex"=mean(cindex_bag),"bag_mse"=mean(mse_bag),"bag_mae"=mean(mae_bag))
                                                   ,"randomforest"=c("rf_cindex"=mean(cindex_rf),"rf_mse"=mean(mse_rf),"rf_mae"=mean(mae_rf))
                                                   ,"boosting"=c("boost_cindex"=mean(cindex_boost),"boost_mse"=mean(mse_boost),"boost_mae"=mean(mae_boost))
                                                   ,"xgb"=c("xgb_cindex"=mean(cindex_xgb),"xgb_mse"=mean(mse_xgb),"xgb_mae"=mean(mae_xgb))
                                                   ,"bart"=c("bart_cindex"=mean(bart_cindex),"bart_mse"= mean(bart_mse),"bart_mae"= mean(bart_mae))
                                                   ,"ctree"=c("ctree_cindex"=mean(cindex_ctree),"ctree_mse"=mean(mse_ctree),"ctree_mae"=mean(mae_ctree))
                                                   ,"rapart"=c("rpart_cindex"=mean(cindex_rpart),"rpart_mse"=mean(mse_rpart),"rpart_mae"=mean(mae_rpart)))
                       ,"linear_result_norm"=list("regression"=c("reg_cindex_norm"=mean(cindex_reg_norm),"reg_mse_norm"=mean(mse_reg_norm),"reg_mae_norm"=mean(mae_reg_norm))
                                                  ,"bagging"=c("bag_cindex_norm"=mean(cindex_bag_norm),"bag_mse_norm"=mean(mse_bag_norm),"bag_mae_norm"=mean(mae_bag_norm))
                                                  ,"randomforest"=c("rf_cindex_norm"=mean(cindex_rf_norm),"rf_mse_norm"=mean(mse_rf_norm),"rf_mae_norm"=mean(mae_rf_norm))
                                                  ,"boosting"=c("boost_cindex_norm"=mean(cindex_boost_norm),"boost_mse_norm"=mean(mse_boost_norm),"boost_mae_norm"=mean(mae_boost_norm))
                                                  ,"xgb"=c("xgb_cindex_norm"=mean(cindex_xgb_norm),"xgb_mse_norm"=mean(mse_xgb_norm),"xgb_mae_norm"=mean(mae_xgb_norm))
                                                  ,"bart"=c("bart_cindex_norm"=mean(bart_cindex_norm),"bart_mse_norm"= mean(bart_mse_norm),"bart_mae_norm"= mean(bart_mae_norm))
                                                  ,"ctree"=c("ctree_cindex_norm"=mean(cindex_ctree_norm),"ctree_mse_norm"=mean(mse_ctree_norm),"ctree_mae_norm"=mean(mae_ctree_norm))
                                                  ,"rapart"=c("rpart_cindex_norm"=mean(cindex_rpart_norm),"rpart_mse_norm"=mean(mse_rpart_norm),"rpart_mae_norm"=mean(mae_rpart_norm)))
                       
    )
    return(linear_result)
  }else if(type=="boxcox"){
    boxcox_result=list(c("type  "='boxcox',"censored_rate"=mean(rate),"樣本數"=n,"模擬次數"=(i-1),"超越次數"=sum(ii)),
                       "ICtree_result"=list(c("IC_cindex"=mean(cindex_LTR),"IC_mse"=mean(mse_LTR),"IC_mae"=mean(mae_LTR))),
                       "middle_ctree"=list(c("ctree_middle_cindex"=mean(cindex_ctree_middle),"ctree_middle_mse"=mean(mse_ctree_middle),"ctree_middle_mae"=mean(mae_ctree_middle))),
                       "boxcox_result_middle"=list("regression"=c("reg_cindex"=mean(cindex_reg),"reg_mse"=mean(mse_reg),"reg_mae"=mean(mae_reg))
                                                   ,"bagging"=c("bag_cindex"=mean(cindex_bag),"bag_mse"=mean(mse_bag),"bag_mae"=mean(mae_bag))
                                                   ,"randomforest"=c("rf_cindex"=mean(cindex_rf),"rf_mse"=mean(mse_rf),"rf_mae"=mean(mae_rf))
                                                   ,"boosting"=c("boost_cindex"=mean(cindex_boost),"boost_mse"=mean(mse_boost),"boost_mae"=mean(mae_boost))
                                                   ,"xgb"=c("xgb_cindex"=mean(cindex_xgb),"xgb_mse"=mean(mse_xgb),"xgb_mae"=mean(mae_xgb))
                                                   ,"bart"=c("bart_cindex"=mean(bart_cindex),"bart_mse"= mean(bart_mse),"bart_mae"= mean(bart_mae))
                                                   ,"ctree"=c("ctree_cindex"=mean(cindex_ctree),"ctree_mse"=mean(mse_ctree),"ctree_mae"=mean(mae_ctree))
                                                   ,"rapart"=c("rpart_cindex"=mean(cindex_rpart),"rpart_mse"=mean(mse_rpart),"rpart_mae"=mean(mae_rpart)))
                       ,"boxcox_result_norm"=list("regression"=c("reg_cindex_norm"=mean(cindex_reg_norm),"reg_mse_norm"=mean(mse_reg_norm),"reg_mae_norm"=mean(mae_reg_norm))
                                                  ,"bagging"=c("bag_cindex_norm"=mean(cindex_bag_norm),"bag_mse_norm"=mean(mse_bag_norm),"bag_mae_norm"=mean(mae_bag_norm))
                                                  ,"randomforest"=c("rf_cindex_norm"=mean(cindex_rf_norm),"rf_mse_norm"=mean(mse_rf_norm),"rf_mae_norm"=mean(mae_rf_norm))
                                                  ,"boosting"=c("boost_cindex_norm"=mean(cindex_boost_norm),"boost_mse_norm"=mean(mse_boost_norm),"boost_mae_norm"=mean(mae_boost_norm))
                                                  ,"xgb"=c("xgb_cindex_norm"=mean(cindex_xgb_norm),"xgb_mse_norm"=mean(mse_xgb_norm),"xgb_mae_norm"=mean(mae_xgb_norm))
                                                  ,"bart"=c("bart_cindex_norm"=mean(bart_cindex_norm),"bart_mse_norm"= mean(bart_mse_norm),"bart_mae_norm"= mean(bart_mae_norm))
                                                  ,"ctree"=c("ctree_cindex_norm"=mean(cindex_ctree_norm),"ctree_mse_norm"=mean(mse_ctree_norm),"ctree_mae_norm"=mean(mae_ctree_norm))
                                                  ,"rapart"=c("rpart_cindex_norm"=mean(cindex_rpart_norm),"rpart_mse_norm"=mean(mse_rpart_norm),"rpart_mae_norm"=mean(mae_rpart_norm)))
    )
    return(boxcox_result)
  }else if(type=="ph"){
    ph_result=list(c("type  "='ph',"censored_rate"=mean(rate),"樣本數"=n,"模擬次數"=(i-1),"超越次數"=sum(ii)),
                   "ICtree_result"=list(c("IC_cindex"=mean(cindex_LTR),"IC_mse"=mean(mse_LTR),"IC_mae"=mean(mae_LTR))),
                   "middle_ctree"=list(c("ctree_middle_cindex"=mean(cindex_ctree_middle),"ctree_middle_mse"=mean(mse_ctree_middle),"ctree_middle_mae"=mean(mae_ctree_middle))),
                   "ph_result_middle"=list("regression"=c("reg_cindex"=mean(cindex_reg),"reg_mse"=mean(mse_reg),"reg_mae"=mean(mae_reg))
                                           ,"bagging"=c("bag_cindex"=mean(cindex_bag),"bag_mse"=mean(mse_bag),"bag_mae"=mean(mae_bag))
                                           ,"randomforest"=c("rf_cindex"=mean(cindex_rf),"rf_mse"=mean(mse_rf),"rf_mae"=mean(mae_rf))
                                           ,"boosting"=c("boost_cindex"=mean(cindex_boost),"boost_mse"=mean(mse_boost),"boost_mae"=mean(mae_boost))
                                           ,"xgb"=c("xgb_cindex"=mean(cindex_xgb),"xgb_mse"=mean(mse_xgb),"xgb_mae"=mean(mae_xgb))
                                           ,"bart"=c("bart_cindex"=mean(bart_cindex),"bart_mse"= mean(bart_mse),"bart_mae"= mean(bart_mae))
                                           ,"ctree"=c("ctree_cindex"=mean(cindex_ctree),"ctree_mse"=mean(mse_ctree),"ctree_mae"=mean(mae_ctree))
                                           ,"rapart"=c("rpart_cindex"=mean(cindex_rpart),"rpart_mse"=mean(mse_rpart),"rpart_mae"=mean(mae_rpart)))
                   ,"ph_result_norm"=list("regression"=c("reg_cindex_norm"=mean(cindex_reg_norm),"reg_mse_norm"=mean(mse_reg_norm),"reg_mae_norm"=mean(mae_reg_norm))
                                          ,"bagging"=c("bag_cindex_norm"=mean(cindex_bag_norm),"bag_mse_norm"=mean(mse_bag_norm),"bag_mae_norm"=mean(mae_bag_norm))
                                          ,"randomforest"=c("rf_cindex_norm"=mean(cindex_rf_norm),"rf_mse_norm"=mean(mse_rf_norm),"rf_mae_norm"=mean(mae_rf_norm))
                                          ,"boosting"=c("boost_cindex_norm"=mean(cindex_boost_norm),"boost_mse_norm"=mean(mse_boost_norm),"boost_mae_norm"=mean(mae_boost_norm))
                                          ,"xgb"=c("xgb_cindex_norm"=mean(cindex_xgb_norm),"xgb_mse_norm"=mean(mse_xgb_norm),"xgb_mae_norm"=mean(mae_xgb_norm))
                                          ,"bart"=c("bart_cindex_norm"=mean(bart_cindex_norm),"bart_mse_norm"= mean(bart_mse_norm),"bart_mae_norm"= mean(bart_mae_norm))
                                          ,"ctree"=c("ctree_cindex_norm"=mean(cindex_ctree_norm),"ctree_mse_norm"=mean(mse_ctree_norm),"ctree_mae_norm"=mean(mae_ctree_norm))
                                          ,"rapart"=c("rpart_cindex_norm"=mean(cindex_rpart_norm),"rpart_mse_norm"=mean(mse_rpart_norm),"rpart_mae_norm"=mean(mae_rpart_norm)))
    )
    
    return(ph_result)
  }else if(type=="aft"){
    mae_reg=NULL
    mae_LTR=NULL
    df_pred_r=NULL
    mse_LTR=NULL
    mse_reg=NULL
    cindex_reg=NULL
    cindex_LTR=NULL
    cindex_bag=NULL
    mse_bag=NULL
    mae_bag=NULL
    cindex_rf=NULL
    mse_rf=NULL
    mae_rf=NULL
    cindex_boost=NULL
    mse_boost=NULL
    mae_boost=NULL
    cindex_xgb=NULL
    mse_xgb=NULL
    mae_xgb=NULL
    cindex_ctree=NULL
    mse_ctree=NULL
    mae_ctree=NULL
    cindex_rpart=NULL
    mse_rpart=NULL
    mae_rpart=NULL
    bart_cindex=NULL
    bart_mse= NULL 
    bart_mae= NULL
    cindex_rpart= NULL
    mse_rpart= NULL
    mae_rpart= NULL
    
    
    cindex_reg_norm=NULL
    mse_reg_norm=NULL
    mae_reg_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_boost_norm=NULL
    cindex_rf_norm=NULL
    mse_rf_norm=NULL
    mae_rf_norm=NULL
    mse_boost_norm=NULL
    mae_boost_norm=NULL
    bart_cindex_norm=NULL
    bart_mse_norm=NULL
    bart_mae_norm=NULL
    cindex_xgb_norm=NULL
    mse_xgb_norm=NULL
    mae_xgb_norm=NULL
    cindex_ctree_norm=NULL
    mse_ctree_norm=NULL
    mae_ctree_norm=NULL
    cindex_rpart_norm= NULL
    mse_rpart_norm= NULL
    mae_rpart_norm= NULL
    
    cindex_ctree_middle=NULL
    mse_ctree_middle=NULL
    mae_ctree_middle=NULL
    mse_c=NULL
    mae_c=NULL
    cindex_c=NULL
    rate=NULL
    ii=NULL
    #################
    generate_t=function(n,x1,x2,x3,x4){
      #theta=-(cos((x1+x2)*pi)+sqrt(x1+x2))
      #theta=beta1*x1+beta2*x2+beta3*x3+beta4*x4
      t1=beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,2,1)#線性ok k=30
      t2=beta0+(0.5*(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5))+1)^2#box_cox trainsfer
      t3=beta0+log(exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5)))#ph model ok k=30
      t4=NULL
      for (jj in 1:n) {
        t4[jj]=beta0+min(beta1*x1[jj],beta2*x2[jj],beta3*x3[jj],beta4*x4[jj])+rnorm(1,3,0.5)#複雜模型
      }
      t5=beta0+exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,0.8,0.3))
      t6=beta0+(beta1*x1+beta2*sin(x2))^0.5+beta3*x3^2+beta4*x2*x3#複雜模型
      return(list(t=t5))
    }
    
    generate_censoring = function(t, k, censoring_rate) {
      #exam_times =seq(0,quantile(t,0.9), length.out = k + 1)
      delta_t =runif(k, min =min, max =max)  # 從 U[0.3, 0.7] 中取值
      tt=0
      exam_times=NULL
      for (i in 1:(k+1)){
        exam_times[i]=tt
        tt=tt+delta_t[i]
      }
      L =sapply(t, function(t) max(exam_times[exam_times <= t]))
      #R=L+delta_t
      R = exam_times[sapply(L,function(x)which(exam_times==x))+1]
      R[is.na(R)]=Inf
      # 將部分觀測值設置為右設限，根據指定比例 censoring_rate
      right_censor <- runif(n) < censoring_rate
      R[right_censor] <-Inf  # 設定為右設限
      return(list(L = L, R = R,status=as.integer(L<t&t<R&R!=Inf)))  # status 表示是否觀測到事件
    }
    ##################
    i=1
    seed_number=i
    censoring_rate =0#突然失聯的人比例
    now=proc.time()
    while (i<=gg){
      #Sys.sleep(1.5)
      set.seed(seed_number+80)
      #set.seed(85)
      #生成xi
      x1 <- sample(0:1,n,replace=TRUE)
      x2 <- rnorm(n,1.3,0.3)
      x3 <- rgamma(n,3,10)
      x4 <- rbeta(n,1.5,0.5)
      # 設定係數 beta
      beta0=beta[1]
      beta1= beta[2]
      beta2 = beta[3]
      beta3 = beta[4]
      beta4 = beta[5]
      #製作資料集
      t=generate_t(n,x1,x2,x3,x4)
      censoring= generate_censoring(t$t, k, censoring_rate) 
      d1=data.frame(t=t$t,d=censoring$status,L=censoring$L,R=censoring$R,x1,x2,x3,x4)
      rate[i]=1-(sum(d1$d)/length(d1$L))
      print(type)
      print(i)
      #檢驗資料是否符合coxph assumtion
      #summary(coxph(Surv(t,d) ~ x1+x2+x3+x4, d1)) 
      #sur=survfit(Surv(t,d)~ x1+x2+x3+x4, data=d1)
      #plot(sur)
      #plot(t$t)
      #分組
      s=sample(1:n,0.7*n)
      dtrain=d1[s,]
      dtest=d1[-s,]
      control_params <- partykit::ctree_control(
        minsplit = 20,
        minbucket =5,
        mincriterion = 0.7,
        mtry = 2
      )
      fit=ICtree(Surv(L,R,type="interval2")~x1+x2+x3+x4,dtrain, Control = control_params)
      #plot(fit)
      LTR_pred1=predict(fit, newdata = dtest, type="response")
      LTR_pred2=predict(fit, newdata = dtest, type="prob")
      if(mean((dtest$t-LTR_pred1)^2)!=Inf){
        #plot(fit)
        mse_LTR[i]=mean((dtest$t-LTR_pred1)^2)
        mae_LTR[i]=mean(abs(dtest$t-LTR_pred1))
        cindex_LTR[i]=concordance(t ~ LTR_pred1 , data = dtest)$concordance
        #cat(mse[i])
        #usedtime2[i]=proc.time()-now1
      } else{
        next
      }
      #計算時間
      #now=proc.time()
      #估計參數ic_sp
      cox_fit=ic_sp(Surv(L,R,type = "interval2")~x1+x2+x3+x4,data=dtrain,model = "ph")
      summary(cox_fit)
      b1=cox_fit$coefficients[1]
      b2=cox_fit$coefficients[2]
      b3=cox_fit$coefficients[3]
      b4=cox_fit$coefficients[4]
      c(b1,b2,b3,b4)
      #b5=cox_fit$coefficients[5]
      #b6=cox_fit$coefficients[6]
      #plot(cox_fit)
      x_beta=b1*dtrain$x1+b2*dtrain$x2+b3*dtrain$x3+b4*dtrain$x4
      #x_beta=dtrain$x1+dtrain$x2+dtrain$x3+dtrain$x4
      #x_beta=predict(cox_fit,newdata=dtrain)
      dtrain$x=x_beta
      df_1=subset(dtrain,d==1)
      pred_t=(df_1$L+df_1$R)/2
      df_1$pred_t=pred_t
      df_pred=subset(dtrain,d==0)
      mm=min(df_1$R-df_1$L)
      ma=max(df_1$R-df_1$L)
      m=runif(length(df_pred$t),mm,ma)
      r=range(dtrain$L)[2]-range(dtrain$L)[1]
      gam_l=tryCatch({
        gam(L~s(x,bs="tp"),data=df_1,method = "REML")
      },error=function(e){
        gam_l=NULL
      })
      if(length(gam_l)==0){
        seed_number=seed_number+1
        next
      }
      #gam_r=gam(R~s(x,bs="tp"),data=df_1,method = "REML")
      #gam_t=gam(ht~s(x,bs="tp"),data=df_1,method = "REML")
      #plot(gam_l)
      #plot(gam_r)
      pred_L=predict(gam_l,data.frame(x=df_pred[,"x"]))
      pred_L=pmax(pred_L,df_pred$L)
      #pred_R=predict(gam_r,data.frame(x=df_pred[,"x"]))
      #pred_t=predict(gam_t,data.frame(x=df_pred[,"x"]))
      pred_R=pred_L+m
      df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8])
      if(length(df_pred_r$L)>0){
        df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8],x=0)
      }
      dtrain_1=rbind(df_1[1:9],df_pred_r)
      
      #############################################
      dtrain_2=dtrain_1
      dtrain_2_int=dtrain_2[dtrain_2$d==1,]
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      ###########################################
      #區間的插補
      p=150
      #cforest_pred1=matrix(0,length(dtest$L),p)
      pred_t_n=matrix(0,length(dtrain_2_int$L),p)
      st=NULL
      for(r in 1:length(dtrain_2_int$L)){
        st[r]=runif(1,dtrain_2_int$L[r],dtrain_2_int$R[r])
      }
      for (j in 1:p){
        pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=mean((dtrain_2_int$L+dtrain_2_int$R)/2),sd=sd(st)/3)
        #Cforest <- ICcforest(Surv(L,R,type="interval2")~x1+x2+x3+x4,dtrain_2, Control = control_params)
        #cforest_pred1[,j]=predict(Cforest,dtest, type="response")
      }
      pred_t_norm=rowMeans(pred_t_n)
      dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int)
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      tt=100
      dtrain_2predt=matrix(0,nrow =length(dtrain_2_int$L),ncol = 50 )
      #cforest_pred=rowMeans(cforest_pred1)
      dd=1
      while (dd<cd) {
        pred_t_norm=rowMeans(pred_t_n)
        dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int[-1])
        pred_t2=matrix(0,length(dtrain_2_int$L),tt)
        xgb_impute = xgboost(data = as.matrix(dtrain_2_int [,c(6:9)]),label = as.matrix(dtrain_2_int[,"pred_t"]),nrounds = 80,objective = "reg:squarederror",colsample_bytree = 0.5,eta = 0.1,max_depth = 6,verbose = 0)
        xgb_pred_t_impute = predict(xgb_impute,as.matrix(dtrain_2_int[,c(6:9)]))
        pred_t0=ifelse((dtrain_2_int$R>xgb_pred_t_impute&xgb_pred_t_impute>dtrain_2_int$L),xgb_pred_t_impute,dtrain_2_int$pred_t)
        dtrain_2_int$pred_t=pred_t0
        a=1
        while(a<tt){
          
          #cat(dd,a)
          jj=sample(1:length(dtrain_2_int$L),length(dtrain_2_int$L))
          dt=dtrain_2_int[jj,]
          dpred=dtrain_2_int[jj,]
          gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
          pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
          pred_t2[,a]=pred_t1
          b=dpred$pred_t-pred_t1
          dpred$pred_t=pred_t1
          dtrain_2_int=dpred
          if(mean(abs(b))<0.001){
            break
          }
          a=a+1
          #dtrain_2=rbind(dt,dpred)
          dtrain_2predt[,dd]=dtrain_2_int$pred_t
          if(dd>1&mean(abs(dtrain_2predt[,dd]-dtrain_2predt[,(dd-1)]))<0.001){
            break
          }
        }
        if(dd<(tt-1)){
          for (j in 1:p){
            pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=dtrain_2_int$pred_t,sd=sd(dtrain_2_int$pred_t))
          }
          dd=dd+1
        }
      }
      
      ####################################
      #右設限的插補
      #dtrain_2_right
      if(length(df_pred_r$L)>0){
        ff=tryCatch({
          fitdist(dtrain_2$L+10^-10, "gamma")
        },
        error=function(e){
          ff=NULL
        })
        if(length(ff)==0){
          seed_number=seed_number+1
          next
        }
        e1=ff$estimate[1]
        e2=ff$estimate[2]
        e1=(rate[i])/(1-rate[i])*e1
        e2=(rate[i])/(1-rate[i])*e2
        pred_rr=rgamma(10000000,e1,e2)
        limit=dtrain_2_right$L
        pred_r_norm=NULL
        for(kk in 1:length(dtrain_2_right$L)){
          
          limit_samples=c(pred_rr[pred_rr >= limit[kk]&pred_rr <(limit[kk]+(1.5+rate[i]^2*e1/(e2))*ma)],dtrain_2_right$L[kk])
          #plot(limit_samples)
          pred_r_norm[kk]=mean(sample(limit_samples,20,replace = TRUE))
        }
        dtrain_2_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
      }
      dtrain_2=rbind(dtrain_2_int,dtrain_2_right)
      dtrain_2_right=dtrain_2_right[-1]
      ####################################
      #最後5行的結果
      #dpred$pred_t=rowMeans(pred_t2[,(a-4):a])
      #jj=sample(1:length(dtrain_2$L),length(dtrain_2$L))
      #dt=dtrain_2[jj,]
      #dpred=dtrain_2[jj,]
      #gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
      #pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
      #dpred$pred_t=pred_t1
      #dtrain_2=dpred
      #dtrain_2=rbind(dt,dpred)
      if(length(df_pred_r$L)>0){
        dtrain_3_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
        #pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=dtrain_1[dtrain_1$d==1,]
        pred_t=(dtrain_3$L+dtrain_3$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_3)
        dtrain_3=rbind(dtrain_3,dtrain_3_right)
      }else{
        dtrain_3=dtrain_1
        pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_1)
      }
      #############################文獻中點法
      dtrain_4=dtrain
      pred_t=ifelse(dtrain_4$d==1,(dtrain_4$L+dtrain_4$R)/2,dtrain_4$L)
      dtrain_4=data.frame(pred_t,dtrain_4)
      dtrain_4=dtrain_4[,-(4:5)]
      ctree_middle=ctree(Surv(pred_t,d)~x1+x2+x3+x4,dtrain_4)
      ctree_middle_pred=predict(ctree_middle,dtest)
      cindex_ctree_middle[i]=concordance(t~ctree_middle_pred,dtest)$concordance
      mse_ctree_middle[i]=mean((dtest$t-ctree_middle_pred)^2)
      mae_ctree_middle[i]=mean(abs(dtest$t-ctree_middle_pred))
      #############################決策樹
      reg=tree(pred_t~x1+x2+x3+x4,dtrain_3)
      reg_norm=tree(pred_t~x1+x2+x3+x4,dtrain_2)
      reg_pred_t = predict(reg,newdata=dtest)
      reg_pred_t_norm=predict(reg_norm,newdata=dtest)
      cindex_reg[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_reg[i]=mean((dtest$t-reg_pred_t)^2)
      mae_reg[i]=mean(abs(dtest$t-reg_pred_t))
      cindex_reg_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_reg_norm[i]=mean((dtest$t-reg_pred_t_norm)^2)
      mae_reg_norm[i]=mean(abs(dtest$t-reg_pred_t_norm))
      
      ############################bagging
      bag = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_3 ,importance = TRUE,mtry = 4)
      bag_pred_t = predict(bag,newdata=dtest)
      cindex_bag[i]=concordance(t~ bag_pred_t,data = dtest)$concordance   #######
      mse_bag[i]=mean((dtest$t-bag_pred_t)^2)
      mae_bag[i]=mean(abs(dtest$t-bag_pred_t))
      bag_norm = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_2 ,importance = TRUE,mtry = 4)
      bag_pred_t_norm = predict(bag_norm,newdata=dtest)
      cindex_bag_norm[i]=concordance(t~ bag_pred_t_norm,data = dtest)$concordance   #######
      mse_bag_norm[i]=mean((dtest$t-bag_pred_t_norm)^2)
      mae_bag_norm[i]=mean(abs(dtest$t-bag_pred_t_norm))
      
      ############################RF
      rf = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_3  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t = predict(rf,newdata=dtest)
      cindex_rf[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_rf[i]=mean((dtest$t-rf_pred_t)^2)
      mae_rf[i]=mean(abs(dtest$t-rf_pred_t))
      rf_norm = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_2  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t_norm = predict(rf_norm,newdata=dtest)
      cindex_rf_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_rf_norm[i]=mean((dtest$t-rf_pred_t_norm)^2)
      mae_rf_norm[i]=mean(abs(dtest$t-rf_pred_t_norm))
      
      ###########################Boosting
      boost = gbm(pred_t~x1+x2+x3+x4,data = dtrain_3 ,
                  distribution = "gaussian",n.trees = 1000,
                  interaction.depth = 4) #"gaussian" 選項,因為這是一個回歸問題
      #interaction.depth = 4 限制了每棵樹的深度
      boost_pred_t = predict(boost,newdata=dtest)
      cindex_boost[i]=concordance(t~ boost_pred_t,data = dtest)$concordance   #######
      mse_boost[i]=mean((dtest$t-boost_pred_t)^2)
      mae_boost[i]=mean(abs(dtest$t-boost_pred_t))
      boost_norm = gbm(pred_t~x1+x2+x3+x4,data = dtrain_2 ,
                       distribution = "gaussian",n.trees = 1000,
                       interaction.depth = 4)
      boost_pred_t_norm = predict(boost_norm,newdata=dtest)
      cindex_boost_norm[i]=concordance(t~ boost_pred_t_norm,data = dtest)$concordance   #######
      mse_boost_norm[i]=mean((dtest$t-boost_pred_t_norm)^2)
      mae_boost_norm[i]=mean(abs(dtest$t-boost_pred_t_norm))
      
      ##########################Xgb
      xgb = xgboost(data = as.matrix(dtrain_3 [,c(6:9)]),label = as.matrix(dtrain_3[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=100,verbose = 0)
      xgb_pred_t = predict(xgb,as.matrix(dtest[,c(5:8)]))
      cindex_xgb[i]=concordance(t~ xgb_pred_t,data = dtest)$concordance   #######
      mse_xgb[i]=mean((dtest$t-xgb_pred_t)^2)
      mae_xgb[i]=mean(abs(dtest$t-xgb_pred_t))
      xgb_norm = xgboost(data = as.matrix(dtrain_2 [,c(6:9)]),label = as.matrix(dtrain_2[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=1000,verbose = 0)
      xgb_pred_t_norm = predict(xgb_norm,as.matrix(dtest[,c(5:8)]))
      cindex_xgb_norm[i]=concordance(t~ xgb_pred_t_norm,data = dtest)$concordance   #######
      mse_xgb_norm[i]=mean((dtest$t-xgb_pred_t_norm)^2)
      mae_xgb_norm[i]=mean(abs(dtest$t-xgb_pred_t_norm))
      
      
      ##########################bart
      x <- dtrain_3[, 6:9]
      y <- dtrain_3[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:8]
      bartfit <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat <- bartfit$yhat.test.mean
      bart_cindex[i] = concordance(t ~ bart_yhat, data = dtest)$concordance
      bart_mse[i] = mean((bart_yhat -dtest$t)^2) 
      bart_mae[i] = mean(abs(bart_yhat -dtest$t))
      x <- dtrain_2[, 6:9]
      y <- dtrain_2[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:8]
      bartfit_norm <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat_norm <- bartfit_norm$yhat.test.mean
      bart_cindex_norm[i] = concordance(t ~ bart_yhat_norm, data = dtest)$concordance
      bart_mse_norm[i] = mean((bart_yhat_norm -dtest$t)^2) 
      bart_mae_norm[i] = mean(abs(bart_yhat_norm -dtest$t))
      
      
      ##########################ctree 
      ctree_m=ctree(pred_t~x1+x2+x3+x4,dtrain_3)
      ctree_pred_t = predict(ctree_m,newdata=dtest)
      cindex_ctree[i]=concordance(t~ ctree_pred_t,data = dtest)$concordance   #######
      mse_ctree[i]=mean((dtest$t-ctree_pred_t)^2)
      mae_ctree[i]=mean(abs(dtest$t-ctree_pred_t))
      ctree_m_norm=ctree(pred_t~x1+x2+x3+x4,dtrain_2)
      ctree_pred_t_norm = predict(ctree_m_norm,newdata=dtest)
      cindex_ctree_norm[i]=concordance(t~ ctree_pred_t_norm,data = dtest)$concordance   #######
      mse_ctree_norm[i]=mean((dtest$t-ctree_pred_t_norm)^2)
      mae_ctree_norm[i]=mean(abs(dtest$t-ctree_pred_t_norm))
      
      ##########################
      rpart=rpart(Surv(pred_t,d)~x1+x2+x3+x4,data = dtrain_3,method = "exp")
      tfit= as.party.rpart(rpart)
      rpart_pred_t = predict(tfit ,newdata =  dtest)
      cindex_rpart[i]=concordance(t~ rpart_pred_t,data = dtest)$concordance   #######
      mse_rpart[i]=mean((dtest$t-rpart_pred_t)^2)
      mae_rpart[i]=mean(abs(dtest$t-rpart_pred_t))
      rpart_norm=rpart(Surv(pred_t,d)~x1+x2+x3+x4,data = dtrain_2,method = "exp")
      tfit_norm= as.party.rpart(rpart_norm)
      rpart_pred_t_norm = predict(tfit_norm ,newdata =  dtest)
      cindex_rpart_norm[i]=concordance(t~ rpart_pred_t_norm,data = dtest)$concordance   #######
      mse_rpart_norm[i]=mean((dtest$t-rpart_pred_t_norm)^2)
      mae_rpart_norm[i]=mean(abs(dtest$t-rpart_pred_t_norm))
      ##########################
      #now1=proc.time()
      #Sys.sleep(1)
      #dtrain$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #dtest$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #mse_c[i]=mean((dtest$t-cforest_pred)^2)
      #mae_c[i]=mean(abs(dtest$t-cforest_pred))
      #cindex_c[i]=concordance(t ~ cforest_pred , data = dtest)$concordance
      ii[i]=ifelse((mean((dtest$t-bart_yhat)^2)>mean((dtest$t-bart_yhat_norm)^2)),1 ,0)
      i=i+1
      seed_number=seed_number+1
      print(c("rate"=1-(sum(d1$d)/length(d1$L)),"LTR"=mean((dtest$t-LTR_pred1)^2),"middel"=mean((dtest$t-bart_yhat)^2),"norm"=mean((dtest$t-bart_yhat_norm)^2)))
    }
    aft_result=list(c("type  "='aft',"censored_rate"=mean(rate),"樣本數"=n,"模擬次數"=(i-1),"超越次數"=sum(ii)),
                    "ICtree_result"=list(c("IC_cindex"=mean(cindex_LTR),"IC_mse"=mean(mse_LTR),"IC_mae"=mean(mae_LTR))),
                    "middle_ctree"=list(c("ctree_middle_cindex"=mean(cindex_ctree_middle),"ctree_middle_mse"=mean(mse_ctree_middle),"ctree_middle_mae"=mean(mae_ctree_middle))),
                    "aft_result_middle"=list("regression"=c("reg_cindex"=mean(cindex_reg),"reg_mse"=mean(mse_reg),"reg_mae"=mean(mae_reg))
                                             ,"bagging"=c("bag_cindex"=mean(cindex_bag),"bag_mse"=mean(mse_bag),"bag_mae"=mean(mae_bag))
                                             ,"randomforest"=c("rf_cindex"=mean(cindex_rf),"rf_mse"=mean(mse_rf),"rf_mae"=mean(mae_rf))
                                             ,"boosting"=c("boost_cindex"=mean(cindex_boost),"boost_mse"=mean(mse_boost),"boost_mae"=mean(mae_boost))
                                             ,"xgb"=c("xgb_cindex"=mean(cindex_xgb),"xgb_mse"=mean(mse_xgb),"xgb_mae"=mean(mae_xgb))
                                             ,"bart"=c("bart_cindex"=mean(bart_cindex),"bart_mse"= mean(bart_mse),"bart_mae"= mean(bart_mae))
                                             ,"ctree"=c("ctree_cindex"=mean(cindex_ctree),"ctree_mse"=mean(mse_ctree),"ctree_mae"=mean(mae_ctree))
                                             ,"rapart"=c("rpart_cindex"=mean(cindex_rpart),"rpart_mse"=mean(mse_rpart),"rpart_mae"=mean(mae_rpart)))
                    ,"aft_result_norm"=list("regression"=c("reg_cindex_norm"=mean(cindex_reg_norm),"reg_mse_norm"=mean(mse_reg_norm),"reg_mae_norm"=mean(mae_reg_norm))
                                            ,"bagging"=c("bag_cindex_norm"=mean(cindex_bag_norm),"bag_mse_norm"=mean(mse_bag_norm),"bag_mae_norm"=mean(mae_bag_norm))
                                            ,"randomforest"=c("rf_cindex_norm"=mean(cindex_rf_norm),"rf_mse_norm"=mean(mse_rf_norm),"rf_mae_norm"=mean(mae_rf_norm))
                                            ,"boosting"=c("boost_cindex_norm"=mean(cindex_boost_norm),"boost_mse_norm"=mean(mse_boost_norm),"boost_mae_norm"=mean(mae_boost_norm))
                                            ,"xgb"=c("xgb_cindex_norm"=mean(cindex_xgb_norm),"xgb_mse_norm"=mean(mse_xgb_norm),"xgb_mae_norm"=mean(mae_xgb_norm))
                                            ,"bart"=c("bart_cindex_norm"=mean(bart_cindex_norm),"bart_mse_norm"= mean(bart_mse_norm),"bart_mae_norm"= mean(bart_mae_norm))
                                            ,"ctree"=c("ctree_cindex_norm"=mean(cindex_ctree_norm),"ctree_mse_norm"=mean(mse_ctree_norm),"ctree_mae_norm"=mean(mae_ctree_norm))
                                            ,"rapart"=c("rpart_cindex_norm"=mean(cindex_rpart_norm),"rpart_mse_norm"=mean(mse_rpart_norm),"rpart_mae_norm"=mean(mae_rpart_norm)))
    )
    return(aft_result)
  }else if (type=="weibull"){
    mae_reg=NULL
    mae_LTR=NULL
    df_pred_r=NULL
    mse_LTR=NULL
    mse_reg=NULL
    cindex_reg=NULL
    cindex_LTR=NULL
    cindex_bag=NULL
    mse_bag=NULL
    mae_bag=NULL
    cindex_rf=NULL
    mse_rf=NULL
    mae_rf=NULL
    cindex_boost=NULL
    mse_boost=NULL
    mae_boost=NULL
    cindex_xgb=NULL
    mse_xgb=NULL
    mae_xgb=NULL
    cindex_ctree=NULL
    mse_ctree=NULL
    mae_ctree=NULL
    cindex_rpart=NULL
    mse_rpart=NULL
    mae_rpart=NULL
    bart_cindex=NULL
    bart_mse= NULL 
    bart_mae= NULL
    cindex_rpart= NULL
    mse_rpart= NULL
    mae_rpart= NULL
    
    
    cindex_reg_norm=NULL
    mse_reg_norm=NULL
    mae_reg_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_boost_norm=NULL
    cindex_rf_norm=NULL
    mse_rf_norm=NULL
    mae_rf_norm=NULL
    mse_boost_norm=NULL
    mae_boost_norm=NULL
    bart_cindex_norm=NULL
    bart_mse_norm=NULL
    bart_mae_norm=NULL
    cindex_xgb_norm=NULL
    mse_xgb_norm=NULL
    mae_xgb_norm=NULL
    cindex_ctree_norm=NULL
    mse_ctree_norm=NULL
    mae_ctree_norm=NULL
    cindex_rpart_norm= NULL
    mse_rpart_norm= NULL
    mae_rpart_norm= NULL
    
    cindex_ctree_middle=NULL
    mse_ctree_middle=NULL
    mae_ctree_middle=NULL
    mse_c=NULL
    mae_c=NULL
    cindex_c=NULL
    rate=NULL
    ii=NULL
    #################
    generate_t=function(n,x1,x2,x3,x4){
      #theta=-(cos((x1+x2)*pi)+sqrt(x1+x2))
      #theta=beta1*x1+beta2*x2+beta3*x3+beta4*x4
      t1=beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,2,1)#線性ok k=30
      t2=beta0+(0.5*(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5))+1)^2#box_cox trainsfer
      t3=beta0+log(exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5)))#ph model ok k=30
      t4=NULL
      for (jj in 1:n) {
        t4[jj]=beta0+min(beta1*x1[jj],beta2*x2[jj],beta3*x3[jj],beta4*x4[jj])+rnorm(1,3,0.5)#複雜模型
      }   
      t5=beta0+rexp(n,1.5)*exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4)
      t6=beta0+(beta1*x1+beta2*sin(x2))^0.5+beta3*x3^2+beta4*x2*x3#複雜模型
      return(list(t=t4))
    }
    generate_censoring = function(t, k, censoring_rate) {
      #exam_times =seq(0,quantile(t,0.9), length.out = k + 1)
      delta_t =runif(k, min =min, max =max)  # 從 U[0.3, 0.7] 中取值
      tt=0
      exam_times=NULL
      for (i in 1:(k+1)){
        exam_times[i]=tt
        tt=tt+delta_t[i]
      }
      L =sapply(t, function(t) max(exam_times[exam_times <= t]))
      #R=L+delta_t
      R = exam_times[sapply(L,function(x)which(exam_times==x))+1]
      R[is.na(R)]=Inf
      # 將部分觀測值設置為右設限，根據指定比例 censoring_rate
      right_censor <- runif(n) < censoring_rate
      R[right_censor] <-Inf  # 設定為右設限
      return(list(L = L, R = R,status=as.integer(L<t&t<R&R!=Inf)))  # status 表示是否觀測到事件
    }
    ##################
    i=1
    seed_number=i
    censoring_rate =0#突然失聯的人比例
    now=proc.time()
    while (i<=gg){
      #Sys.sleep(1.5)
      set.seed(seed_number+80)
      #set.seed(85)
      #生成xi
      x1 <- rweibull(n, 0.6, 35)  
      x2 <- rweibull(n, 0.9, 56) 
      x3 <- rweibull(n, 2.6, 43)
      x4 <- rweibull(n, 1, 40)
      # 設定係數 beta
      beta0=  beta[1]
      beta1=  beta[2]
      beta2 = beta[3]
      beta3 = beta[4]
      beta4 = beta[5]
      #製作資料集
      t=generate_t(n,x1,x2,x3,x4)
      censoring= generate_censoring(t$t, k, censoring_rate) 
      d1=data.frame(t=t$t,d=censoring$status,L=censoring$L,R=censoring$R,x1,x2,x3,x4)
      rate[i]=1-(sum(d1$d)/length(d1$L))
      print(type)
      print(i)
      #檢驗資料是否符合coxph assumtion
      #summary(coxph(Surv(t,d) ~ x1+x2+x3+x4, d1)) 
      #sur=survfit(Surv(t,d)~ x1+x2+x3+x4, data=d1)
      #plot(sur)
      #plot(t$t)
      #分組
      s=sample(1:n,0.7*n)
      dtrain=d1[s,]
      dtest=d1[-s,]
      control_params <- partykit::ctree_control(
        minsplit = 20,
        minbucket =5,
        mincriterion = 0.7,
        mtry = 2
      )
      fit=ICtree(Surv(L,R,type="interval2")~x1+x2+x3+x4,dtrain, Control = control_params)
      #plot(fit)
      LTR_pred1=predict(fit, newdata = dtest, type="response")
      LTR_pred2=predict(fit, newdata = dtest, type="prob")
      if(mean((dtest$t-LTR_pred1)^2)!=Inf){
        #plot(fit)
        mse_LTR[i]=mean((dtest$t-LTR_pred1)^2)
        mae_LTR[i]=mean(abs(dtest$t-LTR_pred1))
        cindex_LTR[i]=concordance(t ~ LTR_pred1 , data = dtest)$concordance
        #cat(mse[i])
        #usedtime2[i]=proc.time()-now1
      } else{
        next
      }
      #計算時間
      #now=proc.time()
      #估計參數ic_sp
      cox_fit=ic_sp(Surv(L,R,type = "interval2")~x1+x2+x3+x4,data=dtrain,model = "ph")
      summary(cox_fit)
      b1=cox_fit$coefficients[1]
      b2=cox_fit$coefficients[2]
      b3=cox_fit$coefficients[3]
      b4=cox_fit$coefficients[4]
      c(b1,b2,b3,b4)
      #b5=cox_fit$coefficients[5]
      #b6=cox_fit$coefficients[6]
      #plot(cox_fit)
      x_beta=b1*dtrain$x1+b2*dtrain$x2+b3*dtrain$x3+b4*dtrain$x4
      #x_beta=dtrain$x1+dtrain$x2+dtrain$x3+dtrain$x4
      #x_beta=predict(cox_fit,newdata=dtrain)
      dtrain$x=x_beta
      df_1=subset(dtrain,d==1)
      pred_t=(df_1$L+df_1$R)/2
      df_1$pred_t=pred_t
      df_pred=subset(dtrain,d==0)
      mm=min(df_1$R-df_1$L)
      ma=max(df_1$R-df_1$L)
      m=runif(length(df_pred$t),mm,ma)
      r=range(dtrain$L)[2]-range(dtrain$L)[1]
      gam_l=tryCatch({
        gam(L~s(x,bs="tp"),data=df_1,method = "REML")
      },error=function(e){
        gam_l=NULL
      })
      if(length(gam_l)==0){
        seed_number=seed_number+1
        next
      }
      #gam_r=gam(R~s(x,bs="tp"),data=df_1,method = "REML")
      #gam_t=gam(ht~s(x,bs="tp"),data=df_1,method = "REML")
      #plot(gam_l)
      #plot(gam_r)
      pred_L=predict(gam_l,data.frame(x=df_pred[,"x"]))
      pred_L=pmax(pred_L,df_pred$L)
      #pred_R=predict(gam_r,data.frame(x=df_pred[,"x"]))
      #pred_t=predict(gam_t,data.frame(x=df_pred[,"x"]))
      pred_R=pred_L+m
      df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8])
      if(length(df_pred_r$L)>0){
        df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8],x=0)
      }
      dtrain_1=rbind(df_1[1:9],df_pred_r)
      
      #############################################
      dtrain_2=dtrain_1
      dtrain_2_int=dtrain_2[dtrain_2$d==1,]
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      ###########################################
      #區間的插補
      p=150
      #cforest_pred1=matrix(0,length(dtest$L),p)
      pred_t_n=matrix(0,length(dtrain_2_int$L),p)
      st=NULL
      for(r in 1:length(dtrain_2_int$L)){
        st[r]=runif(1,dtrain_2_int$L[r],dtrain_2_int$R[r])
      }
      for (j in 1:p){
        pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=mean((dtrain_2_int$L+dtrain_2_int$R)/2),sd=sd(st)/3)
        #Cforest <- ICcforest(Surv(L,R,type="interval2")~x1+x2+x3+x4,dtrain_2, Control = control_params)
        #cforest_pred1[,j]=predict(Cforest,dtest, type="response")
      }
      pred_t_norm=rowMeans(pred_t_n)
      dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int)
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      tt=100
      dtrain_2predt=matrix(0,nrow =length(dtrain_2_int$L),ncol = 50 )
      #cforest_pred=rowMeans(cforest_pred1)
      dd=1
      while (dd<cd) {
        pred_t_norm=rowMeans(pred_t_n)
        dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int[-1])
        pred_t2=matrix(0,length(dtrain_2_int$L),tt)
        xgb_impute = xgboost(data = as.matrix(dtrain_2_int [,c(6:9)]),label = as.matrix(dtrain_2_int[,"pred_t"]),nrounds = 80,objective = "reg:squarederror",colsample_bytree = 0.5,eta = 0.1,max_depth = 6,verbose = 0)
        xgb_pred_t_impute = predict(xgb_impute,as.matrix(dtrain_2_int[,c(6:9)]))
        pred_t0=ifelse((dtrain_2_int$R>xgb_pred_t_impute&xgb_pred_t_impute>dtrain_2_int$L),xgb_pred_t_impute,dtrain_2_int$pred_t)
        dtrain_2_int$pred_t=pred_t0
        a=1
        while(a<tt){
          
          #cat(dd,a)
          jj=sample(1:length(dtrain_2_int$L),length(dtrain_2_int$L))
          dt=dtrain_2_int[jj,]
          dpred=dtrain_2_int[jj,]
          gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
          pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
          pred_t2[,a]=pred_t1
          b=dpred$pred_t-pred_t1
          dpred$pred_t=pred_t1
          dtrain_2_int=dpred
          if(mean(abs(b))<0.001){
            break
          }
          a=a+1
          #dtrain_2=rbind(dt,dpred)
          dtrain_2predt[,dd]=dtrain_2_int$pred_t
          if(dd>1&mean(abs(dtrain_2predt[,dd]-dtrain_2predt[,(dd-1)]))<0.001){
            break
          }
        }
        if(dd<(tt-1)){
          for (j in 1:p){
            pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=dtrain_2_int$pred_t,sd=sd(dtrain_2_int$pred_t))
          }
          dd=dd+1
        }
      }
      
      ####################################
      #右設限的插補
      #dtrain_2_right
      if(length(df_pred_r$L)>0){
        ff=tryCatch({
          fitdist(dtrain_2$L+10^-10, "gamma")
        },
        error=function(e){
          ff=NULL
        })
        if(length(ff)==0){
          seed_number=seed_number+1
          next
        }
        e1=ff$estimate[1]
        e2=ff$estimate[2]
        e1=(rate[i])/(1-rate[i])*e1
        e2=(rate[i])/(1-rate[i])*e2
        pred_rr=rgamma(10000000,e1,e2)
        limit=dtrain_2_right$L
        pred_r_norm=NULL
        for(kk in 1:length(dtrain_2_right$L)){
          
          limit_samples=c(pred_rr[pred_rr >= limit[kk]&pred_rr <(limit[kk]+(1.5+rate[i]^2*e1/(e2))*ma)],dtrain_2_right$L[kk])
          #plot(limit_samples)
          pred_r_norm[kk]=mean(sample(limit_samples,20,replace = TRUE))
        }
        dtrain_2_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
      }
      dtrain_2=rbind(dtrain_2_int,dtrain_2_right)
      dtrain_2_right=dtrain_2_right[-1]
      ####################################
      #最後5行的結果
      #dpred$pred_t=rowMeans(pred_t2[,(a-4):a])
      #jj=sample(1:length(dtrain_2$L),length(dtrain_2$L))
      #dt=dtrain_2[jj,]
      #dpred=dtrain_2[jj,]
      #gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
      #pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
      #dpred$pred_t=pred_t1
      #dtrain_2=dpred
      #dtrain_2=rbind(dt,dpred)
      if(length(df_pred_r$L)>0){
        dtrain_3_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
        #pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=dtrain_1[dtrain_1$d==1,]
        pred_t=(dtrain_3$L+dtrain_3$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_3)
        dtrain_3=rbind(dtrain_3,dtrain_3_right)
      }else{
        dtrain_3=dtrain_1
        pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_1)
      }
      #############################文獻中點法
      dtrain_4=dtrain
      pred_t=ifelse(dtrain_4$d==1,(dtrain_4$L+dtrain_4$R)/2,dtrain_4$L)
      dtrain_4=data.frame(pred_t,dtrain_4)
      dtrain_4=dtrain_4[,-(4:5)]
      ctree_middle=ctree(Surv(pred_t,d)~x1+x2+x3+x4,dtrain_4)
      ctree_middle_pred=predict(ctree_middle,dtest)
      cindex_ctree_middle[i]=concordance(t~ctree_middle_pred,dtest)$concordance
      mse_ctree_middle[i]=mean((dtest$t-ctree_middle_pred)^2)
      mae_ctree_middle[i]=mean(abs(dtest$t-ctree_middle_pred))
      #############################決策樹
      reg=tree(pred_t~x1+x2+x3+x4,dtrain_3)
      reg_norm=tree(pred_t~x1+x2+x3+x4,dtrain_2)
      reg_pred_t = predict(reg,newdata=dtest)
      reg_pred_t_norm=predict(reg_norm,newdata=dtest)
      cindex_reg[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_reg[i]=mean((dtest$t-reg_pred_t)^2)
      mae_reg[i]=mean(abs(dtest$t-reg_pred_t))
      cindex_reg_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_reg_norm[i]=mean((dtest$t-reg_pred_t_norm)^2)
      mae_reg_norm[i]=mean(abs(dtest$t-reg_pred_t_norm))
      
      ############################bagging
      bag = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_3 ,importance = TRUE,mtry = 4)
      bag_pred_t = predict(bag,newdata=dtest)
      cindex_bag[i]=concordance(t~ bag_pred_t,data = dtest)$concordance   #######
      mse_bag[i]=mean((dtest$t-bag_pred_t)^2)
      mae_bag[i]=mean(abs(dtest$t-bag_pred_t))
      bag_norm = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_2 ,importance = TRUE,mtry = 4)
      bag_pred_t_norm = predict(bag_norm,newdata=dtest)
      cindex_bag_norm[i]=concordance(t~ bag_pred_t_norm,data = dtest)$concordance   #######
      mse_bag_norm[i]=mean((dtest$t-bag_pred_t_norm)^2)
      mae_bag_norm[i]=mean(abs(dtest$t-bag_pred_t_norm))
      
      ############################RF
      rf = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_3  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t = predict(rf,newdata=dtest)
      cindex_rf[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_rf[i]=mean((dtest$t-rf_pred_t)^2)
      mae_rf[i]=mean(abs(dtest$t-rf_pred_t))
      rf_norm = randomForest(pred_t~x1+x2+x3+x4,data = dtrain_2  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t_norm = predict(rf_norm,newdata=dtest)
      cindex_rf_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_rf_norm[i]=mean((dtest$t-rf_pred_t_norm)^2)
      mae_rf_norm[i]=mean(abs(dtest$t-rf_pred_t_norm))
      
      ###########################Boosting
      boost = gbm(pred_t~x1+x2+x3+x4,data = dtrain_3 ,
                  distribution = "gaussian",n.trees = 1000,
                  interaction.depth = 4) #"gaussian" 選項,因為這是一個回歸問題
      #interaction.depth = 4 限制了每棵樹的深度
      boost_pred_t = predict(boost,newdata=dtest)
      cindex_boost[i]=concordance(t~ boost_pred_t,data = dtest)$concordance   #######
      mse_boost[i]=mean((dtest$t-boost_pred_t)^2)
      mae_boost[i]=mean(abs(dtest$t-boost_pred_t))
      boost_norm = gbm(pred_t~x1+x2+x3+x4,data = dtrain_2 ,
                       distribution = "gaussian",n.trees = 1000,
                       interaction.depth = 4)
      boost_pred_t_norm = predict(boost_norm,newdata=dtest)
      cindex_boost_norm[i]=concordance(t~ boost_pred_t_norm,data = dtest)$concordance   #######
      mse_boost_norm[i]=mean((dtest$t-boost_pred_t_norm)^2)
      mae_boost_norm[i]=mean(abs(dtest$t-boost_pred_t_norm))
      
      ##########################Xgb
      xgb = xgboost(data = as.matrix(dtrain_3 [,c(6:9)]),label = as.matrix(dtrain_3[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=100,verbose = 0)
      xgb_pred_t = predict(xgb,as.matrix(dtest[,c(5:8)]))
      cindex_xgb[i]=concordance(t~ xgb_pred_t,data = dtest)$concordance   #######
      mse_xgb[i]=mean((dtest$t-xgb_pred_t)^2)
      mae_xgb[i]=mean(abs(dtest$t-xgb_pred_t))
      xgb_norm = xgboost(data = as.matrix(dtrain_2 [,c(6:9)]),label = as.matrix(dtrain_2[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=1000,verbose = 0)
      xgb_pred_t_norm = predict(xgb_norm,as.matrix(dtest[,c(5:8)]))
      cindex_xgb_norm[i]=concordance(t~ xgb_pred_t_norm,data = dtest)$concordance   #######
      mse_xgb_norm[i]=mean((dtest$t-xgb_pred_t_norm)^2)
      mae_xgb_norm[i]=mean(abs(dtest$t-xgb_pred_t_norm))
      
      
      ##########################bart
      x <- dtrain_3[, 6:9]
      y <- dtrain_3[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:8]
      bartfit <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat <- bartfit$yhat.test.mean
      bart_cindex[i] = concordance(t ~ bart_yhat, data = dtest)$concordance
      bart_mse[i] = mean((bart_yhat -dtest$t)^2) 
      bart_mae[i] = mean(abs(bart_yhat -dtest$t))
      x <- dtrain_2[, 6:9]
      y <- dtrain_2[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:8]
      bartfit_norm <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat_norm <- bartfit_norm$yhat.test.mean
      bart_cindex_norm[i] = concordance(t ~ bart_yhat_norm, data = dtest)$concordance
      bart_mse_norm[i] = mean((bart_yhat_norm -dtest$t)^2) 
      bart_mae_norm[i] = mean(abs(bart_yhat_norm -dtest$t))
      
      
      ##########################ctree 
      ctree_m=ctree(pred_t~x1+x2+x3+x4,dtrain_3)
      ctree_pred_t = predict(ctree_m,newdata=dtest)
      cindex_ctree[i]=concordance(t~ ctree_pred_t,data = dtest)$concordance   #######
      mse_ctree[i]=mean((dtest$t-ctree_pred_t)^2)
      mae_ctree[i]=mean(abs(dtest$t-ctree_pred_t))
      ctree_m_norm=ctree(pred_t~x1+x2+x3+x4,dtrain_2)
      ctree_pred_t_norm = predict(ctree_m_norm,newdata=dtest)
      cindex_ctree_norm[i]=concordance(t~ ctree_pred_t_norm,data = dtest)$concordance   #######
      mse_ctree_norm[i]=mean((dtest$t-ctree_pred_t_norm)^2)
      mae_ctree_norm[i]=mean(abs(dtest$t-ctree_pred_t_norm))
      
      ##########################
      rpart=rpart(Surv(pred_t,d)~x1+x2+x3+x4,data = dtrain_3,method = "exp")
      tfit= as.party.rpart(rpart)
      rpart_pred_t = predict(tfit ,newdata =  dtest)
      cindex_rpart[i]=concordance(t~ rpart_pred_t,data = dtest)$concordance   #######
      mse_rpart[i]=mean((dtest$t-rpart_pred_t)^2)
      mae_rpart[i]=mean(abs(dtest$t-rpart_pred_t))
      rpart_norm=rpart(Surv(pred_t,d)~x1+x2+x3+x4,data = dtrain_2,method = "exp")
      tfit_norm= as.party.rpart(rpart_norm)
      rpart_pred_t_norm = predict(tfit_norm ,newdata =  dtest)
      cindex_rpart_norm[i]=concordance(t~ rpart_pred_t_norm,data = dtest)$concordance   #######
      mse_rpart_norm[i]=mean((dtest$t-rpart_pred_t_norm)^2)
      mae_rpart_norm[i]=mean(abs(dtest$t-rpart_pred_t_norm))
      ##########################
      #now1=proc.time()
      #Sys.sleep(1)
      #dtrain$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #dtest$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #mse_c[i]=mean((dtest$t-cforest_pred)^2)
      #mae_c[i]=mean(abs(dtest$t-cforest_pred))
      #cindex_c[i]=concordance(t ~ cforest_pred , data = dtest)$concordance
      ii[i]=ifelse((mean((dtest$t-bart_yhat)^2)>mean((dtest$t-bart_yhat_norm)^2)),1 ,0)
      i=i+1
      seed_number=seed_number+1
      print(c("rate"=1-(sum(d1$d)/length(d1$L)),"LTR"=mean((dtest$t-LTR_pred1)^2),"middel"=mean((dtest$t-xgb_pred_t)^2),"norm"=mean((dtest$t-xgb_pred_t_norm)^2)))
    }
    weibull_result=list(c("type  "='weibull',"censored_rate"=mean(rate),"樣本數"=n,"模擬次數"=(i-1),"超越次數"=sum(ii)),
                        "ICtree_result"=list(c("IC_cindex"=mean(cindex_LTR),"IC_mse"=mean(mse_LTR),"IC_mae"=mean(mae_LTR))),
                        "middle_ctree"=list(c("ctree_middle_cindex"=mean(cindex_ctree_middle),"ctree_middle_mse"=mean(mse_ctree_middle),"ctree_middle_mae"=mean(mae_ctree_middle))),
                        "weibull_result_middle"=list("regression"=c("reg_cindex"=mean(cindex_reg),"reg_mse"=mean(mse_reg),"reg_mae"=mean(mae_reg))
                                                     ,"bagging"=c("bag_cindex"=mean(cindex_bag),"bag_mse"=mean(mse_bag),"bag_mae"=mean(mae_bag))
                                                     ,"randomforest"=c("rf_cindex"=mean(cindex_rf),"rf_mse"=mean(mse_rf),"rf_mae"=mean(mae_rf))
                                                     ,"boosting"=c("boost_cindex"=mean(cindex_boost),"boost_mse"=mean(mse_boost),"boost_mae"=mean(mae_boost))
                                                     ,"xgb"=c("xgb_cindex"=mean(cindex_xgb),"xgb_mse"=mean(mse_xgb),"xgb_mae"=mean(mae_xgb))
                                                     ,"bart"=c("bart_cindex"=mean(bart_cindex),"bart_mse"= mean(bart_mse),"bart_mae"= mean(bart_mae))
                                                     ,"ctree"=c("ctree_cindex"=mean(cindex_ctree),"ctree_mse"=mean(mse_ctree),"ctree_mae"=mean(mae_ctree))
                                                     ,"rapart"=c("rpart_cindex"=mean(cindex_rpart),"rpart_mse"=mean(mse_rpart),"rpart_mae"=mean(mae_rpart)))
                        ,"weibull_result_norm"=list("regression"=c("reg_cindex_norm"=mean(cindex_reg_norm),"reg_mse_norm"=mean(mse_reg_norm),"reg_mae_norm"=mean(mae_reg_norm))
                                                    ,"bagging"=c("bag_cindex_norm"=mean(cindex_bag_norm),"bag_mse_norm"=mean(mse_bag_norm),"bag_mae_norm"=mean(mae_bag_norm))
                                                    ,"randomforest"=c("rf_cindex_norm"=mean(cindex_rf_norm),"rf_mse_norm"=mean(mse_rf_norm),"rf_mae_norm"=mean(mae_rf_norm))
                                                    ,"boosting"=c("boost_cindex_norm"=mean(cindex_boost_norm),"boost_mse_norm"=mean(mse_boost_norm),"boost_mae_norm"=mean(mae_boost_norm))
                                                    ,"xgb"=c("xgb_cindex_norm"=mean(cindex_xgb_norm),"xgb_mse_norm"=mean(mse_xgb_norm),"xgb_mae_norm"=mean(mae_xgb_norm))
                                                    ,"bart"=c("bart_cindex_norm"=mean(bart_cindex_norm),"bart_mse_norm"= mean(bart_mse_norm),"bart_mae_norm"= mean(bart_mae_norm))
                                                    ,"ctree"=c("ctree_cindex_norm"=mean(cindex_ctree_norm),"ctree_mse_norm"=mean(mse_ctree_norm),"ctree_mae_norm"=mean(mae_ctree_norm))
                                                    ,"rapart"=c("rpart_cindex_norm"=mean(cindex_rpart_norm),"rpart_mse_norm"=mean(mse_rpart_norm),"rpart_mae_norm"=mean(mae_rpart_norm)))
    )
    return(weibull_result)
  }else if(type=="other2"){
    mae_reg=NULL
    mae_LTR=NULL
    df_pred_r=NULL
    mse_LTR=NULL
    mse_reg=NULL
    cindex_reg=NULL
    cindex_LTR=NULL
    cindex_bag=NULL
    mse_bag=NULL
    mae_bag=NULL
    cindex_rf=NULL
    mse_rf=NULL
    mae_rf=NULL
    cindex_boost=NULL
    mse_boost=NULL
    mae_boost=NULL
    cindex_xgb=NULL
    mse_xgb=NULL
    mae_xgb=NULL
    cindex_ctree=NULL
    mse_ctree=NULL
    mae_ctree=NULL
    cindex_rpart=NULL
    mse_rpart=NULL
    mae_rpart=NULL
    bart_cindex=NULL
    bart_mse= NULL 
    bart_mae= NULL
    cindex_rpart= NULL
    mse_rpart= NULL
    mae_rpart= NULL
    
    
    cindex_reg_norm=NULL
    mse_reg_norm=NULL
    mae_reg_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_boost_norm=NULL
    cindex_rf_norm=NULL
    mse_rf_norm=NULL
    mae_rf_norm=NULL
    mse_boost_norm=NULL
    mae_boost_norm=NULL
    bart_cindex_norm=NULL
    bart_mse_norm=NULL
    bart_mae_norm=NULL
    cindex_xgb_norm=NULL
    mse_xgb_norm=NULL
    mae_xgb_norm=NULL
    cindex_ctree_norm=NULL
    mse_ctree_norm=NULL
    mae_ctree_norm=NULL
    cindex_rpart_norm= NULL
    mse_rpart_norm= NULL
    mae_rpart_norm= NULL
    
    cindex_ctree_middle=NULL
    mse_ctree_middle=NULL
    mae_ctree_middle=NULL
    mse_c=NULL
    mae_c=NULL
    cindex_c=NULL
    rate=NULL
    ii=NULL
    #################
    generate_t=function(n,x1,x2,x3,x4){
      #theta=-(cos((x1+x2)*pi)+sqrt(x1+x2))
      #theta=beta1*x1+beta2*x2+beta3*x3+beta4*x4
      t1=beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,2,1)#線性ok k=30
      t2=beta0+(0.5*(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5))+1)^2#box_cox trainsfer
      t3=beta0+log(exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5)))#ph model ok k=30
      t4=NULL
      for (jj in 1:n) {
        t4[jj]=beta0+min(beta1*x1[jj],beta2*x2[jj],beta3*x3[jj],beta4*x4[jj])+rnorm(1,3,0.5)#複雜模型
      }
      t5=beta0+rexp(n,1.5)*exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4)
      t6=beta0+(beta1*x1+beta2*sin(x2))^0.5+beta3*x3^2+beta4*x2*x3+rnorm(n,3,0.5)#複雜模型
      return(list(t=t6))
    }
    
    generate_censoring = function(t, k, censoring_rate) {
      #exam_times =seq(0,quantile(t,0.9), length.out = k + 1)
      delta_t =runif(k, min =min, max =max)  # 從 U[0.3, 0.7] 中取值
      tt=0
      exam_times=NULL
      for (i in 1:(k+1)){
        exam_times[i]=tt
        tt=tt+delta_t[i]
      }
      L =sapply(t, function(t) max(exam_times[exam_times <= t]))
      #R=L+delta_t
      R = exam_times[sapply(L,function(x)which(exam_times==x))+1]
      R[is.na(R)]=Inf
      # 將部分觀測值設置為右設限，根據指定比例 censoring_rate
      right_censor <- runif(n) < censoring_rate
      R[right_censor] <-Inf  # 設定為右設限
      return(list(L = L, R = R,status=as.integer(L<t&t<R&R!=Inf)))  # status 表示是否觀測到事件
    }
    ##################
    i=1
    seed_number=i
    censoring_rate =0#突然失聯的人比例
    now=proc.time()
    while (i<=gg){
      #Sys.sleep(1.5)
      set.seed(seed_number+80)
      #set.seed(85)
      #生成xi
      x1 <- rnorm(n, 9, 1.5)  
      x2 <- sample(seq(0,2,by=0.1),n,replace = TRUE) 
      x3 <- rpois(n,2)
      x4 <- rbinom(n, size = 1, prob = 0.5)
      # 設定係數 beta
      beta0=beta[1]
      beta1= beta[2]
      beta2 = beta[3]
      beta3 = beta[4]
      beta4 = beta[5]
      #製作資料集
      t=generate_t(n,x1,x2,x3,x4)
      censoring= generate_censoring(t$t, k, censoring_rate) 
      d1=data.frame(t=t$t,d=censoring$status,L=censoring$L,R=censoring$R,x1,x2,x3,x4)
      rate[i]=1-(sum(d1$d)/length(d1$L))
      print(type)
      print(i)
      #檢驗資料是否符合coxph assumtion
      #summary(coxph(Surv(t,d) ~ x1+x2+x3+x4, d1)) 
      #sur=survfit(Surv(t,d)~ x1+x2+x3+x4, data=d1)
      #plot(sur)
      #plot(t$t)
      #分組
      s=sample(1:n,0.7*n)
      dtrain=d1[s,]
      dtest=d1[-s,]
      control_params <- partykit::ctree_control(
        minsplit = 20,
        minbucket =5,
        mincriterion = 0.9,
        mtry = 2
      )
      
      fit=tryCatch({
        ICtree(Surv(L,R,type="interval2")~x1+x2+x3,dtrain, Control = control_params)
      },
      error=function(e){
        fit=NULL
      })
      if(length(fit)==0){
        seed_number=seed_number+1
        next
      }
      #fit=ICtree(Surv(L,R,type="interval2")~x1+x2+x3,dtrain, Control = control_params)
      #plot(fit)
      LTR_pred1=predict(fit, newdata = dtest, type="response")
      LTR_pred2=predict(fit, newdata = dtest, type="prob")
      if(mean((dtest$t-LTR_pred1)^2)!=Inf){
        #plot(fit)
        mse_LTR[i]=mean((dtest$t-LTR_pred1)^2)
        mae_LTR[i]=mean(abs(dtest$t-LTR_pred1))
        cindex_LTR[i]=concordance(t ~ LTR_pred1 , data = dtest)$concordance
        #cat(mse[i])
        #usedtime2[i]=proc.time()-now1
      } else{
        next
      }
      #計算時間
      #now=proc.time()
      #估計參數ic_sp
      cox_fit=ic_sp(Surv(L,R,type = "interval2")~x1+x2+x3,data=dtrain,model = "ph")
      summary(cox_fit)
      b1=cox_fit$coefficients[1]
      b2=cox_fit$coefficients[2]
      b3=cox_fit$coefficients[3]
      #b5=cox_fit$coefficients[5]
      #b6=cox_fit$coefficients[6]
      #plot(cox_fit)
      x_beta=b1*dtrain$x1+b2*dtrain$x2+b3*dtrain$x3
      #x_beta=dtrain$x1+dtrain$x2+dtrain$x3+dtrain$x4
      #x_beta=predict(cox_fit,newdata=dtrain)
      dtrain$x=x_beta
      df_1=subset(dtrain,d==1)
      pred_t=(df_1$L+df_1$R)/2
      df_1$pred_t=pred_t
      df_pred=subset(dtrain,d==0)
      mm=min(df_1$R-df_1$L)
      ma=max(df_1$R-df_1$L)
      m=runif(length(df_pred$t),mm,ma)
      r=range(dtrain$L)[2]-range(dtrain$L)[1]
      gam_l=tryCatch({
        gam(L~s(x,bs="tp"),data=df_1,method = "REML")
      },error=function(e){
        gam_l=NULL
      })
      if(length(gam_l)==0){
        seed_number=seed_number+1
        next
      }
      #gam_r=gam(R~s(x,bs="tp"),data=df_1,method = "REML")
      #gam_t=gam(ht~s(x,bs="tp"),data=df_1,method = "REML")
      #plot(gam_l)
      #plot(gam_r)
      pred_L=predict(gam_l,data.frame(x=df_pred[,"x"]))
      pred_L=pmax(pred_L,df_pred$L)
      #pred_R=predict(gam_r,data.frame(x=df_pred[,"x"]))
      #pred_t=predict(gam_t,data.frame(x=df_pred[,"x"]))
      pred_R=pred_L+m
      df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8])
      if(length(df_pred_r$L)>0){
        df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8],x=0)
      }
      dtrain_1=rbind(df_1[1:9],df_pred_r)
      
      #############################################
      dtrain_2=dtrain_1
      dtrain_2_int=dtrain_2[dtrain_2$d==1,]
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      ###########################################
      #區間的插補
      p=150
      #cforest_pred1=matrix(0,length(dtest$L),p)
      pred_t_n=matrix(0,length(dtrain_2_int$L),p)
      st=NULL
      for(r in 1:length(dtrain_2_int$L)){
        st[r]=runif(1,dtrain_2_int$L[r],dtrain_2_int$R[r])
      }
      for (j in 1:p){
        pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=mean((dtrain_2_int$L+dtrain_2_int$R)/2),sd=sd(st)/3)
        #Cforest <- ICcforest(Surv(L,R,type="interval2")~x1+x2+x3+x4,dtrain_2, Control = control_params)
        #cforest_pred1[,j]=predict(Cforest,dtest, type="response")
      }
      pred_t_norm=rowMeans(pred_t_n)
      dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int)
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      tt=100
      dtrain_2predt=matrix(0,nrow =length(dtrain_2_int$L),ncol = 50 )
      #cforest_pred=rowMeans(cforest_pred1)
      dd=1
      while (dd<cd) {
        pred_t_norm=rowMeans(pred_t_n)
        dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int[-1])
        pred_t2=matrix(0,length(dtrain_2_int$L),tt)
        xgb_impute = xgboost(data = as.matrix(dtrain_2_int [,c(6:8)]),label = as.matrix(dtrain_2_int[,"pred_t"]),nrounds = 80,objective = "reg:squarederror",colsample_bytree = 0.5,eta = 0.1,max_depth = 6,verbose = 0)
        xgb_pred_t_impute = predict(xgb_impute,as.matrix(dtrain_2_int[,c(6:8)]))
        pred_t0=ifelse((dtrain_2_int$R>xgb_pred_t_impute&xgb_pred_t_impute>dtrain_2_int$L),xgb_pred_t_impute,dtrain_2_int$pred_t)
        dtrain_2_int$pred_t=pred_t0
        a=1
        while(a<tt){
          
          #cat(dd,a)
          jj=sample(1:length(dtrain_2_int$L),length(dtrain_2_int$L))
          dt=dtrain_2_int[jj,]
          dpred=dtrain_2_int[jj,]
          gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
          pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
          pred_t2[,a]=pred_t1
          b=dpred$pred_t-pred_t1
          dpred$pred_t=pred_t1
          dtrain_2_int=dpred
          if(mean(abs(b))<0.001){
            break
          }
          a=a+1
          #dtrain_2=rbind(dt,dpred)
          dtrain_2predt[,dd]=dtrain_2_int$pred_t
          if(dd>1&mean(abs(dtrain_2predt[,dd]-dtrain_2predt[,(dd-1)]))<0.001){
            break
          }
        }
        if(dd<(tt-1)){
          for (j in 1:p){
            pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=dtrain_2_int$pred_t,sd=sd(dtrain_2_int$pred_t))
          }
          dd=dd+1
        }
      }
      
      ####################################
      #右設限的插補
      #dtrain_2_right
      if(length(df_pred_r$L)>0){
        ff=tryCatch({
          fitdist(dtrain_2$L+10^-10, "gamma")
        },
        error=function(e){
          ff=NULL
        })
        if(length(ff)==0){
          seed_number=seed_number+1
          next
        }
        e1=ff$estimate[1]
        e2=ff$estimate[2]
        e1=(rate[i])/(1-rate[i])*e1
        e2=(rate[i])/(1-rate[i])*e2
        pred_rr=rgamma(10000000,e1,e2)
        limit=dtrain_2_right$L
        pred_r_norm=NULL
        for(kk in 1:length(dtrain_2_right$L)){
          limit_samples=c(pred_rr[pred_rr >= limit[kk]&pred_rr <(limit[kk]+(1.5+rate[i]^2*e1/(e2))*ma)],dtrain_2_right$L[kk])
          #plot(limit_samples)
          pred_r_norm[kk]=mean(sample(limit_samples,200,replace = TRUE))
        }
        dtrain_2_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
      }
      dtrain_2=rbind(dtrain_2_int,dtrain_2_right)
      dtrain_2_right=dtrain_2_right[-1]
      #print(rate[i]^2.5*e1/e2)
      ####################################
      #最後5行的結果
      #dpred$pred_t=rowMeans(pred_t2[,(a-4):a])
      #jj=sample(1:length(dtrain_2$L),length(dtrain_2$L))
      #dt=dtrain_2[jj,]
      #dpred=dtrain_2[jj,]
      #gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
      #pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
      #dpred$pred_t=pred_t1
      #dtrain_2=dpred
      #dtrain_2=rbind(dt,dpred)
      if(length(df_pred_r$L)>0){
        dtrain_3_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
        #pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=dtrain_1[dtrain_1$d==1,]
        pred_t=(dtrain_3$L+dtrain_3$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_3)
        dtrain_3=rbind(dtrain_3,dtrain_3_right)
      }else{
        dtrain_3=dtrain_1
        pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_1)
      }
      #############################文獻中點法
      dtrain_4=dtrain
      pred_t=ifelse(dtrain_4$d==1,(dtrain_4$L+dtrain_4$R)/2,dtrain_4$L)
      dtrain_4=data.frame(pred_t,dtrain_4)
      ctree_middle=ctree(Surv(pred_t,d)~x1+x2+x3,dtrain_4)
      ctree_middle_pred=predict(ctree_middle,dtest)
      cindex_ctree_middle[i]=concordance(t~ctree_middle_pred,dtest)$concordance
      mse_ctree_middle[i]=mean((dtest$t-ctree_middle_pred)^2)
      mae_ctree_middle[i]=mean(abs(dtest$t-ctree_middle_pred))
      #############################決策樹
      reg=tree(pred_t~x1+x2+x3,dtrain_3)
      reg_norm=tree(pred_t~x1+x2+x3,dtrain_2)
      reg_pred_t = predict(reg,newdata=dtest)
      reg_pred_t_norm=predict(reg_norm,newdata=dtest)
      cindex_reg[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_reg[i]=mean((dtest$t-reg_pred_t)^2)
      mae_reg[i]=mean(abs(dtest$t-reg_pred_t))
      cindex_reg_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_reg_norm[i]=mean((dtest$t-reg_pred_t_norm)^2)
      mae_reg_norm[i]=mean(abs(dtest$t-reg_pred_t_norm))
      
      ############################bagging
      bag = randomForest(pred_t~x1+x2+x3,data = dtrain_3 ,importance = TRUE,mtry = 4)
      bag_pred_t = predict(bag,newdata=dtest)
      cindex_bag[i]=concordance(t~ bag_pred_t,data = dtest)$concordance   #######
      mse_bag[i]=mean((dtest$t-bag_pred_t)^2)
      mae_bag[i]=mean(abs(dtest$t-bag_pred_t))
      bag_norm = randomForest(pred_t~x1+x2+x3,data = dtrain_2 ,importance = TRUE,mtry = 4)
      bag_pred_t_norm = predict(bag_norm,newdata=dtest)
      cindex_bag_norm[i]=concordance(t~ bag_pred_t_norm,data = dtest)$concordance   #######
      mse_bag_norm[i]=mean((dtest$t-bag_pred_t_norm)^2)
      mae_bag_norm[i]=mean(abs(dtest$t-bag_pred_t_norm))
      
      ############################RF
      rf = randomForest(pred_t~x1+x2+x3,data = dtrain_3  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t = predict(rf,newdata=dtest)
      cindex_rf[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_rf[i]=mean((dtest$t-rf_pred_t)^2)
      mae_rf[i]=mean(abs(dtest$t-rf_pred_t))
      rf_norm = randomForest(pred_t~x1+x2+x3,data = dtrain_2  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t_norm = predict(rf_norm,newdata=dtest)
      cindex_rf_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_rf_norm[i]=mean((dtest$t-rf_pred_t_norm)^2)
      mae_rf_norm[i]=mean(abs(dtest$t-rf_pred_t_norm))
      
      ###########################Boosting
      boost = gbm(pred_t~x1+x2+x3,data = dtrain_3 ,
                  distribution = "gaussian",n.trees = 1000,
                  interaction.depth = 4) #"gaussian" 選項,因為這是一個回歸問題
      #interaction.depth = 4 限制了每棵樹的深度
      boost_pred_t = predict(boost,newdata=dtest)
      cindex_boost[i]=concordance(t~ boost_pred_t,data = dtest)$concordance   #######
      mse_boost[i]=mean((dtest$t-boost_pred_t)^2)
      mae_boost[i]=mean(abs(dtest$t-boost_pred_t))
      boost_norm = gbm(pred_t~x1+x2+x3,data = dtrain_2 ,
                       distribution = "gaussian",n.trees = 1000,
                       interaction.depth = 4)
      boost_pred_t_norm = predict(boost_norm,newdata=dtest)
      cindex_boost_norm[i]=concordance(t~ boost_pred_t_norm,data = dtest)$concordance   #######
      mse_boost_norm[i]=mean((dtest$t-boost_pred_t_norm)^2)
      mae_boost_norm[i]=mean(abs(dtest$t-boost_pred_t_norm))
      
      ##########################Xgb
      xgb = xgboost(data = as.matrix(dtrain_3 [,c(6:8)]),label = as.matrix(dtrain_3[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=100,verbose = 0)
      xgb_pred_t = predict(xgb,as.matrix(dtest[,c(5:7)]))
      cindex_xgb[i]=concordance(t~ xgb_pred_t,data = dtest)$concordance   #######
      mse_xgb[i]=mean((dtest$t-xgb_pred_t)^2)
      mae_xgb[i]=mean(abs(dtest$t-xgb_pred_t))
      xgb_norm = xgboost(data = as.matrix(dtrain_2 [,c(6:8)]),label = as.matrix(dtrain_2[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=1000,verbose = 0)
      xgb_pred_t_norm = predict(xgb_norm,as.matrix(dtest[,c(5:7)]))
      cindex_xgb_norm[i]=concordance(t~ xgb_pred_t_norm,data = dtest)$concordance   #######
      mse_xgb_norm[i]=mean((dtest$t-xgb_pred_t_norm)^2)
      mae_xgb_norm[i]=mean(abs(dtest$t-xgb_pred_t_norm))
      
      
      ##########################bart
      x <- dtrain_3[, 6:8]
      y <- dtrain_3[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:7]
      bartfit <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat <- bartfit$yhat.test.mean
      bart_cindex[i] = concordance(t ~ bart_yhat, data = dtest)$concordance
      bart_mse[i] = mean((bart_yhat -dtest$t)^2) 
      bart_mae[i] = mean(abs(bart_yhat -dtest$t))
      x <- dtrain_2[, 6:8]
      y <- dtrain_2[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:7]
      bartfit_norm <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat_norm <- bartfit_norm$yhat.test.mean
      bart_cindex_norm[i] = concordance(t ~ bart_yhat_norm, data = dtest)$concordance
      bart_mse_norm[i] = mean((bart_yhat_norm -dtest$t)^2) 
      bart_mae_norm[i] = mean(abs(bart_yhat_norm -dtest$t))
      
      
      ##########################ctree 
      ctree_m=ctree(pred_t~x1+x2+x3,dtrain_3)
      ctree_pred_t = predict(ctree_m,newdata=dtest)
      cindex_ctree[i]=concordance(t~ ctree_pred_t,data = dtest)$concordance   #######
      mse_ctree[i]=mean((dtest$t-ctree_pred_t)^2)
      mae_ctree[i]=mean(abs(dtest$t-ctree_pred_t))
      ctree_m_norm=ctree(pred_t~x1+x2+x3,dtrain_2)
      ctree_pred_t_norm = predict(ctree_m_norm,newdata=dtest)
      cindex_ctree_norm[i]=concordance(t~ ctree_pred_t_norm,data = dtest)$concordance   #######
      mse_ctree_norm[i]=mean((dtest$t-ctree_pred_t_norm)^2)
      mae_ctree_norm[i]=mean(abs(dtest$t-ctree_pred_t_norm))
      
      ##########################
      rpart=rpart(Surv(pred_t,d)~x1+x2+x3,data = dtrain_3,method = "exp")
      tfit= as.party.rpart(rpart)
      rpart_pred_t = predict(tfit ,newdata =  dtest)
      cindex_rpart[i]=concordance(t~ rpart_pred_t,data = dtest)$concordance   #######
      mse_rpart[i]=mean((dtest$t-rpart_pred_t)^2)
      mae_rpart[i]=mean(abs(dtest$t-rpart_pred_t))
      rpart_norm=rpart(Surv(pred_t,d)~x1+x2+x3,data = dtrain_2,method = "exp")
      tfit_norm= as.party.rpart(rpart_norm)
      rpart_pred_t_norm = predict(tfit_norm ,newdata =  dtest)
      cindex_rpart_norm[i]=concordance(t~ rpart_pred_t_norm,data = dtest)$concordance   #######
      mse_rpart_norm[i]=mean((dtest$t-rpart_pred_t_norm)^2)
      mae_rpart_norm[i]=mean(abs(dtest$t-rpart_pred_t_norm))
      ##########################
      #now1=proc.time()
      #Sys.sleep(1)
      #dtrain$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #dtest$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #mse_c[i]=mean((dtest$t-cforest_pred)^2)
      #mae_c[i]=mean(abs(dtest$t-cforest_pred))
      #cindex_c[i]=concordance(t ~ cforest_pred , data = dtest)$concordance
      ii[i]=ifelse((mean((dtest$t-xgb_pred_t)^2)>mean((dtest$t-xgb_pred_t_norm)^2)),1 ,0)
      i=i+1
      seed_number=seed_number+1
      print(c("rate"=1-(sum(d1$d)/length(d1$L)),"LTR"=mean((dtest$t-LTR_pred1)^2),"middel"=mean((dtest$t-xgb_pred_t)^2),"norm"=mean((dtest$t-xgb_pred_t_norm)^2)))
    }
    other_result=list(c("type  "='other2',"censored_rate"=mean(rate),"樣本數"=n,"模擬次數"=(i-1),"超越次數"=sum(ii)),
                      "ICtree_result"=list(c("IC_cindex"=mean(cindex_LTR),"IC_mse"=mean(mse_LTR),"IC_mae"=mean(mae_LTR))),
                      "middle_ctree"=list(c("ctree_middle_cindex"=mean(cindex_ctree_middle),"ctree_middle_mse"=mean(mse_ctree_middle),"ctree_middle_mae"=mean(mae_ctree_middle))),
                      "other_result_middle"=list("regression"=c("reg_cindex"=mean(cindex_reg),"reg_mse"=mean(mse_reg),"reg_mae"=mean(mae_reg))
                                                 ,"bagging"=c("bag_cindex"=mean(cindex_bag),"bag_mse"=mean(mse_bag),"bag_mae"=mean(mae_bag))
                                                 ,"randomforest"=c("rf_cindex"=mean(cindex_rf),"rf_mse"=mean(mse_rf),"rf_mae"=mean(mae_rf))
                                                 ,"boosting"=c("boost_cindex"=mean(cindex_boost),"boost_mse"=mean(mse_boost),"boost_mae"=mean(mae_boost))
                                                 ,"xgb"=c("xgb_cindex"=mean(cindex_xgb),"xgb_mse"=mean(mse_xgb),"xgb_mae"=mean(mae_xgb))
                                                 ,"bart"=c("bart_cindex"=mean(bart_cindex),"bart_mse"= mean(bart_mse),"bart_mae"= mean(bart_mae))
                                                 ,"ctree"=c("ctree_cindex"=mean(cindex_ctree),"ctree_mse"=mean(mse_ctree),"ctree_mae"=mean(mae_ctree))
                                                 ,"rapart"=c("rpart_cindex"=mean(cindex_rpart),"rpart_mse"=mean(mse_rpart),"rpart_mae"=mean(mae_rpart)))
                      ,"other_result_norm"=list("regression"=c("reg_cindex_norm"=mean(cindex_reg_norm),"reg_mse_norm"=mean(mse_reg_norm),"reg_mae_norm"=mean(mae_reg_norm))
                                                ,"bagging"=c("bag_cindex_norm"=mean(cindex_bag_norm),"bag_mse_norm"=mean(mse_bag_norm),"bag_mae_norm"=mean(mae_bag_norm))
                                                ,"randomforest"=c("rf_cindex_norm"=mean(cindex_rf_norm),"rf_mse_norm"=mean(mse_rf_norm),"rf_mae_norm"=mean(mae_rf_norm))
                                                ,"boosting"=c("boost_cindex_norm"=mean(cindex_boost_norm),"boost_mse_norm"=mean(mse_boost_norm),"boost_mae_norm"=mean(mae_boost_norm))
                                                ,"xgb"=c("xgb_cindex_norm"=mean(cindex_xgb_norm),"xgb_mse_norm"=mean(mse_xgb_norm),"xgb_mae_norm"=mean(mae_xgb_norm))
                                                ,"bart"=c("bart_cindex_norm"=mean(bart_cindex_norm),"bart_mse_norm"= mean(bart_mse_norm),"bart_mae_norm"= mean(bart_mae_norm))
                                                ,"ctree"=c("ctree_cindex_norm"=mean(cindex_ctree_norm),"ctree_mse_norm"=mean(mse_ctree_norm),"ctree_mae_norm"=mean(mae_ctree_norm))
                                                ,"rapart"=c("rpart_cindex_norm"=mean(cindex_rpart_norm),"rpart_mse_norm"=mean(mse_rpart_norm),"rpart_mae_norm"=mean(mae_rpart_norm)))
    )
    return(other_result)
  }else if(type=="other"){
    mae_reg=NULL
    mae_LTR=NULL
    df_pred_r=NULL
    mse_LTR=NULL
    mse_reg=NULL
    cindex_reg=NULL
    cindex_LTR=NULL
    cindex_bag=NULL
    mse_bag=NULL
    mae_bag=NULL
    cindex_rf=NULL
    mse_rf=NULL
    mae_rf=NULL
    cindex_boost=NULL
    mse_boost=NULL
    mae_boost=NULL
    cindex_xgb=NULL
    mse_xgb=NULL
    mae_xgb=NULL
    cindex_ctree=NULL
    mse_ctree=NULL
    mae_ctree=NULL
    cindex_rpart=NULL
    mse_rpart=NULL
    mae_rpart=NULL
    bart_cindex=NULL
    bart_mse= NULL 
    bart_mae= NULL
    cindex_rpart= NULL
    mse_rpart= NULL
    mae_rpart= NULL
    
    
    cindex_reg_norm=NULL
    mse_reg_norm=NULL
    mae_reg_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_bag_norm=NULL
    mse_bag_norm=NULL
    mae_bag_norm=NULL
    cindex_boost_norm=NULL
    cindex_rf_norm=NULL
    mse_rf_norm=NULL
    mae_rf_norm=NULL
    mse_boost_norm=NULL
    mae_boost_norm=NULL
    bart_cindex_norm=NULL
    bart_mse_norm=NULL
    bart_mae_norm=NULL
    cindex_xgb_norm=NULL
    mse_xgb_norm=NULL
    mae_xgb_norm=NULL
    cindex_ctree_norm=NULL
    mse_ctree_norm=NULL
    mae_ctree_norm=NULL
    cindex_rpart_norm= NULL
    mse_rpart_norm= NULL
    mae_rpart_norm= NULL
    
    cindex_ctree_middle=NULL
    mse_ctree_middle=NULL
    mae_ctree_middle=NULL
    mse_c=NULL
    mae_c=NULL
    cindex_c=NULL
    rate=NULL
    ii=NULL
    #################
    generate_t=function(n,x1,x2,x3,x4){
      #theta=-(cos((x1+x2)*pi)+sqrt(x1+x2))
      #theta=beta1*x1+beta2*x2+beta3*x3+beta4*x4
      t1=beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,2,1)#線性ok k=30
      t2=beta0+(0.5*(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5))+1)^2#box_cox trainsfer
      t3=beta0+log(exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4+rnorm(n,3,0.5)))#ph model ok k=30
      t4=NULL
      for (jj in 1:n) {
        t4[jj]=beta0+min(beta1*x1[jj],beta2*x2[jj],beta3*x3[jj],beta4*x4[jj])+rnorm(1,3,0.5)#複雜模型
      }
      t5=beta0+rexp(n,1.5)*exp(beta1*x1+beta2*x2+beta3*x3+beta4*x4)
      t6=beta0+beta1*x1+beta2*sin(x2)+beta3*x3^2+beta4*x2*x3+rnorm(n,2,0.5)#複雜模型
      return(list(t=t6))
    }
    
    
    generate_censoring = function(t, k, censoring_rate) {
      #exam_times =seq(0,quantile(t,0.9), length.out = k + 1)
      delta_t =runif(k, min =min, max =max)  # 從 U[0.3, 0.7] 中取值
      tt=0
      exam_times=NULL
      for (i in 1:(k+1)){
        exam_times[i]=tt
        tt=tt+delta_t[i]
      }
      L =sapply(t, function(t) max(exam_times[exam_times <= t]))
      #R=L+delta_t
      R = exam_times[sapply(L,function(x)which(exam_times==x))+1]
      R[is.na(R)]=Inf
      # 將部分觀測值設置為右設限，根據指定比例 censoring_rate
      right_censor <- runif(n) < censoring_rate
      R[right_censor] <-Inf  # 設定為右設限
      return(list(L = L, R = R,status=as.integer(L<t&t<R&R!=Inf)))  # status 表示是否觀測到事件
    }
    ##################
    i=1
    seed_number=i
    censoring_rate =0#突然失聯的人比例
    now=proc.time()
    while (i<=gg){
      #Sys.sleep(1.5)
      set.seed(seed_number+80)
      #set.seed(85)
      #生成xi
      x1 <- rnorm(n, 4.6, 1)  
      x2 <- sample(seq(0,2,by=0.1),n,replace = TRUE) 
      x3 <- rpois(n,2)
      x4 <- rbinom(n, size = 1, prob = 0.5)#我懶得改 所以有他也無所謂反正沒用到
      # 設定係數 beta
      beta0=beta[1]
      beta1= beta[2]
      beta2 = beta[3]
      beta3 = beta[4]
      beta4 = beta[5]
      #製作資料集
      t=generate_t(n,x1,x2,x3,x4)
      censoring= generate_censoring(t$t, k, censoring_rate) 
      d1=data.frame(t=t$t,d=censoring$status,L=censoring$L,R=censoring$R,x1,x2,x3,x4)
      rate[i]=1-(sum(d1$d)/length(d1$L))
      print(type)
      print(i)
      #檢驗資料是否符合coxph assumtion
      #summary(coxph(Surv(t,d) ~ x1+x2+x3+x4, d1)) 
      #sur=survfit(Surv(t,d)~ x1+x2+x3+x4, data=d1)
      #plot(sur)
      #plot(t$t)
      #分組
      s=sample(1:n,0.7*n)
      dtrain=d1[s,]
      dtest=d1[-s,]
      control_params <- partykit::ctree_control(
        minsplit = 20,
        minbucket =5,
        mincriterion = 0.9,
        mtry = 2
      )
      fit=tryCatch({
        ICtree(Surv(L,R,type="interval2")~x1+x2+x3,dtrain, Control = control_params)
      },
      error=function(e){
        fit=NULL
      })
      if(length(fit)==0){
        seed_number=seed_number+1
        next
      }
      #plot(fit)
      LTR_pred1=predict(fit, newdata = dtest, type="response")
      LTR_pred2=predict(fit, newdata = dtest, type="prob")
      if(mean((dtest$t-LTR_pred1)^2)!=Inf){
        #plot(fit)
        mse_LTR[i]=mean((dtest$t-LTR_pred1)^2)
        mae_LTR[i]=mean(abs(dtest$t-LTR_pred1))
        cindex_LTR[i]=concordance(t ~ LTR_pred1 , data = dtest)$concordance
        #cat(mse[i])
        #usedtime2[i]=proc.time()-now1
      } else{
        next
      }
      #計算時間
      #now=proc.time()
      #估計參數ic_sp
      cox_fit=ic_sp(Surv(L,R,type = "interval2")~x1+x2+x3,data=dtrain,model = "ph")
      summary(cox_fit)
      b1=cox_fit$coefficients[1]
      b2=cox_fit$coefficients[2]
      b3=cox_fit$coefficients[3]
      #b5=cox_fit$coefficients[5]
      #b6=cox_fit$coefficients[6]
      #plot(cox_fit)
      x_beta=b1*dtrain$x1+b2*dtrain$x2+b3*dtrain$x3
      #x_beta=dtrain$x1+dtrain$x2+dtrain$x3+dtrain$x4
      #x_beta=predict(cox_fit,newdata=dtrain)
      dtrain$x=x_beta
      df_1=subset(dtrain,d==1)
      pred_t=(df_1$L+df_1$R)/2
      df_1$pred_t=pred_t
      df_pred=subset(dtrain,d==0)
      mm=min(df_1$R-df_1$L)
      ma=max(df_1$R-df_1$L)
      m=runif(length(df_pred$t),mm,ma)
      r=range(dtrain$L)[2]-range(dtrain$L)[1]
      gam_l=tryCatch({
        gam(L~s(x,bs="tp"),data=df_1,method = "REML")
      },error=function(e){
        gam_l=NULL
      })
      if(length(gam_l)==0){
        seed_number=seed_number+1
        next
      }
      #gam_r=gam(R~s(x,bs="tp"),data=df_1,method = "REML")
      #gam_t=gam(ht~s(x,bs="tp"),data=df_1,method = "REML")
      #plot(gam_l)
      #plot(gam_r)
      pred_L=predict(gam_l,data.frame(x=df_pred[,"x"]))
      pred_L=pmax(pred_L,df_pred$L)
      #pred_R=predict(gam_r,data.frame(x=df_pred[,"x"]))
      #pred_t=predict(gam_t,data.frame(x=df_pred[,"x"]))
      pred_R=pred_L+m
      df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8])
      if(length(df_pred_r$L)>0){
        df_pred_r=data.frame(df_pred[,c(1:2)],L=pred_L,R=pred_R,df_pred[,5:8],x=0)
      }
      dtrain_1=rbind(df_1[1:9],df_pred_r)
      
      #############################################
      dtrain_2=dtrain_1
      dtrain_2_int=dtrain_2[dtrain_2$d==1,]
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      ###########################################
      #區間的插補
      p=150
      #cforest_pred1=matrix(0,length(dtest$L),p)
      pred_t_n=matrix(0,length(dtrain_2_int$L),p)
      st=NULL
      for(r in 1:length(dtrain_2_int$L)){
        st[r]=runif(1,dtrain_2_int$L[r],dtrain_2_int$R[r])
      }
      for (j in 1:p){
        pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=mean((dtrain_2_int$L+dtrain_2_int$R)/2),sd=sd(st)/3)
        #Cforest <- ICcforest(Surv(L,R,type="interval2")~x1+x2+x3+x4,dtrain_2, Control = control_params)
        #cforest_pred1[,j]=predict(Cforest,dtest, type="response")
      }
      pred_t_norm=rowMeans(pred_t_n)
      dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int)
      dtrain_2_right=dtrain_2[dtrain_2$d==0,]
      tt=100
      dtrain_2predt=matrix(0,nrow =length(dtrain_2_int$L),ncol = 50 )
      #cforest_pred=rowMeans(cforest_pred1)
      dd=1
      while (dd<cd) {
        pred_t_norm=rowMeans(pred_t_n)
        dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int[-1])
        pred_t2=matrix(0,length(dtrain_2_int$L),tt)
        xgb_impute = xgboost(data = as.matrix(dtrain_2_int [,c(6:8)]),label = as.matrix(dtrain_2_int[,"pred_t"]),nrounds = 80,objective = "reg:squarederror",colsample_bytree = 0.5,eta = 0.1,max_depth = 6,verbose = 0)
        xgb_pred_t_impute = predict(xgb_impute,as.matrix(dtrain_2_int[,c(6:8)]))
        pred_t0=ifelse((dtrain_2_int$R>xgb_pred_t_impute&xgb_pred_t_impute>dtrain_2_int$L),xgb_pred_t_impute,dtrain_2_int$pred_t)
        dtrain_2_int$pred_t=pred_t0
        a=1
        while(a<tt){
          
          #cat(dd,a)
          jj=sample(1:length(dtrain_2_int$L),length(dtrain_2_int$L))
          dt=dtrain_2_int[jj,]
          dpred=dtrain_2_int[jj,]
          gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
          pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
          pred_t2[,a]=pred_t1
          b=dpred$pred_t-pred_t1
          dpred$pred_t=pred_t1
          dtrain_2_int=dpred
          if(mean(abs(b))<0.001){
            break
          }
          a=a+1
          #dtrain_2=rbind(dt,dpred)
          dtrain_2predt[,dd]=dtrain_2_int$pred_t
          if(dd>1&mean(abs(dtrain_2predt[,dd]-dtrain_2predt[,(dd-1)]))<0.001){
            break
          }
        }
        if(dd<(tt-1)){
          for (j in 1:p){
            pred_t_n[,j]=rtruncnorm(length(dtrain_2_int$d),dtrain_2_int$L,dtrain_2_int$R,mean=dtrain_2_int$pred_t,sd=sd(dtrain_2_int$pred_t))
          }
          dd=dd+1
        }
      }
      
      ####################################
      #右設限的插補
      #dtrain_2_right
      if(length(df_pred_r$L)>0){
        ff=tryCatch({
          fitdist(dtrain_2$L+10^-10, "gamma")
        },
        error=function(e){
          ff=NULL
        })
        if(length(ff)==0){
          seed_number=seed_number+1
          next
        }
        e1=ff$estimate[1]
        e2=ff$estimate[2]
        e1=(rate[i])/(1-rate[i])*e1
        e2=(rate[i])/(1-rate[i])*e2
        pred_rr=rgamma(10000000,e1,e2)
        limit=dtrain_2_right$L
        pred_r_norm=NULL
        for(kk in 1:length(dtrain_2_right$L)){
          limit_samples=c(pred_rr[pred_rr >= limit[kk]&pred_rr <(limit[kk]+(1.5+rate[i]^2*e1/(e2))*ma)],dtrain_2_right$L[kk])
          #plot(limit_samples)
          pred_r_norm[kk]=mean(sample(limit_samples,200,replace = TRUE))
        }
        dtrain_2_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
      }
      dtrain_2=rbind(dtrain_2_int,dtrain_2_right)
      dtrain_2_right=dtrain_2_right[-1]
      #print(rate[i]^2.5*e1/e2)
      ####################################
      #最後5行的結果
      #dpred$pred_t=rowMeans(pred_t2[,(a-4):a])
      #jj=sample(1:length(dtrain_2$L),length(dtrain_2$L))
      #dt=dtrain_2[jj,]
      #dpred=dtrain_2[jj,]
      #gam_t=gam(pred_t~s(x,bs="tp"),data=dt,method = "REML")
      #pred_t1=ifelse((dpred$R>predict(gam_t,dpred)&predict(gam_t,dpred)>dpred$L),predict(gam_t,dpred),dpred$pred_t)
      #dpred$pred_t=pred_t1
      #dtrain_2=dpred
      #dtrain_2=rbind(dt,dpred)
      if(length(df_pred_r$L)>0){
        dtrain_3_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
        #pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=dtrain_1[dtrain_1$d==1,]
        pred_t=(dtrain_3$L+dtrain_3$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_3)
        dtrain_3=rbind(dtrain_3,dtrain_3_right)
      }else{
        dtrain_3=dtrain_1
        pred_t=(dtrain_1$L+dtrain_1$R)/2
        dtrain_3=data.frame(pred_t=pred_t,dtrain_1)
      }
      #############################文獻中點法
      dtrain_4=dtrain
      pred_t=ifelse(dtrain_4$d==1,(dtrain_4$L+dtrain_4$R)/2,dtrain_4$L)
      dtrain_4=data.frame(pred_t,dtrain_4)
      ctree_middle=ctree(Surv(pred_t,d)~x1+x2+x3,dtrain_4)
      ctree_middle_pred=predict(ctree_middle,dtest)
      cindex_ctree_middle[i]=concordance(t~ctree_middle_pred,dtest)$concordance
      mse_ctree_middle[i]=mean((dtest$t-ctree_middle_pred)^2)
      mae_ctree_middle[i]=mean(abs(dtest$t-ctree_middle_pred))
      #############################決策樹
      reg=tree(pred_t~x1+x2+x3,dtrain_3)
      reg_norm=tree(pred_t~x1+x2+x3,dtrain_2)
      reg_pred_t = predict(reg,newdata=dtest)
      reg_pred_t_norm=predict(reg_norm,newdata=dtest)
      cindex_reg[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_reg[i]=mean((dtest$t-reg_pred_t)^2)
      mae_reg[i]=mean(abs(dtest$t-reg_pred_t))
      cindex_reg_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_reg_norm[i]=mean((dtest$t-reg_pred_t_norm)^2)
      mae_reg_norm[i]=mean(abs(dtest$t-reg_pred_t_norm))
      
      ############################bagging
      bag = randomForest(pred_t~x1+x2+x3,data = dtrain_3 ,importance = TRUE,mtry = 4)
      bag_pred_t = predict(bag,newdata=dtest)
      cindex_bag[i]=concordance(t~ bag_pred_t,data = dtest)$concordance   #######
      mse_bag[i]=mean((dtest$t-bag_pred_t)^2)
      mae_bag[i]=mean(abs(dtest$t-bag_pred_t))
      bag_norm = randomForest(pred_t~x1+x2+x3,data = dtrain_2 ,importance = TRUE,mtry = 4)
      bag_pred_t_norm = predict(bag_norm,newdata=dtest)
      cindex_bag_norm[i]=concordance(t~ bag_pred_t_norm,data = dtest)$concordance   #######
      mse_bag_norm[i]=mean((dtest$t-bag_pred_t_norm)^2)
      mae_bag_norm[i]=mean(abs(dtest$t-bag_pred_t_norm))
      
      ############################RF
      rf = randomForest(pred_t~x1+x2+x3,data = dtrain_3  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t = predict(rf,newdata=dtest)
      cindex_rf[i]=concordance(t~ reg_pred_t,data = dtest)$concordance   #######
      mse_rf[i]=mean((dtest$t-rf_pred_t)^2)
      mae_rf[i]=mean(abs(dtest$t-rf_pred_t))
      rf_norm = randomForest(pred_t~x1+x2+x3,data = dtrain_2  ,importance = TRUE ,ntree = 1000,mtry = 2)
      rf_pred_t_norm = predict(rf_norm,newdata=dtest)
      cindex_rf_norm[i]=concordance(t~ reg_pred_t_norm,data = dtest)$concordance   #######
      mse_rf_norm[i]=mean((dtest$t-rf_pred_t_norm)^2)
      mae_rf_norm[i]=mean(abs(dtest$t-rf_pred_t_norm))
      
      ###########################Boosting
      boost = gbm(pred_t~x1+x2+x3,data = dtrain_3 ,
                  distribution = "gaussian",n.trees = 1000,
                  interaction.depth = 4) #"gaussian" 選項,因為這是一個回歸問題
      #interaction.depth = 4 限制了每棵樹的深度
      boost_pred_t = predict(boost,newdata=dtest)
      cindex_boost[i]=concordance(t~ boost_pred_t,data = dtest)$concordance   #######
      mse_boost[i]=mean((dtest$t-boost_pred_t)^2)
      mae_boost[i]=mean(abs(dtest$t-boost_pred_t))
      boost_norm = gbm(pred_t~x1+x2+x3,data = dtrain_2 ,
                       distribution = "gaussian",n.trees = 1000,
                       interaction.depth = 4)
      boost_pred_t_norm = predict(boost_norm,newdata=dtest)
      cindex_boost_norm[i]=concordance(t~ boost_pred_t_norm,data = dtest)$concordance   #######
      mse_boost_norm[i]=mean((dtest$t-boost_pred_t_norm)^2)
      mae_boost_norm[i]=mean(abs(dtest$t-boost_pred_t_norm))
      
      ##########################Xgb
      xgb = xgboost(data = as.matrix(dtrain_3 [,c(6:8)]),label = as.matrix(dtrain_3[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=100,verbose = 0)
      xgb_pred_t = predict(xgb,as.matrix(dtest[,c(5:7)]))
      cindex_xgb[i]=concordance(t~ xgb_pred_t,data = dtest)$concordance   #######
      mse_xgb[i]=mean((dtest$t-xgb_pred_t)^2)
      mae_xgb[i]=mean(abs(dtest$t-xgb_pred_t))
      xgb_norm = xgboost(data = as.matrix(dtrain_2 [,c(6:8)]),label = as.matrix(dtrain_2[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=1000,verbose = 0)
      xgb_pred_t_norm = predict(xgb_norm,as.matrix(dtest[,c(5:7)]))
      cindex_xgb_norm[i]=concordance(t~ xgb_pred_t_norm,data = dtest)$concordance   #######
      mse_xgb_norm[i]=mean((dtest$t-xgb_pred_t_norm)^2)
      mae_xgb_norm[i]=mean(abs(dtest$t-xgb_pred_t_norm))
      
      
      ##########################bart
      x <- dtrain_3[, 6:8]
      y <- dtrain_3[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:7]
      bartfit <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat <- bartfit$yhat.test.mean
      bart_cindex[i] = concordance(t ~ bart_yhat, data = dtest)$concordance
      bart_mse[i] = mean((bart_yhat -dtest$t)^2) 
      bart_mae[i] = mean(abs(bart_yhat -dtest$t))
      x <- dtrain_2[, 6:8]
      y <- dtrain_2[, "pred_t"]
      xtrain <- x
      ytrain <- y
      xtest <- dtest[,5:7]
      bartfit_norm <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
      bart_yhat_norm <- bartfit_norm$yhat.test.mean
      bart_cindex_norm[i] = concordance(t ~ bart_yhat_norm, data = dtest)$concordance
      bart_mse_norm[i] = mean((bart_yhat_norm -dtest$t)^2) 
      bart_mae_norm[i] = mean(abs(bart_yhat_norm -dtest$t))
      
      
      ##########################ctree 
      ctree_m=ctree(pred_t~x1+x2+x3,dtrain_3)
      ctree_pred_t = predict(ctree_m,newdata=dtest)
      cindex_ctree[i]=concordance(t~ ctree_pred_t,data = dtest)$concordance   #######
      mse_ctree[i]=mean((dtest$t-ctree_pred_t)^2)
      mae_ctree[i]=mean(abs(dtest$t-ctree_pred_t))
      ctree_m_norm=ctree(pred_t~x1+x2+x3,dtrain_2)
      ctree_pred_t_norm = predict(ctree_m_norm,newdata=dtest)
      cindex_ctree_norm[i]=concordance(t~ ctree_pred_t_norm,data = dtest)$concordance   #######
      mse_ctree_norm[i]=mean((dtest$t-ctree_pred_t_norm)^2)
      mae_ctree_norm[i]=mean(abs(dtest$t-ctree_pred_t_norm))
      
      ##########################
      rpart=rpart(Surv(pred_t,d)~x1+x2+x3,data = dtrain_3,method = "exp")
      tfit= as.party.rpart(rpart)
      rpart_pred_t = predict(tfit ,newdata =  dtest)
      cindex_rpart[i]=concordance(t~ rpart_pred_t,data = dtest)$concordance   #######
      mse_rpart[i]=mean((dtest$t-rpart_pred_t)^2)
      mae_rpart[i]=mean(abs(dtest$t-rpart_pred_t))
      rpart_norm=rpart(Surv(pred_t,d)~x1+x2+x3,data = dtrain_2,method = "exp")
      tfit_norm= as.party.rpart(rpart_norm)
      rpart_pred_t_norm = predict(tfit_norm ,newdata =  dtest)
      cindex_rpart_norm[i]=concordance(t~ rpart_pred_t_norm,data = dtest)$concordance   #######
      mse_rpart_norm[i]=mean((dtest$t-rpart_pred_t_norm)^2)
      mae_rpart_norm[i]=mean(abs(dtest$t-rpart_pred_t_norm))
      ##########################
      #now1=proc.time()
      #Sys.sleep(1)
      #dtrain$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #dtest$R <- ifelse(dtrain_1$d == 0, 99999, dtrain_1$R)
      #mse_c[i]=mean((dtest$t-cforest_pred)^2)
      #mae_c[i]=mean(abs(dtest$t-cforest_pred))
      #cindex_c[i]=concordance(t ~ cforest_pred , data = dtest)$concordance
      ii[i]=ifelse((mean((dtest$t-xgb_pred_t)^2)>mean((dtest$t-xgb_pred_t_norm)^2)),1 ,0)
      i=i+1
      seed_number=seed_number+1
      print(c("rate"=1-(sum(d1$d)/length(d1$L)),"LTR"=mean((dtest$t-LTR_pred1)^2),"middel"=mean((dtest$t-xgb_pred_t)^2),"norm"=mean((dtest$t-xgb_pred_t_norm)^2)))
    }
    other_result=list(c("type  "='other',"censored_rate"=mean(rate),"樣本數"=n,"模擬次數"=(i-1),"超越次數"=sum(ii)),
                      "ICtree_result"=list(c("IC_cindex"=mean(cindex_LTR),"IC_mse"=mean(mse_LTR),"IC_mae"=mean(mae_LTR))),
                      "middle_ctree"=list(c("ctree_middle_cindex"=mean(cindex_ctree_middle),"ctree_middle_mse"=mean(mse_ctree_middle),"ctree_middle_mae"=mean(mae_ctree_middle))),
                      "other_result_middle"=list("regression"=c("reg_cindex"=mean(cindex_reg),"reg_mse"=mean(mse_reg),"reg_mae"=mean(mae_reg))
                                                 ,"bagging"=c("bag_cindex"=mean(cindex_bag),"bag_mse"=mean(mse_bag),"bag_mae"=mean(mae_bag))
                                                 ,"randomforest"=c("rf_cindex"=mean(cindex_rf),"rf_mse"=mean(mse_rf),"rf_mae"=mean(mae_rf))
                                                 ,"boosting"=c("boost_cindex"=mean(cindex_boost),"boost_mse"=mean(mse_boost),"boost_mae"=mean(mae_boost))
                                                 ,"xgb"=c("xgb_cindex"=mean(cindex_xgb),"xgb_mse"=mean(mse_xgb),"xgb_mae"=mean(mae_xgb))
                                                 ,"bart"=c("bart_cindex"=mean(bart_cindex),"bart_mse"= mean(bart_mse),"bart_mae"= mean(bart_mae))
                                                 ,"ctree"=c("ctree_cindex"=mean(cindex_ctree),"ctree_mse"=mean(mse_ctree),"ctree_mae"=mean(mae_ctree))
                                                 ,"rapart"=c("rpart_cindex"=mean(cindex_rpart),"rpart_mse"=mean(mse_rpart),"rpart_mae"=mean(mae_rpart)))
                      ,"other_result_norm"=list("regression"=c("reg_cindex_norm"=mean(cindex_reg_norm),"reg_mse_norm"=mean(mse_reg_norm),"reg_mae_norm"=mean(mae_reg_norm))
                                                ,"bagging"=c("bag_cindex_norm"=mean(cindex_bag_norm),"bag_mse_norm"=mean(mse_bag_norm),"bag_mae_norm"=mean(mae_bag_norm))
                                                ,"randomforest"=c("rf_cindex_norm"=mean(cindex_rf_norm),"rf_mse_norm"=mean(mse_rf_norm),"rf_mae_norm"=mean(mae_rf_norm))
                                                ,"boosting"=c("boost_cindex_norm"=mean(cindex_boost_norm),"boost_mse_norm"=mean(mse_boost_norm),"boost_mae_norm"=mean(mae_boost_norm))
                                                ,"xgb"=c("xgb_cindex_norm"=mean(cindex_xgb_norm),"xgb_mse_norm"=mean(mse_xgb_norm),"xgb_mae_norm"=mean(mae_xgb_norm))
                                                ,"bart"=c("bart_cindex_norm"=mean(bart_cindex_norm),"bart_mse_norm"= mean(bart_mse_norm),"bart_mae_norm"= mean(bart_mae_norm))
                                                ,"ctree"=c("ctree_cindex_norm"=mean(cindex_ctree_norm),"ctree_mse_norm"=mean(mse_ctree_norm),"ctree_mae_norm"=mean(mae_ctree_norm))
                                                ,"rapart"=c("rpart_cindex_norm"=mean(cindex_rpart_norm),"rpart_mse_norm"=mean(mse_rpart_norm),"rpart_mae_norm"=mean(mae_rpart_norm)))
    )
    return(other_result)
  }
}
betas=matrix(c(c(0,2.5,2.55,1.8,1.3),c(0,1.5,1.55,0.8,0.3),c(0,2.5,2.55,1.8,1.3),c(0,1.5,0.55,0.8,0.3)
               ,c(0,0.5,0.55,0.8,0.3),c(0,0.5,0.55,0.8,0.3),c(0,0.5,0.55,0.8,0.3),c(0,0.5,0.55,0.8,0.3)
               ,c(0,5.5,5.55,5.8,5.3),c(32,1.5,0.5,0.8,0.3),c(0,3.5,3.55,2.8,2.3),c(0,3.55,3.5,2.8,2.3)
               ,c(20,2.5,2.55,1.8,1.3),c(2,1.65,0.7,0.8,0.3),c(4,2,2.5,0.3,1.6),c(8,4,2.5,0.5,1.7),
               c(2,1.3,0.5,0.8,0.3),c(1,0.8,0.5,0.6,0.3),c(2,1.3,0.4,1.1,1)),nrow = 5)
types=c("linear","boxcox","ph","aft","weibull","other","other2")
#auto(200,200,beta =betas[,2],p=150,min=1,max=4,k=3000,cd0,type = types[2] )    )
#------------------------------無設限
gg=500
cd=30
aaa=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=11000,cd,type = types[1])
bbb=auto(gg,200,beta =betas[,2],p=150,min=1,max=4,k=35000,cd,type = types[2])
ccc=auto(gg,200,beta =betas[,3],p=150,min=1,max=4,k=11000,cd,type = types[3])
ddd=auto(gg,200,beta =betas[,4],p=150,min=1,max=4,k=60000,cd,type = types[4])
nnnn=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=14000,cd,type = types[5])
qqqq=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=23000,cd,type = types[6])
uom=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=230000,cd,type = types[7])


eee=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=1800,cd,type = types[1])
fff=auto(gg,200,beta =betas[,6],p=150,min=0.3,max=0.7,k=7000,cd,type = types[2])
ggg=auto(gg,200,beta =betas[,7],p=150,min=0.3,max=0.7,k=2000,cd,type = types[3])
hhh=auto(gg,200,beta =betas[,8],p=150,min=0.3,max=0.7,k=1600,cd,type = types[4])
mmmm=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=2100,cd,type = types[5])
pppp=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=3800,cd,type = types[6])
uos=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=38000,cd,type = types[7])


iiii=auto(gg,200,beta =betas[,9],p=150,min=3,max=7,k=1300,cd,type = types[1])
jjjj=auto(gg,200,beta =betas[,2],p=150,min=3,max=7,k=1800,cd,type = types[2])
kkkk=auto(gg,200,beta =betas[,9],p=150,min=3,max=7,k=1300,cd,type = types[3])
llll=auto(gg,200,beta =betas[,10],p=150,min=3,max=7,k=9000 ,cd,type = types[4])
oooo=auto(gg,200,beta =betas[,1],p=150,min=3,max=7,k=7000,cd,type = types[5])
rrrr=auto(gg,200,beta =betas[,1],p=150,min=3,max=7,k=1100,cd,type = types[6])
uob=auto(gg,200,beta =betas[,1],p=150,min=3,max=7,k=1010,cd,type = types[7])


#_________________________________________________________________________輕度
aa=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=11,cd,type = types[1])
bb=auto(gg,200,beta =betas[,2],p=150,min=1,max=4,k=35,cd,type = types[2])
cc=auto(gg,200,beta =betas[,3],p=150,min=1,max=4,k=11,cd,type = types[3])
dd=auto(gg,200,beta =betas[,4],p=150,min=1,max=4,k=6,cd,type = types[4])
nnn=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=14,cd,type = types[5])
qqq=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=23,cd,type = types[6])
lom=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=19,cd,type = types[7])


ee=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=18,cd,type = types[1])
ff=auto(gg,200,beta =betas[,6],p=150,min=0.3,max=0.7,k=70,cd,type = types[2])
ggg=auto(gg,200,beta =betas[,7],p=150,min=0.3,max=0.7,k=20,cd,type = types[3])
hh=auto(gg,200,beta =betas[,8],p=150,min=0.3,max=0.7,k=16,cd,type = types[4])
mmm=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=21,cd,type = types[5])
ppp=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=38,cd,type = types[6])
los=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=36,cd,type = types[7])


iii=auto(gg,200,beta =betas[,9],p=150,min=3,max=7,k=13,cd,type = types[1])
jjj=auto(gg,200,beta =betas[,2],p=150,min=3,max=7,k=18,cd,type = types[2])
kkk=auto(gg,200,beta =betas[,9],p=150,min=3,max=7,k=13,cd,type = types[3])
lll=auto(gg,200,beta =betas[,10],p=150,min=3,max=7,k=9 ,cd,type = types[4])
ooo=auto(gg,200,beta =betas[,1],p=150,min=3,max=7,k=7,cd,type = types[5])
rrr=auto(gg,200,beta =betas[,1],p=150,min=3,max=7,k=11,cd,type = types[6])
lob=auto(gg,200,beta =betas[,1],p=150,min=3,max=7,k=9,cd,type = types[7])

#------------------------------------------------------------------------------------重度
mm=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=16,cd,type = types[1])
nn=auto(gg,200,beta =betas[,6],p=150,min=0.3,max=0.7,k=60,cd,type = types[2])
oo=auto(gg,200,beta =betas[,7],p=150,min=0.3,max=0.7,k=18,cd,type = types[3])
pp=auto(gg,200,beta =betas[,8],p=150,min=0.3,max=0.7,k=3,cd,type = types[4])
sss=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=14,cd,type = types[5])
vvv=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=24,cd,type = types[6])
hos=auto(gg,200,beta =betas[,5],p=150,min=0.3,max=0.7,k=22,cd,type = types[7])


qq=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=10,cd,type = types[1])
ss=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=10,cd,type = types[3])
qqq=auto(gg,300,beta =betas[,1],p=150,min=1,max=4,k=10,cd,type = types[1])
sss=auto(gg,300,beta =betas[,1],p=150,min=1,max=4,k=10,cd,type = types[3])
oooo=auto(gg,200,beta =betas[,12],p=150,min=3,max=7,k=7000,cd,type = types[5])
ooo=auto(gg,200,beta =betas[,12],p=150,min=3,max=7,k=7,cd,type = types[5])
oooop=auto(gg,300,beta =betas[,12],p=150,min=3,max=7,k=7000,cd,type = types[5])
ooop=auto(gg,300,beta =betas[,12],p=150,min=3,max=7,k=7,cd,type = types[5])

qq=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=10,cd,type = types[1])
rr=auto(gg,200,beta =betas[,2],p=150,min=1,max=4,k=30,cd,type = types[2])
ss=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=10,cd,type = types[3])
tt=auto(gg,200,beta =betas[,4],p=150,min=1,max=4,k=3,cd,type = types[4])
ttt=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=8,cd,type = types[5])
www=auto(gg,200,beta =betas[,1],p=150,min=1,max=4,k=15,cd,type = types[6])
hom=auto(gg,200,beta =betas[,16],p=150,min=1,max=4,k=8,cd,type = types[7])
hom3=auto(gg,300,beta =betas[,16],p=150,min=1,max=4,k=8,cd,type = types[7])



uu=auto(gg,200,beta =betas[,9],p=150,min=3,max=7,k=12,cd,type = types[1])
vv=auto(gg,200,beta =betas[,2],p=150,min=3,max=7,k=15,cd,type = types[2])
ww=auto(gg,200,beta =betas[,9],p=150,min=3,max=7,k=12,cd,type = types[3])
xx=auto(gg,200,beta =betas[,12],p=150,min=3,max=7,k=6,cd,type = types[4])
uuu=auto(gg,200,beta =betas[,12],p=150,min=3,max=7,k=8,cd,type = types[5])
xxx=auto(gg,200,beta =betas[,1],p=150,min=3,max=7,k=4,cd,type = types[6])
hob=auto(gg,200,beta =betas[,1],p=150,min=3,max=7,k=6,cd,type = types[7])

#--------------------------------------------------------

gg=100
cd=30

#---無
ddd=auto(gg,n,beta =betas[,17],p=150,min=1,max=4,k=60000,cd,type = types[4])
hhh=auto(gg,n,beta =betas[,18],p=150,min=0.3,max=0.7,k=1600,cd,type = types[4])
llll=auto(gg,n,beta =betas[,17],p=150,min=3,max=7,k=9000 ,cd,type = types[4])
#輕
dd=auto(gg,n,beta =betas[,17],p=150,min=1,max=4,k=12,cd,type = types[4])
ddd=auto(gg,300,beta =betas[,17],p=150,min=1,max=4,k=12,cd,type = types[4])
hh=auto(gg,n,beta =betas[,18],p=150,min=0.3,max=0.7,k=32,cd,type = types[4])
lll=auto(gg,n,beta =betas[,19],p=150,min=3,max=7,k=6 ,cd,type = types[4])
#重
pp=auto(gg,n,beta =betas[,18],p=150,min=0.3,max=0.7,k=24,cd,type = types[4])
tt=auto(gg,n,beta =betas[,17],p=150,min=1,max=4,k=8,cd,type = types[4])
tt=auto(gg,200,beta =betas[,17],p=150,min=1,max=4,k=12,cd,type = types[4])
xx=auto(gg,200,beta =betas[,19],p=150,min=3,max=7,k=6,cd,type = types[4])
xxx=auto(gg,300,beta =betas[,19],p=150,min=3,max=7,k=6,cd,type = types[4])


selected=function(d){
  result_types <- unique(gsub("_result_(middle|norm)$", "", names(d)))
  key1 <- paste0(result_types[4], "_result_middle")
  key2 <- paste0(result_types[4], "_result_norm")
  mse_valuesm <- sapply(d[[key1]], function(x) x[grep("mse", names(x))])
  mse_valuesn <- sapply(d[[key2]], function(x) x[grep("mse", names(x))])
  
  ############################
  min_mse_model_norm <- names(mse_valuesn)[which.min(mse_valuesn)]
  
  # 從 middle 中提取相同模型的 MSE
  mse_values_middle <- sapply(d[[key1]], function(x) x[grep("mse", names(x))])
  min_mse_model_middle <- gsub("_norm$", "", min_mse_model_norm)
  min_mse_middle <- mse_values_middle[min_mse_model_middle]
  cindexnames=gsub("_mse$", "_cindex",min_mse_model_middle)
  cindexnamen=paste0(cindexnames,"_norm")
  min_mse_model_norm
  min_mse_norm=mse_valuesn[min_mse_model_norm]
  #比較c_index
  c_index_middle=sapply(d[[key1]], function(x) x[grep("cindex", names(x))])
  c_index_norm=sapply(d[[key2]], function(x) x[grep("cindex", names(x))])
  min_mse_middle_cindex=c_index_middle[cindexnames]
  min_mse_norm_cindex=c_index_norm[cindexnamen]
  return(c("是否mse獲勝"=ifelse(min(min_mse_norm)<min(min_mse_middle),"是","否"),"是否cindex獲勝"=ifelse(min(min_mse_middle_cindex)<min(min_mse_norm_cindex),"是","否"),min_mse_norm,min_mse_middle,min_mse_norm_cindex,min_mse_middle_cindex))
}

worl=selected(aaa)

c("無設限","輕設限","重設限",
  "線性","boxcox","ph","aft","weibull","其他","其他2",
  "中區間","小區間","大區間")
keyword=list(aaa,bbb,ccc,ddd,nnnn,qqqq,uom,
             eee,fff,ggg,hhh,mmmm,pppp,uos,
             iiii,jjjj,kkkk,llll,oooo,rrrr,uob,
             aa,bb,cc,dd,nnn,qqq,lom,ee,ff,ggg,hh,mmm,ppp,los,
             iii,jjj,kkk,lll,ooo,rrr,lob,
             mm,nn,oo,pp,sss,vvv,hos,
             qq,rr,ss,tt,ttt,www,hom,uu,vv,ww,xx,uuu,xxx,hob)


worls=list("模擬次數"=gg,'xgam循環次數'=cd,
           "無設限線性中區間"=selected(aaa),"無設限boxcox中區間"=selected(bbb),
           "無設限ph中區間"=selected(ccc),"無設限aft中區間"=selected(ddd),
           "無設限weibull中區間"=selected(nnnn),"無設限其他中區間"=selected(qqqq),
           "無設限其他2中區間"=selected(uom),"無設限線性小區間"=selected(eee),
           "無設限boxcox小區間"=selected(fff),"無設限ph小區間"=selected(ggg),
           "無設限aft小區間"=selected(hhh),"無設限weibull小區間"=selected(mmmm),
           "無設限其他小區間"=selected(pppp),"無設限其他2小區間"=selected(uos),
           "無設限線性大區間"=selected(iiii),"無設限boxcox大區間"=selected(jjjj),
           "無設限ph大區間"=selected(kkkk),"無設限aft大區間"=selected(llll),
           "無設限weibull大區間"=selected(oooo),"無設限其他大區間"=selected(rrrr),
           "無設限其他2大區間"=selected(uob),
           
           "輕設限線性中區間"=selected(aa),"輕設限boxcox中區間"=selected(bb),
           "輕設限ph中區間"=selected(cc),"無設限aft中區間"=selected(dd),
           "輕設限weibull中區間"=selected(nnn),"輕設限其他中區間"=selected(qqq),
           "輕設限其他2中區間"=selected(lom),"輕設限線性小區間"=selected(ee),
           "輕設限boxcox小區間"=selected(ff),"輕設限ph小區間"=selected(ggg),
           "輕設限aft小區間"=selected(hh),"輕設限weibull小區間"=selected(mmm),
           "輕設限其他小區間"=selected(ppp),"輕設限其他2小區間"=selected(los),
           "輕設限線性大區間"=selected(iii),"輕設限boxcox大區間"=selected(jjj),
           "輕設限ph大區間"=selected(kkk),"輕設限aft大區間"=selected(lll),
           "輕設限weibull大區間"=selected(ooo),"輕設限其他大區間"=selected(rrr),
           "輕設限其他2大區間"=selected(lob),
           
           "重設限線性小區間"=selected(mm),"重設限boxcox小區間"=selected(nn),
           "重設限ph小區間"=selected(oo),"重設限aft小區間"=selected(pp),
           "重設限weibull小區間"=selected(sss),"重設限其他小區間"=selected(vvv),
           "重設限其他2小區間"=selected(hos),"重設限線性中區間"=selected(qq),
           "重設限boxcox中區間"=selected(rr),"重設限ph中區間"=selected(ss),
           "重設限aft中區間"=selected(tt),"重設限weibull中區間"=selected(ttt),
           "重設限其他中區間"=selected(www),"重設限其他2中區間"=selected(hom),
           "重設限線性大區間"=selected(uu),"重設限boxcox大區間"=selected(vv),
           "重設限ph大區間"=selected(ww),"重設限aft大區間"=selected(xx),
           "重設限weibull大區間"=selected(uuu),"重設限其他大區間"=selected(xxx),
           "重設限其他2大區間"=selected(hob)
)
