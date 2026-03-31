library(bayesSurv)
rdata=data(tandmob2)
print(rdata)
tandmob2$BAD.83
L=tandmob2$EBEG.24
R=tandmob2$EEND.24
L[which(is.na(L))]=5
R[which(is.na(R))]=Inf
b1=tandmob2$BAD.53
b2=tandmob2$BAD.63
b3=tandmob2$BAD.73
b4=tandmob2$BAD.83
b5=tandmob2$BAD.54
b6=tandmob2$BAD.64
b7=tandmob2$BAD.74
b8=tandmob2$BAD.84
b9=tandmob2$BAD.55
b10=tandmob2$BAD.65
b11=tandmob2$BAD.75
b12=tandmob2$BAD.85

t1=tandmob2$T53.DMF
t2=tandmob2$T63.DMF
t3=tandmob2$T73.DMF
t4=tandmob2$T83.DMF
t5=tandmob2$T54.DMF
t6=tandmob2$T64.DMF
t7=tandmob2$T74.DMF
t8=tandmob2$T84.DMF
t9=tandmob2$T55.DMF
t10=tandmob2$T65.DMF
t11=tandmob2$T75.DMF
t12=tandmob2$T85.DMF

c1=tandmob2$T53.CAR
c2=tandmob2$T63.CAR
c3=tandmob2$T73.CAR
c4=tandmob2$T83.CAR
c5=tandmob2$T54.CAR
c6=tandmob2$T64.CAR
c7=tandmob2$T74.CAR
c8=tandmob2$T84.CAR
c9=tandmob2$T55.CAR
c10=tandmob2$T65.CAR
c11=tandmob2$T75.CAR
c12=tandmob2$T85.CAR
gender=c()
badtooth=c()
dtooth=c()
ctooth=c()
items=c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12",
        "c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12",
        "t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12")
#for(item in items){
#  itemname=get(item)
#  itemname[which(is.na(itemname))]=0
#  assign(item,itemname)
#}

for(i in 1:length(tandmob2$IDNR)){
  badtooth[i]=b1[i]+b2[i]+b3[i]+b4[i]+b5[i]+b6[i]+b7[i]+b8[i]+b9[i]+b10[i]+b11[i]+b12[1]
  dtooth[i]=t1[i]+t2[i]+t3[i]+t4[i]+t5[i]+t6[i]+t7[i]+t8[i]+t9[i]+t10[i]+t11[i]+t12[1]
  ctooth[i]=c1[i]+c2[i]+c3[i]+c4[i]+c5[i]+c6[i]+c7[i]+c8[i]+c9[i]+c10[i]+c11[i]+c12[1]
}
tandmob2

#gender=1為女孩,badtooth為12顆牙中有多少顆蛀牙被拔除,dtooth為爛掉或填充或缺失
#ctooth為恆牙出現時的第一次檢查之前的最後一次檢查中，乳牙 xx 是否由於正畸原因而被拔除或腐爛。
#左右設限的單位為年

rdata=data.frame("L"=L,"R"=R,"gender"=tandmob2$GENDERNum,"province"=tandmob2$PROVINCE,
                 "educ"=tandmob2$EDUC,"starbr"=tandmob2$STARTBR,"fluor"=tandmob2$FLUOR
                 ,"badtooth"=badtooth,"dtooth"=dtooth,"ctooth"=ctooth)
rdata=rdata[which(!is.na(rowSums(rdata))),]
#write.csv(rdata,"C:/Users/user/Desktop/教授的論文/景鴻/0325/data.csv")
#-------------------------------------------------------------------------------
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
ii=NULL
#################

generate_censoring = function(d) {
  L =d$L
  R=d$R
  return(list(status=as.integer(R!=Inf)))  # status 表示是否觀測到事件
}
##################
now=proc.time()
#製作資料集
set.seed(1)
censoring= generate_censoring(rdata)
d1=data.frame(d=censoring$status,rdata)
rate[i]=1-(sum(d1$d)/length(d1$L))
#分組
s=sample(1:length(rdata$L),0.7*length(rdata$L))
dtrain=d1[s,]
dtest=d1[-s,]
control_params <- partykit::ctree_control(
  minsplit = 20,
  minbucket =5,
  mincriterion = 0.7,
  mtry = 2
)
x1=dtrain$gender
x2=dtrain$province
x3=dtrain$educ
x4=dtrain$starbr
x5=dtrain$fluor
x6=dtrain$badtooth
x7=dtrain$dtooth
x8=dtrain$ctooth
fit=ICtree(Surv(L,R,type="interval2")~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,dtrain, Control = control_params)
plot(fit)
LTR_pred1=predict(fit, newdata = dtest, type="response")
#LTR_pred2=predict(fit, newdata = dtest, type="prob")
cox_fit=ic_sp(Surv(L,R,type = "interval2")~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data=dtrain,model = "ph")
summary(cox_fit)
b1=cox_fit$coefficients[1]
b2=cox_fit$coefficients[2]
b3=cox_fit$coefficients[3]
b4=cox_fit$coefficients[4]
b5=cox_fit$coefficients[5]
b6=cox_fit$coefficients[6]
b7=cox_fit$coefficients[7]
b8=cox_fit$coefficients[8]
c(b1,b2,b3,b4,b5,b6,b7,b8)
#b5=cox_fit$coefficients[5]
#b6=cox_fit$coefficients[6]
#plot(cox_fit)
x_beta=b1*x1+b2*x2+b3*x3+b4*x4+b5*x5+b6*x6+b7*x7+b8*x8
#x_beta=dtrain$x1+dtrain$x2+dtrain$x3+dtrain$x4
#x_beta=predict(cox_fit,newdata=dtrain)
dtrain$x=x_beta
df_1=subset(dtrain,d==1)
pred_t=(df_1$L+df_1$R)/2
df_1$pred_t=pred_t
df_pred=subset(dtrain,d==0)
mm=min(df_1$R-df_1$L)
ma=max(df_1$R-df_1$L)
m=runif(length(df_pred$L),mm,ma)
r=range(dtrain$L)[2]-range(dtrain$L)[1]
gam_l=gam(L~s(x,bs="tp"),data=df_1,method = "REML")
pred_L=predict(gam_l,data.frame(x=df_pred[,"x"]))
pred_L=pmax(pred_L,df_pred$L)
#pred_R=predict(gam_r,data.frame(x=df_pred[,"x"]))
#pred_t=predict(gam_t,data.frame(x=df_pred[,"x"]))
pred_R=pred_L+m
df_pred_r=data.frame(d=df_pred[,c(1:1)],L=pred_L,R=pred_R,df_pred[,4:12])
if(length(df_pred_r$L)>0){
  df_pred_r=data.frame(d=df_pred[,"d"],L=pred_L,R=pred_R,df_pred[,4:12])
}
dtrain_1=rbind(df_1[1:12],df_pred_r)

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
cd=30
while (dd<cd) {
  pred_t_norm=rowMeans(pred_t_n)
  dtrain_2_int=data.frame(pred_t=pred_t_norm,dtrain_2_int[-1])
  pred_t2=matrix(0,length(dtrain_2_int$L),tt)
  xgb_impute = xgboost(data = as.matrix(dtrain_2_int [,c(4:11)]),label = as.matrix(dtrain_2_int[,"pred_t"]),nrounds = 80,objective = "reg:squarederror",colsample_bytree = 0.5,eta = 0.1,max_depth = 6,verbose = 0)
  xgb_pred_t_impute = predict(xgb_impute,as.matrix(dtrain_2_int[,c(4:11)]))
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
rate=1-(length(dtrain[dtrain$d==1,]$d)/length(dtrain$d))
if(length(df_pred_r$L)>0){
  ff=tryCatch({
    fitdist(dtrain_2$L+10^-10, "gamma")
  },
  error=function(e){
    ff=NULL
  })
  if(length(ff)==0){
    #seed_number=seed_number+1
    next
  }
  e1=ff$estimate[1]
  e2=ff$estimate[2]
  e1=(rate)/(1-rate)*e1
  e2=(rate)/(1-rate)*e2
  pred_rr=rgamma(10000000,e1,e2)
  limit=dtrain_2_right$L
  pred_r_norm=NULL
  for(kk in 1:length(dtrain_2_right$L)){
    
    limit_samples=c(pred_rr[pred_rr >= limit[kk]&pred_rr <(limit[kk]+(1.5+rate^2*e1/(e2))*ma)],dtrain_2_right$L[kk])
    #plot(limit_samples)
    pred_r_norm[kk]=mean(sample(limit_samples,20,replace = TRUE))
  }
  dtrain_2_right=data.frame(pred_t=pred_r_norm,dtrain_2_right)
}
dtrain_2=rbind(dtrain_2_int,dtrain_2_right)
dtrain_2_right=dtrain_2_right[-1]
#############################################
#挑選學習方式
    index_c=sample(length(dtrain_2$d),0.7*length(dtrain_2$d))
    dtrain_2_int_fold=dtrain_2[index_c,]
    #dtrain_2_int_fold=dtrain_2[dtrain_2$d==1,]
    dtest_int_fold=dtrain_2[-index_c,]
    #dtest_int_fold=dtest[dtest$d==1,]
    #t_middle=((dtest_int_fold$L+dtest_int_fold$R)/2)
    t_middle=dtest_int_fold$pred_t
    #t_middle=NULL
    #for(tta in 1:length(dtest_int_fold$L)){
    #  t_middle[tta]=(runif(1,dtest_int_fold$L[tta],dtest_int_fold$R[tta]))
    #}
    reg_norm=tree(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,dtrain_2_int_fold)
    reg_pred_t = predict(reg_norm,newdata=dtest_int_fold)
    mse_reg=mean((t_middle-reg_pred_t)^2)
    
    bag_norm = randomForest(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_2_int_fold ,importance = TRUE,mtry = 4)
    bag_pred_t_norm = predict(bag_norm,newdata=dtest_int_fold)
    mse_bag_norm=mean((t_middle-bag_pred_t_norm)^2)
    
    rf_norm = randomForest(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_2_int_fold  ,importance = TRUE ,ntree = 1000,mtry = 2)
    rf_pred_t_norm = predict(rf_norm,newdata=dtest_int_fold)
    mse_rf_norm=mean((t_middle-rf_pred_t_norm)^2)
    
    boost_norm = gbm(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_2_int_fold ,
                     distribution = "gaussian",n.trees = 1000,
                     interaction.depth = 4)
    boost_pred_t_norm = predict(boost_norm,newdata=dtest_int_fold)
    mse_boost_norm=mean((t_middle-boost_pred_t_norm)^2)
    
    xgb_norm = xgboost(data = as.matrix(dtrain_2_int_fold [,c(5:12)]),label = as.matrix(dtrain_2_int_fold[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=1000)
    xgb_pred_t_norm = predict(xgb_norm,as.matrix(dtest_int_fold[,c(5:12)]))
    mse_xgb_norm=mean((t_middle-xgb_pred_t_norm)^2)
    
    
    xtrain <- dtrain_2_int_fold[, 5:12]
    ytrain <- dtrain_2_int_fold[, "pred_t"]
    xtest <- dtest_int_fold[,5:12]
    bartfit_norm <- gbart(xtrain, ytrain, x.test = xtest)
    bart_yhat_norm <- bartfit_norm$yhat.test.mean
    mse_bart_norm=mean((t_middle-bart_yhat_norm)^2)
    
    ctree_m_norm=ctree(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,dtrain_2_int_fold)
    ctree_pred_t_norm = predict(ctree_m_norm,newdata=dtest_int_fold)
    mse_ctree_norm=mean((t_middle-ctree_pred_t_norm)^2)
    
    rpart_norm=rpart(Surv(pred_t,d)~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_2_int_fold,method = "exp")
    tfit_norm= as.party.rpart(rpart_norm)
    rpart_pred_t_norm = predict(tfit_norm ,newdata =  dtest_int_fold)
    mse_rpart_norm=mean((t_middle-rpart_pred_t_norm)^2)
    compare=list("reg"=mse_reg,"bag"=mse_bag_norm,"rf"=mse_rf_norm,"boosting"=mse_boost_norm,"xgb"=mse_xgb_norm
                 ,"bart"=mse_bart_norm,"ctree"=mse_ctree_norm,"rpart"=mse_rpart_norm)
    method=names(which.min(compare))
    print(method)
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
#註解:ctree可以用來處理右設限資料，所以我們將ctree的y視作middle、ctree的c視作L 這樣子帶入
dtrain_4=dtrain
pred_t=ifelse(dtrain_4$d==1,(dtrain_4$L+dtrain_4$R)/2,dtrain_4$L)
dtrain_4=data.frame(pred_t,dtrain_4)
#dtrain_4=dtrain_4[,-(4:5)]
ctree_middle=ctree(Surv(pred_t,d)~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,dtrain_4)
ctree_middle_pred=predict(ctree_middle,dtest)
#############################決策樹
if(method=="reg"){
  reg=tree(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,dtrain_3)
  reg_norm=tree(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,dtrain_2)
  reg_pred_t = predict(reg,newdata=dtest)
  pred_true_middle=reg_pred_t
  reg_pred_t_norm=predict(reg_norm,newdata=dtest)
  pred_true_gamma=reg_pred_t_norm
}else if(method=="bag"){
############################bagging
  bag = randomForest(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_3 ,importance = TRUE,mtry = 4)
  bag_pred_t = predict(bag,newdata=dtest)
  pred_true_middle=bag_pred_t
  bag_norm = randomForest(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_2 ,importance = TRUE,mtry = 4)
  bag_pred_t_norm = predict(bag_norm,newdata=dtest)
  pred_true_gamma=bag_pred_t_norm
}else if(method=="rf"){
############################RF
  rf = randomForest(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_3  ,importance = TRUE ,ntree = 1000,mtry = 2)
  rf_pred_t = predict(rf,newdata=dtest)
  pred_true_middle=rf_pred_t
  rf_norm = randomForest(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_2  ,importance = TRUE ,ntree = 1000,mtry = 2)
  rf_pred_t_norm = predict(rf_norm,newdata=dtest)
  pred_true_gamma=rf_pred_t_norm
}else if(method=="boosting"){
###########################Boosting
  boost = gbm(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_3 ,
              distribution = "gaussian",n.trees = 1000,
              interaction.depth = 4) #"gaussian" 選項,因為這是一個回歸問題
  #interaction.depth = 4 限制了每棵樹的深度
  boost_pred_t = predict(boost,newdata=dtest)
  pred_true_middle=boost_pred_t
  boost_norm = gbm(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_2 ,
                   distribution = "gaussian",n.trees = 1000,
                   interaction.depth = 4)
  boost_pred_t_norm = predict(boost_norm,newdata=dtest)
  pred_true_gamma=boost_pred_t_norm
}else if(method=="xgb"){
##########################Xgb
  xgb = xgboost(data = as.matrix(dtrain_3 [,c(5:12)]),label = as.matrix(dtrain_3[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=100,verbose = 0)
  xgb_pred_t = predict(xgb,as.matrix(dtest[,c(4:11)]))
  pred_true_middle=xgb_pred_t
  xgb_norm = xgboost(data = as.matrix(dtrain_2 [,c(5:12)]),label = as.matrix(dtrain_2[,"pred_t"]),nrounds = 100,objective = "reg:squarederror",mtry=1000,verbose = 0)
  xgb_pred_t_norm = predict(xgb_norm,as.matrix(dtest[,c(4:11)]))
  pred_true_gamma=xgb_pred_t_norm
}else if(method=="bart"){

##########################bart
  x <- dtrain_3[, 5:12]
  y <- dtrain_3[, "pred_t"]
  xtrain <- x
  ytrain <- y
  xtest <- dtest[,4:11]
  bartfit <- gbart(xtrain, ytrain, x.test = xtest,printevery=1100L)
  bart_yhat <- bartfit$yhat.test.mean
  pred_true_middle=bartfit$yhat.test.mean
  x <- dtrain_2[, 5:12]
  y <- dtrain_2[, "pred_t"]
  xtrain <- x
  ytrain <- y
  xtest <- dtest[,4:11]
  bartfit_norm <- gbart(xtrain, ytrain, x.test = xtest,,printevery=1100L)
  bart_yhat_norm <- bartfit_norm$yhat.test.mean
  pred_true_gamma=bartfit_norm$yhat.test.mean
}else if(method=="ctree"){

##########################ctree 
  ctree_m=ctree(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,dtrain_3)
  ctree_pred_t = predict(ctree_m,newdata=dtest)
  pred_true_middle=ctree_pred_t
  ctree_m_norm=ctree(pred_t~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,dtrain_2)
  ctree_pred_t_norm = predict(ctree_m_norm,newdata=dtest)
  pred_true_gamma=ctree_pred_t_norm
}else if(method=="rpart"){
##########################
  rpart=rpart(Surv(pred_t,d)~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_3,method = "exp")
  tfit= as.party.rpart(rpart)
  rpart_pred_t = predict(tfit ,newdata =  dtest)
  pred_true_middle=rpart_pred_t
  rpart_norm=rpart(Surv(pred_t,d)~gender+province+educ+starbr+fluor+badtooth+dtooth+ctooth,data = dtrain_2,method = "exp")
  tfit_norm= as.party.rpart(rpart_norm)
  rpart_pred_t_norm = predict(tfit_norm ,newdata =  dtest)
  pred_true_gamma=rpart_pred_t_norm
}
##########################
print(c("方法"=method,"rate"=1-(sum(d1$d)/length(d1$L)),"LTR"=var(LTR_pred1),"ctree"=var(ctree_middle_pred),"middel"=var(pred_true_middle),"gamma"=var(pred_true_gamma)))
print(c("方法"=method,"rate"=1-(sum(d1$d)/length(d1$L)),"LTR"=mean(LTR_pred1),"ctree"=mean(ctree_middle_pred),"middel"=mean(pred_true_middle),"gamma"=mean(pred_true_gamma)))

dtest$IC=LTR_pred1
dtest$middle=pred_true_middle
dtest$gam=pred_true_gamma
dtest$ctree=ctree_middle_pred
iccounts=0
middlecounts=0
gamcounts=0
ccounts=0

for(i in 1:length(dtest[dtest$d==1,]$d)){
  iccounts=iccounts+ifelse(dtest[dtest$d==1,]$IC[i]>dtest[dtest$d==1,]$L[i]&dtest[dtest$d==1,]$IC[i]<dtest[dtest$d==1,]$R[i],1,0)
  middlecounts=middlecounts+ifelse(dtest[dtest$d==1,]$middle[i]>dtest[dtest$d==1,]$L[i]&dtest[dtest$d==1,]$middle[i]<dtest[dtest$d==1,]$R[i],1,0)
  gamcounts=gamcounts+ifelse(dtest[dtest$d==1,]$gam[i]>dtest[dtest$d==1,]$L[i]&dtest[dtest$d==1,]$gam[i]<dtest[dtest$d==1,]$R[i],1,0)
  ccounts=ccounts+ifelse(dtest[dtest$d==1,]$ctree[i]>dtest[dtest$d==1,]$L[i]&dtest[dtest$d==1,]$ctree[i]<dtest[dtest$d==1,]$R[i],1,0)
}

ic=iccounts/length(dtest[dtest$d==1,]$d)
mid=middlecounts/length(dtest[dtest$d==1,]$d)
gamm=gamcounts/length(dtest[dtest$d==1,]$d)
ct=ccounts/length(dtest[dtest$d==1,]$d)


cc=0
iccounts=0
middlecounts=0
gamcounts=0
ccounts=0


m=max(dtest[dtest$d==1,]$R-dtest[dtest$d==1,]$L)

for(i in 1:length(dtest$d)){
  iccounts=iccounts+ifelse(dtest$IC[i]>=dtest$L[i]-cc*m&dtest$IC[i]<=dtest$R[i]+cc*m,1,0)
  middlecounts=middlecounts+ifelse(dtest$middle[i]>=dtest$L[i]-cc*m&dtest$middle[i]<=dtest$R[i]+cc*m,1,0)
  gamcounts=gamcounts+ifelse(dtest$gam[i]>=dtest$L[i]-cc*m&dtest$gam[i]<=dtest$R[i]+cc*m,1,0)
  ccounts=ccounts+ifelse(dtest$ctree[i]>=dtest$L[i]-cc*m&dtest$ctree[i]<=dtest$R[i]+cc*m,1,0)
}

ic1=iccounts/length(dtest$d)
mid2=middlecounts/length(dtest$d)
gamm3=gamcounts/length(dtest$d)
ct4=ccounts/length(dtest$d)
cat(ic1,mid2,gamm3,ct4)
  
d=list(IC=sort(LTR_pred1),middle=sort(pred_true_middle),gam=sort(pred_true_gamma),ctree=sort(dtest$ctree))
boxplot(d)
abline(h=median(dtest$L),col="blue",lty = 2, lwd = 2)
abline(h=median(dtest$R),col="red",lty = 2, lwd = 2)

library(ggplot2)
library(dplyr)
library(tidyr)
#全體----------------------
df <- data.frame(
  Index = 1:length(LTR_pred1),
  LTR_pred1 = sort(dtest$IC),
  pred_true_middle = sort(dtest$middle),
  pred_true_gamma = sort(dtest$gam),
  ctree_pred=sort(dtest$ctree)
)

# 轉為長格式 (long format)
df_long <- df %>%
  pivot_longer(cols = -Index, names_to = "Group", values_to = "Value")



# 設定線型對應表
line_styles <- c(
  LTR_pred1 = "dotted",         # lty = 4 in base R
  pred_true_middle = "dashed",   # lty = 3
  pred_true_gamma = "dashed" ,   # lty = 2
  ctree_pred="dotted"
)

# 畫出 ggplot 折線圖
ggplot(df_long, aes(x = Index, y = Value, color = Group, linetype = Group)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = line_styles) +
  scale_color_manual(values = c("purple","blue" ,"green", "red")) +
  ylim(min(dtest$L), max(dtest[dtest$d==1,]$R)) +
  labs(title = " Prediction Curves",
       x = "Index", y = "Predicted Value") +
  geom_hline(yintercept = quantile(dtest[dtest$d==1,]$R, probs = 0.75), linetype = "solid", color = "black", size = 0.8,)+
  geom_hline(yintercept = quantile(dtest[dtest$d==1,]$L, probs = 0.25), linetype = "solid", color = "black", size = 0.8)+
  annotate("text",x=350,y=quantile(dtest[dtest$d==1,]$R, probs = 0.75)+0.3,label="完全區間右端點的0.75百分位數",color="black")+
  annotate("text",x=350,y=quantile(dtest[dtest$d==1,]$L, probs = 0.25)-0.3,label="完全區間左端點的0.25百分位數",color="black")
theme_minimal()
#性別------------------------
time_transform <- function(d, type){
  ft <- function(d, time_col){
    library(tidyverse)
    
    time_col <- enquo(time_col)  # 把變數轉成 quosure
    
    dtest1 <- d %>%
      mutate(age_group = cut(!!time_col,
                             breaks = seq(floor(9), ceiling(13) + 0.3, by = 0.15),
                             include.lowest = TRUE,
                             right = FALSE),
             hit = ifelse(!!time_col > 0.5, 1, 0))  # 加 hit
    
    summary_age <- dtest1 %>%
      group_by(age_group) %>%
      summarise(count = sum(hit)/nrow(dtest1), .groups = "drop")
    
    summary_age$age_mid <- sapply(as.character(summary_age$age_group), function(x) {
      bounds <- as.numeric(gsub("\\[|\\]|\\(|\\)", "", unlist(strsplit(x, ","))))
      mean(bounds)
    })
    
    return(summary_age)
  }
  
  if(type == "IC"){
    a = ft(d, LTR_pred1)
  } else if(type == "middle"){
    a = ft(d, pred_true_middle)
  } else if(type == "gam"){
    a = ft(d, pred_true_gamma)
  } else if(type == "ctree"){
    a = ft(d, ctree_pred)
  }
  
  return(a)
}
gplot=function(s,names,groups){
  ggplot(s, aes(x = age_mid, y = count,color = group)) +
    geom_line(size = 1.2) +
    geom_point(size=3)+
    theme_minimal() +
    labs(title = paste0(groups,"每0.15歲年區間的 ", names,"> 0.5 個數"),
         x = "年齡（以 0.075 為區間中點）",
         y = "符合條件的個數")
}
types=c("IC","middle","gam","ctree")
groups=c("gender","educ","fluor")
#時間轉換函數
df1<- data.frame(
  Index_girl = 1:length(dtest[dtest$gender==1,]$IC),
  LTR_pred1 = sort(dtest[dtest$gender==1,]$IC),
  pred_true_middle = sort(dtest[dtest$gender==1,]$middle),
  pred_true_gamma = sort(dtest[dtest$gender==1,]$gam),
  ctree_pred=sort(dtest[dtest$gender==1,]$ctree)
)
df2<- data.frame(
  Index_boy = 1:length(dtest[dtest$gender==0,]$IC),
  LTR_pred1  = sort(dtest[dtest$gender==0,]$IC),
  pred_true_middle  = sort(dtest[dtest$gender==0,]$middle),
  pred_true_gamma  = sort(dtest[dtest$gender==0,]$gam),
  ctree_pred =sort(dtest[dtest$gender==0,]$ctree)
)

#df1=dtest[order(dtest$L),]
library(tidyverse)

# 加上性別欄位
df1$gender <- "girl"
df2$gender <- "boy"

# 將 Index 欄名稱統一（否則 pivot_longer 會報錯）
df1 <- df1 %>% rename(Index = Index_girl)
df2 <- df2 %>% rename(Index = Index_boy)
#############################################

for(j in 1:4){
  aa=time_transform(df1,types[j])
  bb=time_transform(df2,types[j])
  aa$group="girl"
  bb$group="boy"
  s_all=rbind(aa,bb)
  assign(paste0("s_all",j),s_all)
  assign(paste0("a", j), aa)
  assign(paste0("b", j), bb)
}

##############################################
gplot(s_all1,types[1],groups[1])#ic
gplot(s_all2,types[2],groups[1])#middle
gplot(s_all3,types[3],groups[1])#gam
gplot(s_all4,types[4],groups[1])#ctree
# 合併資料
df_combined <- bind_rows(df1, df2)

df_long <- df_combined %>%
  pivot_longer(cols = -c(Index, gender),
               names_to = "Model",
               values_to = "Value")
ggplot(df_long, aes(x = Index, y = Value, color = gender, group = gender)) +
  geom_line(size = 0.6) +
  facet_wrap(~ Model, scales = "free_y") +
  theme_minimal() +
  labs(title = "Model Comparison by Gender",
       x = "Index",
       y = "Prediction Value")

#教育程度-----------------------------------  
df1<- data.frame(
  Index_low = 1:length(dtest[dtest$educ==0,]$IC),
  LTR_pred1 = sort(dtest[dtest$educ==0,]$IC),
  pred_true_middle = sort(dtest[dtest$educ==0,]$middle),
  pred_true_gamma = sort(dtest[dtest$educ==0,]$gam),
  ctree_pred=sort(dtest[dtest$educ==0,]$ctree)
)
df2<- data.frame(
  Index_normal = 1:length(dtest[dtest$educ==1,]$IC),
  LTR_pred1 = sort(dtest[dtest$educ==1,]$IC),
  pred_true_middle = sort(dtest[dtest$educ==1,]$middle),
  pred_true_gamma = sort(dtest[dtest$educ==1,]$gam),
  ctree_pred=sort(dtest[dtest$educ==1,]$ctree)
)
df3=data.frame(
  Index_high = 1:length(dtest[dtest$educ==2,]$IC),
  LTR_pred1 = sort(dtest[dtest$educ==2,]$IC),
  pred_true_middle = sort(dtest[dtest$educ==2,]$middle),
  pred_true_gamma = sort(dtest[dtest$educ==2,]$gam),
  ctree_pred=sort(dtest[dtest$educ==2,]$ctree)
)
#df1=dtest[order(dtest$L),]


# 加上性別欄位
df1$edu <- "Free"
df2$edu <- "community_school"
df3$edu="Province_school"

# 將 Index 欄名稱統一（否則 pivot_longer 會報錯）
df1 <- df1 %>% rename(Index = Index_low)
df2 <- df2 %>% rename(Index = Index_normal)
df3 =  df3 %>% rename(Index = Index_high)
############################################
for(j in 1:4){
  aa=time_transform(df1,types[j])
  bb=time_transform(df2,types[j])
  cc=time_transform(df3,types[j])
  aa$group="free"
  bb$group="community_school"
  cc$group="Province_school"
  s_all=rbind(aa,bb,cc)
  assign(paste0("s_all",j),s_all)
  assign(paste0("a", j), aa)
  assign(paste0("b", j), bb)
  assign(paste0("c", j), cc)
}

gplot(s_all1,types[1],groups[2])#ic
gplot(s_all2,types[2],groups[2])#middle
gplot(s_all3,types[3],groups[2])#gam
gplot(s_all4,types[4],groups[2])#ctree
###################################################
# 合併資料# 合併資料# 合併資料
df_combined <- bind_rows(df1, df2,df3)

df_long <- df_combined %>%
  pivot_longer(cols = -c(Index, edu),
               names_to = "Model",
               values_to = "Value")
ggplot(df_long, aes(x = Index, y = Value, color = edu, group = edu)) +
  geom_line(size = 0.6) +
  facet_wrap(~ Model, scales = "free_y") +
  theme_minimal() +
  labs(title = "Model Comparison by educ",
       x = "Index",
       y = "Prediction Value")





#fluor--------------------

df1<- data.frame(
  Index_noncontain = 1:length(dtest[dtest$fluor==0,]$IC),
  LTR_pred1 = sort(dtest[dtest$fluor==0,]$IC),
  pred_true_middle = sort(dtest[dtest$fluor==0,]$middle),
  pred_true_gamma = sort(dtest[dtest$fluor==0,]$gam),
  ctree_pred=sort(dtest[dtest$fluor==0,]$ctree)
)
df2<- data.frame(
  Index_contain = 1:length(dtest[dtest$fluor==1,]$IC),
  LTR_pred1 = sort(dtest[dtest$fluor==1,]$IC),
  pred_true_middle = sort(dtest[dtest$fluor==1,]$middle),
  pred_true_gamma = sort(dtest[dtest$fluor==1,]$gam),
  ctree_pred=sort(dtest[dtest$fluor==1,]$ctree)
)
#df1=dtest[order(dtest$L),]

# 加上性別欄位
df1$fluor <- "noncontain"
df2$fluor <- "contain"


# 將 Index 欄名稱統一（否則 pivot_longer 會報錯）
df1 <- df1 %>% rename(Index = Index_noncontain)
df2 <- df2 %>% rename(Index = Index_contain)
############################################
for(j in 1:4){
  aa=time_transform(df1,types[j])
  bb=time_transform(df2,types[j])
  aa$group="noncontain"
  bb$group="contain"
  s_all=rbind(aa,bb)
  assign(paste0("s_all",j),s_all)
  assign(paste0("a", j), aa)
  assign(paste0("b", j), bb)
}

gplot(s_all1,types[1],groups[3])#ic
gplot(s_all2,types[2],groups[3])#middle
gplot(s_all3,types[3],groups[3])#gam
gplot(s_all4,types[4],groups[3])#ctree
###################################################
# 合併資料
df_combined <- bind_rows(df1, df2)

df_long <- df_combined %>%
  pivot_longer(cols = -c(Index, fluor),
               names_to = "Model",
               values_to = "Value")
ggplot(df_long, aes(x = Index, y = Value, color = fluor, group = fluor)) +
  geom_line(size = 0.6) +
  facet_wrap(~ Model, scales = "free_y") +
  theme_minimal() +
  labs(title = "Model Comparison by fluor",
       x = "Index",
       y = "Prediction Value")

