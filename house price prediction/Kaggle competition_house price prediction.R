library('compiler')
train<-read.csv('~/Documents/Kaggle/House_Prices/train.csv',stringsAsFactors = FALSE)
test<-read.csv('~/Documents/Kaggle/House_Prices/test.csv',stringsAsFactors = FALSE)

library('plyr')
all<-rbind.fill(train,test)
rawdata<-rbind.fill(train,test)

Y<-c('YearRemodAdd','YearBuilt','GarageYrBlt')
for(j in Y){
for (i in 1:nrow(all)){
  if(is.na(all[i,j])){
    all[i,j]=NA 
  }else if(all[i,j]>2010){
    all[i,j]=NA
  }else if(all[i,j]<1800){
    all[i,j]=NA    
  }else if(all[i,j]>all[i,'YrSold']){
    all[i,j]=NA
  }else{NULL}
}
}


all$YearRemodAdd<-all$YrSold-all$YearRemodAdd
all$YearBuilt<-all$YrSold-all$YearBuilt
all$GarageYrBlt<-all$YrSold-all$GarageYrBlt
all$Bathabove<-(all$HalfBath*0.5)+all$FullBath
all$Bsmbath<-(all$BsmtHalfBath*0.5)+all$BsmtFullBath
all$Totalbath<-all$Bathabove+all$Bsmbath

library('lubridate')
all$YrSold<-2011-all$YrSold

for(j in Y){
  for (i in 1:nrow(all)){
    if(is.na(all[i,j])){
      all[i,j]=NA 
    }else if(all[i,j]<0){
      all[i,j]=NA
    }else{NULL}
  }
}




#轉換ranking chracter converision
likert_col1<-c('BsmtFinType1','BsmtFinType2')
likert_col1_ref<-c('BsmtFinSF1','BsmtFinSF1')
j=1
for(j in 1:length(likert_col1)){
  for(i in 1:nrow(all)){
    if (is.na(all[i,likert_col1[j]])&&is.na(all[i,likert_col1_ref[j]])){
      all[i,likert_col1[j]]=NA
    }else if (is.na(all[i,likert_col1[j]])&&all[i,likert_col1_ref[j]]==0){
      all[i,likert_col1[j]]=0
    }else if (all[i,likert_col1[j]]==0&&all[i,likert_col1_ref[j]]==0){
      all[i,likert_col1[j]]=0
    }else if (all[i,likert_col1[j]]!=0&&all[i,likert_col1_ref[j]]==0){
      all[i,likert_col1[j]]=NA
    }else if (is.na(all[i,likert_col1[j]])&&all[i,likert_col1_ref[j]]!=0){
      all[i,likert_col1[j]]=all[i,likert_col1[j]]
    }else if (all[i,likert_col1[j]]=='GLQ'&&all[i,likert_col1_ref[j]]>0){
      all[i,likert_col1[j]]=6
    }else if(all[i,likert_col1[j]]=='ALQ'&&all[i,likert_col1_ref[j]]>0){
      all[i,likert_col1[j]]=5
    }else if(all[i,likert_col1[j]]=='BLQ'&&all[i,likert_col1_ref[j]]>0){
      all[i,likert_col1[j]]=4
    }else if(all[i,likert_col1[j]]=='Rec'&&all[i,likert_col1_ref[j]]>0){
      all[i,likert_col1[j]]=3
    }else if(all[i,likert_col1[j]]=='LwQ'&&all[i,likert_col1_ref[j]]>0){
      all[i,likert_col1[j]]=2
    }else if(all[i,likert_col1[j]]=='Unf'&&all[i,likert_col1_ref[j]]>0){
      all[i,likert_col1[j]]=1
    }else{NULL
    }
  }
}

likert_col2<-c('ExterQual','ExterCond','HeatingQC','KitchenQual')
for(k in likert_col2){
  for(i in 1:nrow(all)){
    if(is.na(all[i,k])){
      all[i,k]=NA
    }else if (all[i,k]=='Ex'){
      all[i,k]=5
    }else if(all[i,k]=='Gd'){
      all[i,k]=4
    }else if(all[i,k]=='TA'){
      all[i,k]=3
    }else if(all[i,k]=='Fa'){
      all[i,k]=2
    }else if(all[i,k]=='Po'){
      all[i,k]=1
    }else{NULL
    }
  }
}

likert_col3<-c('BsmtQual','BsmtCond','FireplaceQu','GarageQual','GarageCond','PoolQC')
likert_col3_ref<-c('TotalBsmtSF','TotalBsmtSF','Fireplaces','GarageArea','GarageArea','PoolArea')
for(j in 1:length(likert_col3)){
  for(i in 1:nrow(all)){
    if (is.na(all[i,likert_col3[j]])&&is.na(all[i,likert_col3_ref[j]])){
      all[i,likert_col3[j]]=NA
    }else if (is.na(all[i,likert_col3[j]])&&all[i,likert_col3_ref[j]]==0){
      all[i,likert_col3[j]]=0
    }else if (all[i,likert_col3[j]]==0&&all[i,likert_col3_ref[j]]==0){
      all[i,likert_col3[j]]=0
    }else if (all[i,likert_col3[j]]!=0&&all[i,likert_col3_ref[j]]==0){
      all[i,likert_col3[j]]=NA
    }else if (is.na(all[i,likert_col3[j]])&&all[i,likert_col3_ref[j]]!=0){
      all[i,likert_col3[j]]=all[i,likert_col3[j]]
    }else if(all[i,likert_col3[j]]=='Ex'&&all[i,likert_col3_ref[j]]>0){
      all[i,likert_col3[j]]=5
    }else if(all[i,likert_col3[j]]=='Gd'&&all[i,likert_col3_ref[j]]>0){
      all[i,likert_col3[j]]=4
    }else if(all[i,likert_col3[j]]=='TA'&&all[i,likert_col3_ref[j]]>0){
      all[i,likert_col3[j]]=3
    }else if(all[i,likert_col3[j]]=='Fa'&&all[i,likert_col3_ref[j]]>0){
      all[i,likert_col3[j]]=2
    }else if(all[i,likert_col3[j]]=='Po'&&all[i,likert_col3_ref[j]]>0){
      all[i,likert_col3[j]]=1
    }else{NULL
    }
  }
}

for(i in 1:nrow(all)){
  if( is.na(all[i,'BsmtExposure'])&&is.na(all[i,'TotalBsmtSF'])){
    all[i,'BsmtExposure']=NA
  }else if( is.na(all[i,'BsmtExposure'])&&all[i,'TotalBsmtSF']==0){
    all[i,'BsmtExposure']=0
  }else if( all[i,'BsmtExposure']==0&&all[i,'TotalBsmtSF']==0){
    all[i,'BsmtExposure']=0
  }else if( all[i,'BsmtExposure']!=0&&all[i,'TotalBsmtSF']==0){
    all[i,'BsmtExposure']=NA
  }else if( is.na(all[i,'BsmtExposure'])&&all[i,'TotalBsmtSF']!=0){
    all[i,'BsmtExposure']=all[i,'BsmtExposure']
  }else if (all[i,'BsmtExposure']=='Gd'&&all[i,'TotalBsmtSF']>0){
    all[i,'BsmtExposure']=4
  }else if( all[i,'BsmtExposure']=='Av'&&all[i,'TotalBsmtSF']>0){
    all[i,'BsmtExposure']=3
  }else if( all[i,'BsmtExposure']=='Mn'&&all[i,'TotalBsmtSF']>0){
    all[i,'BsmtExposure']=2
  }else if( all[i,'BsmtExposure']=='No'&&all[i,'TotalBsmtSF']>0){
    all[i,'BsmtExposure']=1
  }else{NULL
  }
}

for(i in 1:nrow(all)){
  if (is.na(all[i,'Fence'])){
    all[i,'Fence']=0
  }else if (all[i,'Fence']=='GdPrv'){
    all[i,'Fence']=4
  }else if( all[i,'Fence']=='MnPrv'){
    all[i,'Fence']=3
  }else if( all[i,'Fence']=='GdWo'){
    all[i,'Fence']=2
  }else if( all[i,'Fence']=='MnWw'){
    all[i,'Fence']=1
  }else{NULL
  }
}

likert_col4<-c('MiscFeature','Alley')
for(l in likert_col4 )
for(i in 1:nrow(all)){
  if (is.na(all[i,l])){
    all[i,l]=0
  }else{NULL
  }
}

tranfered<-c('BsmtFinType1','BsmtFinType2','ExterQual','ExterCond','HeatingQC','KitchenQual','BsmtQual','BsmtCond','FireplaceQu','GarageQual','GarageCond','PoolQC','BsmtExposure','Fence')


#transfer integer to numeric
convert_func<-function(x){as.numeric(as.character(x))}
convert_int<-function(x){as.integer(as.character(x))}
all[,tranfered]<- sapply(all[,tranfered], convert_int)
col_num<-c()
col_chr<-c()
for(i in 1:length(all)){
  if (class(all[[i]])=='integer'){
    col_num[i]<-colnames(all[i])
    }else if(class(all[[i]])=='numeric'){
      col_num[i]<-colnames(all[i])
      }else {NULL
  }
}
for(i in 1:length(all)){
  if (class(all[[i]])=='character'){
    col_chr[i]<-colnames(all[i])
  }else {NULL
  }
}
col_num<-col_num[!is.na(col_num)]
col_chr<-col_chr[!is.na(col_chr)]
all[,col_num]<- sapply(all[,col_num], convert_func)




library('mice')
library('VIM')
#手動算na
na_all<-data.frame()
for(i in 1:length(all)){
  na_all[i,1]<-i
  na_all[i,2]<-colnames(all[i])
  na_all[i,3]<-sum(is.na(all[i])=='TRUE')
  na_all[i,4]<-class(all[[i]])
}

for(i in 1:nrow(na_all)){
  na_all[i,'ratio']<-paste(round((na_all[i,'V3']/2919)*100,2),'%')
}
colnames(na_all)<-c('Index','val','na_num','type')
na_all<-na_all[na_all$na_num!=0,]
na_all<-na_all[order(na_all$na_num,decreasing = T),]
"lar_miss<-head(na_all$Index) #遺失過多要刪除的項目
lar_miss<-lar_miss[!lar_miss==81]  #扣除SalesPrice"
#Na check auto
"all.mis <- missForest::prodNA(all, noNA=0.1) 
summary(all.mis)
md.pattern(all.mis)
aggr(all.mis,labels=names(all.mis), numbers=T, sortVars=T, 
     cex.axis=.7, cex.numbers=0.7, cex.lab=1.2,
     ylab=c('缺項比率','缺項樣式'))"

outlier<-all[all$GrLivArea>4000,]
all_reg<-all[-c(as.integer(rownames(all[all$GrLivArea>4000,]))),]


all_no_miss_num<-all_reg[,col_num] #-missing col & ouliner observations
all_no_miss_num<-all_no_miss_num[complete.cases(all_no_miss_num),]
#correlation
cor(all_no_miss_num)
'heatmap'
'================================='
install.packages('reshape2')
library(reshape2)
library(ggplot2)
'================================='
relation_c<-melt(cor(all_no_miss_num))
r<-relation_c[0.5<relation_c$value,]
'base<-nrow(relation_c[0.5<relation_c$value,])+1
end<-nrow(relation_c[(-0.5)>relation_c$value,])+base-1'
r[228:277,]<-relation_c[(-0.5)>relation_c$value,] #from base to end
r<-r[r$value<1,]
r<-r[r$value>(-1),]
price<-r[r$Var1=="SalePrice",]

num_sig<-as.character(price[[2]])
num_trans<-all[,num_sig]

all_reg
no_miss<-all_reg[complete.cases(all_reg),]
plot(all_reg$LotFrontage,all_reg$SalePrice)

'===========Based on chr test transfer data to numeric================'
all_adj<-all#make a copy

#d<-c('Street','Alley','LandSlope','Condition2','RoofMatl','Heating','Functional','PavedDrive','MiscFeature','condition1','RoofStyle','Neighborhood','MSZoning','BldgType','Exterior1st','Exterior2nd') 欲刪除


adj<-c('reglot','banked','CulDSac','PConc','CentralAir','SBrkr','BuiltInGar','New','Partial','HouseStyle','MasVnrType','GarageFinish')


r<-c('LotShape','LandContour','LotConfig','Foundation','CentralAir','Electrical','GarageType','SaleType','SaleCondition')
n<-c('reglot','banked','CulDSac','PConc','CentralAir','SBrkr','BuiltInGar','New','Partial')
m<-c('Reg','Bnk','CulDSac','PConc','Y','SBrkr','BuiltIn','New','Partial')


for(p in 1:length(r)){
  for(i in 1:nrow(all_adj) ){
    if (is.na(all_adj[i,r[p]])){
      all_adj[i,n[p]]=NA
    }else if (all_adj[i,r[p]]==m[p]){
      all_adj[i,n[p]]=1
    }else{all_adj[i,n[p]]=0
    }
  }
}
all_adj[,n]<- sapply(all_adj[,n], convert_int)

#HouseStyle
for(i in 1:length(all_adj$HouseStyle)){
  if (is.na(all_adj[i,'HouseStyle'])){
    all_adj[i,'HouseStyle']=NA
  }else if(all_adj[i,'HouseStyle']=='1Story'){
    all_adj[i,'HouseStyle']=1
  }else if(all_adj[i,'HouseStyle']=='1.5Fin'){
    all_adj[i,'HouseStyle']=2
  }else if(all_adj[i,'HouseStyle']=='1.5Unf'){
    all_adj[i,'HouseStyle']=2
  }else if(all_adj[i,'HouseStyle']=='2.5Unf'){
    all_adj[i,'HouseStyle']=3
  }else if(all_adj[i,'HouseStyle']=='2.5Fin'){
    all_adj[i,'HouseStyle']=3
  }else if(all_adj[i,'HouseStyle']=='2Story'){
    all_adj[i,'HouseStyle']=3
  }else if(all_adj[i,'HouseStyle']=='SFoyer'&&all_adj[i,'X2ndFlrSF']==0){
    all_adj[i,'HouseStyle']=2
  }else if(all_adj[i,'HouseStyle']=='SFoyer'&&all_adj[i,'X2ndFlrSF']>0){
    all_adj[i,'HouseStyle']=3
  }else if(all_adj[i,'HouseStyle']=='SLvl'&&all_adj[i,'X2ndFlrSF']==0){
    all_adj[i,'HouseStyle']=2
  }else if(all_adj[i,'HouseStyle']=='SLvl'&&all_adj[i,'X2ndFlrSF']>0){
    all_adj[i,'HouseStyle']=3
  }else{NULL
  }
}
all_adj[,'HouseStyle']<- sapply(all_adj[,'HouseStyle'],convert_int)

#MasVnrType
for(i in 1:length(all_adj$MasVnrType)){
  if (is.na(all_adj[i,'MasVnrType'])){
    all_adj[i,'MasVnrType']=NA
  }else if (is.na(all_adj[i,'MasVnrArea'])){
    all_adj[i,'MasVnrType']=NA
  }else if (all_adj[i,'MasVnrType']!='None'&&all_adj[i,'MasVnrArea']==0){
    all_adj[i,'MasVnrType']=NA
  }else if (all_adj[i,'MasVnrType']=='None'&&all_adj[i,'MasVnrArea']==0){
    all_adj[i,'MasVnrType']=0
  }else if (all_adj[i,'MasVnrType']=='None'&&all_adj[i,'MasVnrArea']>0){
    all_adj[i,'MasVnrType']=NA
  }else if (all_adj[i,'MasVnrType']=='BrkCmn'&&all_adj[i,'MasVnrArea']>0){
    all_adj[i,'MasVnrType']=1
  }else if (all_adj[i,'MasVnrType']=='BrkFace'&&all_adj[i,'MasVnrArea']>0){
    all_adj[i,'MasVnrType']=2   
  }else if (all_adj[i,'MasVnrType']=='Stone'&&all_adj[i,'MasVnrArea']>0){
    all_adj[i,'MasVnrType']=3
  }else{NULL
  }
}

all_adj[,'MasVnrType']<- sapply(all_adj[,'MasVnrType'], convert_int)

 #GarageFinish
for(i in 1:length(all_adj$GarageFinish)){
  if (is.na(all_adj[i,'GarageArea'])){
    all_adj[i,'GarageFinish']=NA
  }else if (!is.na(all_adj[i,'GarageFinish'])&&is.na(all_adj[i,'GarageArea'])){
    all_adj[i,'GarageFinish']=NA
  }else if (is.na(all_adj[i,'GarageFinish'])&&all_adj[i,'GarageArea']>0){
    all_adj[i,'GarageFinish']=NA
  }else if (is.na(all_adj[i,'GarageFinish'])&&all_adj[i,'GarageArea']==0){
    all_adj[i,'GarageFinish']=0
      }else if (!is.na(all_adj[i,'GarageFinish'])&&all_adj[i,'GarageArea']==0){
    all_adj[i,'GarageFinish']=NA
  }else if (all_adj[i,'GarageFinish']=='Fin'&&all_adj[i,'GarageArea']>0){
    all_adj[i,'GarageFinish']=3
  }else if (all_adj[i,'GarageFinish']=='RFn'&&all_adj[i,'GarageArea']>0){
    all_adj[i,'GarageFinish']=2   
  }else if (all_adj[i,'GarageFinish']=='Unf'&&all_adj[i,'GarageArea']>0){
    all_adj[i,'GarageFinish']=1
  }else{NULL
  }
}

all_adj[,'GarageFinish']<- sapply(all_adj[,'GarageFinish'],convert_int)

chr_trans<-sapply(all_adj[,adj],convert_int)

"
+4 LotShape :Reg significant lower than IR1 IR2      ->reglot(LotShape[LotShape==Reg,])
+5 LandContour: Bnk significant lower than others       ->banked (LandContour[LandContour==Bnk,])
+6 LotConfig:CulDSac significant lower than others(CulDSac was not significant with FR3雙峰 3obs)  ->CulDSac(LotConfig[LotConfig==CulDSac],])
+12 HouseStyle: 重新設定為1story<1.5s<2story
+17 MasVnrType :Stone>BrkFace>BrkCmn(exclude none observation) wallbrick=Y*walltype=Stone>BrkFace>BrkCmn
18 Foundation: PConc significant higher than most(except for stone(1obs)&Wood(2obs))  ->PConc(1,0)
+20 CentralAir: Has significant higher than    -> No CentralAir(1,0)
+21 Electrical:  SBrkr significant higher   -> SBrkr(1,0)
+23 GarageTyp: BuiltIn significant higher   -> BuiltIn(1,0)
+24 GarageFinish:Fin>RFn>Unf NA=0(logically pass)
+27 SaleType: New significant higher   -> New (1,0)
+28 SaleCondition: Partial significant higher   -> Partial (1,0)
"






num_trans
chr_trans
all_fin<-cbind(num_trans,chr_trans)
colnames(all_fin)

'====================補資料================'


#手動算na
na_all2<-data.frame()
for(i in 1:length(all_fin)){
  na_all2[i,1]<-i
  na_all2[i,2]<-colnames(all_fin[i])
  na_all2[i,3]<-sum(is.na(all_fin[i])=='TRUE')
  na_all2[i,4]<-class(all_fin[[i]])
}
for(i in 1:nrow(na_all2)){
  na_all2[i,'ratio']<-paste(round((na_all2[i,'V3']/2919)*100,2),'%')
}

sum(na_all2$V3)  #Seed number train


#result1
mice.data<- mice(all_fin,
                  m = 5,           # 產生三個被填補好的資料表
                  maxit = 50,      # max iteration
                  method = 'cart', # 使用CART決策樹，進行遺漏值預測
                  seed = 500)    
apply(true_val,2,pMiss)

all_fin<-complete(mice.data, 3)
train_fin<-all_fin[1:1460,]
train_fin[,'SalePrice']<-train[,'SalePrice']
"================================== 迴歸================================="
reg1<-lm(SalePrice~OverallQual+ExterQual+BsmtQual+BsmtFinSF1+TotalBsmtSF+X1stFlrSF+GrLivArea+FullBath+KitchenQual+TotRmsAbvGrd+FireplaceQu+GarageCars+GarageArea+Bathabove+Totalbath+YearBuilt+YearRemodAdd+GarageYrBlt+reglot+banked+CulDSac+PConc+CentralAir+SBrkr+BuiltInGar+New+Partial+HouseStyle+MasVnrType+GarageFinish,data = train_fin)
summary(reg1)

reg2<-lm(SalePrice~OverallQual+ExterQual+BsmtQual+BsmtFinSF1+GrLivArea+KitchenQual+TotRmsAbvGrd+FireplaceQu+GarageCars+Totalbath+GarageYrBlt+reglot+banked+CulDSac+BuiltInGar+HouseStyle,data = train_fin)
summary(reg2)

reg3<-lm(SalePrice~OverallQual+ExterQual+BsmtQual+BsmtFinSF1+GrLivArea+KitchenQual+FireplaceQu+GarageCars+Totalbath+reglot+banked+CulDSac+BuiltInGar+HouseStyle,data = train_fin)
summary(reg3)






test_fin<-all_fin[1461:2919,]
result1<-data.frame(row.names(test_fin))
result1[,2]<-as.data.frame(predict(reg3,newdata = test_fin))
colnames(result1)<-c('Id','SalePrice')
round(result1$SalePrice, digits = 7)
write.table(result1, file = "submission2.1.CSV", sep = ",")


#result2
all_fin<-cbind(num_trans,chr_trans)
all_fin[,'SalePrice']<-all[,'SalePrice']

na_all5<-data.frame()
for(i in 1:length(all_fin)){
  na_all5[i,1]<-i
  na_all5[i,2]<-colnames(all_fin[i])
  na_all5[i,3]<-sum(is.na(all_fin[i])=='TRUE')
  na_all5[i,4]<-class(all_fin[[i]])
}
for(i in 1:nrow(na_all5)){
  na_all5[i,'ratio']<-paste(round((na_all5[i,'V3']/2919)*100,2),'%')
}
sum(na_all5$V3)  #Seed number test


mice.data<- mice(all_fin,
                 m = 5,           # 產生三個被填補好的資料表
                 maxit = 50,      # max iteration
                 method = 'cart', # 使用CART決策樹，進行遺漏值預測
                 seed = 500)    
apply(true_val,2,pMiss)
'fit <- with(data = mice.data, exp = lm(SalePrice~OverallQual+ExterQual+BsmtQual+BsmtFinSF1+TotalBsmtSF+X1stFlrSF+GrLivArea+FullBath+KitchenQual+TotRmsAbvGrd+FireplaceQu+GarageCars+GarageArea+Bathabove+Totalbath+YearBuilt+YearRemodAdd+GarageYrBlt+reglot+banked+CulDSac+PConc+CentralAir+SBrkr+BuiltInGar+New+Partial+HouseStyle+MasVnrType+GarageFinish))
summary(fit)'
fit2 <- with(data = mice.data, exp = lm(SalePrice~OverallQual+ExterQual+BsmtQual+BsmtFinSF1+TotalBsmtSF+X1stFlrSF+GrLivArea+FullBath+KitchenQual+TotRmsAbvGrd+FireplaceQu+GarageCars+GarageArea+Bathabove+Totalbath+YearBuilt+YearRemodAdd+GarageYrBlt+reglot+banked+CulDSac+PConc+CentralAir+SBrkr+BuiltInGar+New+Partial+HouseStyle+MasVnrType+GarageFinish))
combine <- pool(fit2) #combine results of all 5 models
summary(fit2)