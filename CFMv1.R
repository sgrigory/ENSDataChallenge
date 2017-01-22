path="~/Documents/ens challenge/cfm"
data0=read.csv(paste0(path,"/training_input.csv"))
data0.outcome=read.csv(paste0(path,"/challenge_output.csv"))

testing.input=read.csv(paste0(path,"/testing_input.csv"))

library(moments)

library(reshape2)
library(caret)
library(zoo)


data0.part1=data0[data0$ID %in% 1:(dim(data0.outcome)[1]/2),]

data0.part1=data0

data0.part2=data0[data0$ID %in% (1+dim(data0.outcome)[1]/2):(dim(data0.outcome)[1]),]

part=createDataPartition(1:(dim(data0.outcome)[1]/2),p=.1,list=F)

part=1:((dim(data0.outcome)[1]/2)/10)

part=1:((dim(data0.outcome)[1]))


data.part=data0.part1[data0.part1$ID %in% part,]
testing.input.part=testing.input

outcome.processed=sapply(data0.outcome$ID.TARGET,function(x) as.numeric(strsplit(as.character(x),split=";")[[1]][2]))

outcome.processed.part1=outcome.processed[1:(length(outcome.processed)/2)]
outcome.processed.part2=outcome.processed[(1+length(outcome.processed)/2):(length(outcome.processed))]

outcome.part=outcome.processed.part1[part]

outcome.part=outcome.processed

cleanup=function(df)
{
  df=data.frame(lapply(df,as.numeric))
  
  print(head(df$ask_1))
  print(head(df$bid_1))
  df$spread=(df$ask_1-df$bid_1==1)
  ID.spread=aggregate(spread~ID,data=df,FUN=all)
  print(head(ID.spread))
  
  nms=setdiff(names(df),c("ID","offset","bid_1","bid_2","ask_2","spread"))  
  
  total_change = (df[df$offset=="-1000",nms]-df[df$offset=="0",nms])==0
  
  print(dim(total_change))
  df[df$offset=="-1000",nms] = df[df$offset=="-1000",nms]-df[df$offset=="-500",nms]      
  df[df$offset=="-500",nms] = df[df$offset=="-500",nms]-df[df$offset=="-200",nms]
  df[df$offset=="-200",nms] = df[df$offset=="-200",nms]-df[df$offset=="-100",nms]
  df[df$offset=="-100",nms] = df[df$offset=="-100",nms]-df[df$offset=="-50",nms]
  df[df$offset=="-50",nms] = df[df$offset=="-50",nms]-df[df$offset=="-20",nms]
  df[df$offset=="-20",nms] = df[df$offset=="-20",nms]-df[df$offset=="-10",nms]
  df[df$offset=="-10",nms] = df[df$offset=="-10",nms]-df[df$offset=="0",nms]
  
 
  
  #log transform
  
  
  # long to wide
  
  res=unique(df$ID)
  nms=setdiff(names(df),c("ID","offset","bid_1","bid_2","ask_2","spread"))
  
  for (nm in nms){
    col_nms=sapply(as.character(unique(df$offset)),function(x) paste(nm,x,sep="_"))  
    data.filtered=dcast(df,ID~offset,value.var =nm)
    colnames(data.filtered)=c("ID",col_nms)
    id=data.filtered$ID
    data.filtered$ID=NULL
    res=cbind(res,data.filtered)
  }
  colnames(res)[1]="ID"
  #res["ask_1_-500"]=NULL
  #res["ask_1_-200"]=NULL
  #res["ask_1_-100"]=NULL
  #res["ask_1_-50"]=NULL
  #res["ask_1_-20"]=NULL
  #res["ask_1_-10"]=NULL

  log_vars=c("bid_size_1_0","ask_size_1_0","bid_size_2_0","ask_size_2_0",
             "bid_sqentry_1_0", "ask_sqentry_1_0", "bid_sqentry_2_0", "ask_sqentry_2_0")
  res[,log_vars]=log(res[,log_vars]+100)
  
  
  log_vars_50=c("bid_entry_1_0","ask_entry_1_0")
  res[,log_vars]=log(res[,log_vars]+50)
  
  log_vars_50=c("bid_entropy_1_0","ask_entropy_1_0","bid_entropy_2_0","ask_entropy_2_0")
  res[,log_vars]=exp(2*res[,log_vars])
  

  res=na.locf(res)
  
  res=data.frame(lapply(res,as.numeric))
  
  res$spread=as.numeric(ID.spread$spread)
 
  #add roll diff
  
  list_for_mean=c("ask_1_0","bid_size_1_0","ask_size_1_0","bid_size_2_0",       
                  "ask_size_2_0","bid_entry_1_0","ask_entry_1_0","bid_entry_2_0",
                  "ask_entry_2_0","bid_entropy_1_0","ask_entropy_1_0","bid_entropy_2_0",
                  "ask_entropy_2_0","bid_sqentry_1_0","ask_sqentry_1_0","bid_sqentry_2_0",  
                  "ask_sqentry_2_0", "nb_trade_0") 
  
  res_roll=rollmean(res[,list_for_mean],k=1000,fill=rep("extend",3))
  
  res_roll_diff=rbind(rep(0,dim(res_roll)[2]),rollmean(diff(res_roll),k=1000,fill=rep("extend",3)))
  
  colnames(res_roll_diff)=sapply(colnames(res_roll),function(x) paste(x,".roll.diff"))
  
  res=cbind(res,res_roll_diff)
  
  res$`bid_entry_1_0 .roll.diff`=res$`bid_entry_1_0 .roll.diff`-res$`ask_entry_1_0 .roll.diff`
  res$`bid_entry_2_0 .roll.diff`=res$`bid_entry_2_0 .roll.diff`-res$`ask_entry_2_0 .roll.diff`
  res$`bid_entropy_1_0 .roll.diff`=res$`bid_entropy_1_0 .roll.diff`-res$`ask_entropy_1_0 .roll.diff`
  res$`bid_entropy_2_0 .roll.diff`=res$`bid_entropy_2_0 .roll.diff`-res$`ask_entropy_2_0 .roll.diff`
  
   
  print(dim(res))
  print(dim(total_change))
  
  res=cbind(res,total_change)
  print(dim(res))
  
  return(res)
}

#cleanup

reshaped=cleanup(data.part)

reshaped$outcome=as.numeric(outcome.part)

#near.zeros=nearZeroVar(reshaped)

reshaped2=reshaped #[,!(1:dim(reshaped)[2] %in% near.zeros)]

reshaped2$outcome=as.factor(reshaped2$outcome)

#Feature analysis

library(Boruta)

for.boruta=reshaped2[1:10000,c(-1,-dim(reshaped2)[2])]

boruta.res=Boruta(for.boruta, reshaped2[1:10000,"outcome"], maxRuns = 101, doTrace = 0)

names.boruta=names(boruta.res$finalDecision)[(boruta.res$finalDecision=="Confirmed")|(boruta.res$finalDecision=="Tentative")]

reshaped2=reshaped2[,c("ID",names.boruta,"outcome")]


#correlations
library(corrplot)

correlations=cor(data.frame(lapply(reshaped2[1:10000,1:40],as.numeric)))

corrplot(correlations,method="circle",type="lower")

cor(reshaped2[1:10000,]$ask_entry_1_.20*reshaped2[1:10000,]$ask_1_0,reshaped2[1:10000,]$ask_size_1_.20)

cor(reshaped2[1:10000,]$ask_entry_1_.20,reshaped2[1:10000,]$ask_size_1_.20)

plot(na.omit(reshaped2[1:10000,]$ask_size_1_.20/reshaped2[1:10000,]$ask_entry_1_.20))

plot(reshaped2[1:10000,]$ask_entry_1_.20)

qplot(reshaped2[1:10000,]$ask_size_1_0/reshaped2[1:10000,]$ask_entry_1_0,reshaped2[1:10000,]$ask_size_1_0)

vars_to_remove=findCorrelation(data.frame(lapply(reshaped2[1:10000,],as.numeric)),cutoff=.9)

reshaped3=reshaped2[,-setdiff(vars_to_remove,which(colnames(reshaped2)=="outcome"))]

reshaped3=reshaped2

partition=createDataPartition(1:dim(reshaped3)[1],p=0.7,list=F)
train=reshaped3[partition,]
test=reshaped3[-partition,]


# fit with glm

tC=trainControl(method="repeatedcv",number=10,repeats=5)

fit.glm=train(outcome~.,method="glmnet",data=train[-1],trControl=tC)


pred.glm.train=predict(fit.glm,newdata=train)
print(mean(pred.glm.train==train$outcome))

pred.glm.test=predict(fit.glm,newdata=test)
print(mean(pred.glm.test==test$outcome))


# svm
fit.svm=train(outcome~.,method="svmRadial",data=train[-1],trControl=tC)

pred.svm.train=predict(fit.svm,newdata=train)
print(mean(pred.svm.train==train$outcome))

pred.svm.test=predict(fit.svm,newdata=test)
print(mean(pred.svm.test==test$outcome))

mean(pred.glm.test=="1")
mean(test$outcome=="1")

mean(pred.glm.train=="1")
mean(train$outcome=="1")


# xgb

# test on part2

reshaped.part2=cleanup(data0.part2)

pred.glm.part2=predict(fit.glm,newdata=reshaped.part2)
correct.part2=pred.glm.part2==outcome.processed.part2
mean(correct.part2)
plot(rollmean(as.numeric(correct.part2),k=10000))

plot(rollmean(as.numeric(reshaped.part2$nb_trade),k=10000))

plot(rollmean(as.numeric(outcome.processed.part2),k=10000))

pred.df=data.frame(
  y1=rollmean(as.numeric(outcome.processed.part2),k=10000),
  y2=rollmean(as.numeric(pred.glm.part2),k=10000)
  )

mean(outcome.processed.part2) 
mean(as.numeric(pred.glm.part2)-1)

plt=ggplot()+geom_line(data=pred.df,aes(y=y1,x=1:dim(pred.df)[1],fill="red",col=1))

plt

plt+geom_line(data=pred.df,aes(y=y2,x=1:dim(pred.df)[1],fill="blue",col=2))

pred.svm.part2=predict(fit.svm,newdata=reshaped.part2)
correct.svm.part2=pred.svm.part2==outcome.processed.part2
plot(rollmean(as.numeric(correct.svm.part2),k=10000))

plot(reshaped2$`bid_entropy_2_0 .roll.diff`,type='l')

pred.glm.part1=predict(fit.glm,newdata=reshaped2)
correct.part1=pred.glm.part1==outcome.processed.part1[part]
plot(rollmean(as.numeric(correct.part1),k=10000))

diff(rollmean(as.numeric(reshaped2$nb_trade),k=300))

plot(rollmean(as.numeric(reshaped2$ask_entropy_1_0-reshaped2$bid_entropy_1_0),k=1000),type='l')
plot(rollmean(as.numeric(reshaped2$bid_entropy_1_0),k=1000),type='l')

length(pred.glm.part2)

length(outcome.processed.part2)

ggplot(data=reshaped2,aes(x=ask_entropy_2_0,y=ask_entropy_2_.200,col=outcome))+geom_point()

tb=table(reshaped2$ask_entropy_1_.50==0,reshaped$outcome)
tb/rowSums(tb)

tb=table(reshaped2$ask_entropy_1_.100==0,reshaped$outcome)
tb/rowSums(tb)

tb=table(reshaped2$ask_entropy_1_.200==0,reshaped$outcome)
tb/rowSums(tb)

tb=table(reshaped2$ask_entropy_1_.500==0,reshaped$outcome)
tb/rowSums(tb)

tb=table(reshaped2$ask_entropy_1_.1000==0,reshaped$outcome)
tb/rowSums(tb)

table(reshaped2$ask_entropy_1_.500==0,reshaped2$ask_entropy_1_.1000==0)


mean(reshaped.part2$ask_entropy_1_.20==0)
mean(reshaped.part2$ask_entropy_1_.50==0)
mean(reshaped.part2$ask_entropy_1_.100==0)
mean(reshaped.part2$ask_entropy_1_.200==0)
mean(reshaped.part2$ask_entropy_1_.500==0)
mean(reshaped.part2$ask_entropy_1_.1000==0)

mean(reshaped.part2$ask_entropy_2_.20==0)
mean(reshaped.part2$ask_entropy_2_.50==0)
mean(reshaped.part2$ask_entropy_2_.100==0)
mean(reshaped.part2$ask_entropy_2_.200==0)
mean(reshaped.part2$ask_entropy_2_.500==0)
mean(reshaped.part2$ask_entropy_2_.1000==0)



tb=table(reshaped2$ask_entropy_1_.500,reshaped$outcome)[,1]
entropy.df=data.frame(outcome=as.vector(tb),ind=as.numeric(names(tb)))
mean.entropy.df=data.frame(rollmean(entropy.df[order(entropy.df$ind),],k=100))
ggplot(data=mean.entropy.df,aes(x=ind,y=outcome))+geom_line()

tb=table(reshaped2$ask_size_1_.500,reshaped$outcome)[,1]
entropy.df=data.frame(outcome=as.vector(tb),ind=as.numeric(names(tb)))
mean.entropy.df=data.frame(rollmean(entropy.df[order(entropy.df$ind),],k=100))
ggplot(data=mean.entropy.df,aes(x=ind,y=outcome))+geom_line()

# fit with gbm

tC=trainControl(method="repeatedcv",number=5,repeats=2)

fit.gbm=train(outcome~.,method="gbm",train[-1],trControl=tC)

pred.gbm.train=predict(fit.gbm,newdata=train)
mean(pred.gbm.train==train$outcome)

pred.gbm.test=predict(fit.gbm,newdata=test)
mean(pred.gbm.test==test$outcome)

# test

pred.test.glm=predict(fit.glm,newdata=test,type="prob")[,2]

pred.test.gbm=predict(fit.gbm,newdata=test,type="prob")[,2]

pred.test=as.numeric((pred.test.glm+pred.test.gbm)/2>0.5)

pred.test=as.numeric((pred.test.glm+pred.test.gbm)/2>0.5)

mean(pred.test==test$outcome)

#predict

input.testing.clean=cleanup(testing.input.part)

pred.final.glm=predict(fit.glm,newdata=input.testing.clean,type="prob")[,2]

pred.final.gbm=predict(fit.gbm,newdata=input.testing.clean,type="prob")[,2]

pred.final=as.numeric((pred.final.glm+pred.final.gbm)/2>0.5)

pred.final=as.numeric(pred.final.glm>0.5)

pred.final=predict(fit.glm,newdata=input.testing.clean)

plot(rollmean(pred.final==1,k=10000),type='l')

rollmean(pred.final==1,k=1000)


mean(pred.glm.test==1)

mean(pred.glm.train==1)


# write


submission=data.frame(ID=input.testing.clean$ID,TARGET=pred.final)

mean(as.numeric(submission$TARGET)-1)

write.table(submission,file=paste0(path,"/submission.csv"),sep=";",append = FALSE,row.names = F)

library(ggplot2)





#offset
plot(table(data.part$offset,outcome.part)/rowSums(table(data.part$offset,outcome.part)))

#nb_trades
tb=table(data.part$nb_trade,outcome.part)/rowSums(table(data.part$nb_trade,outcome.part))

plot(tb)


# 

tb1=table(data.part$ask_size_1,outcome.part)
tb=tb1/rowSums(tb1)

df=data.frame(tb)

df$Var1=cut(as.numeric(df$Var1),50)

df=aggregate(Freq~.,data=df,FUN=mean)

ggplot(df,aes(x=Var1,y=Freq))+geom_point()
library(plotly)
#bid_size and ask_size are anticorrelated
ggplot(data=data.part,aes(x=bid_size_1,y=bid_size_2,col=outcome.part))+geom_point()
ggplot(data=data.part,aes(x=ask_size_1,y=ask_size_2,col=outcome.part))+geom_point()

ggplot(data=data.part,aes(x=bid_size_2,y=ask_size_2,col=outcome.part))+geom_point()
ggplot(data=data.part,aes(x=bid_entropy_1,y=ask_entropy_1,col=outcome))+geom_point()
ggplot(data=data.part,aes(x=bid_entropy_2,y=ask_entropy_2,col=outcome))+geom_point()
ggplot(data=data.part,aes(x=ask_size_1,y=ask_size_1+bid_size_1,col=outcome.part))+geom_point()
qplot(ask_size_1+bid_size_1,data=data.part)

sd(-cor(data.part$ask_size_1,data.part$bid_size_1)*data.part$bid_size_1+data.part$ask_size_1)
sd(data.part$bid_size_1)

sd(data.part$bid_size_1-predict(lm(bid_size_1~ask_size_1,data=data.part),newdata=data.part))

sd(data.part$ask_size_1)


cor(data.part$ask_size_1,data.part$bid_size_1)

cor.test(data.part$ask_size_1,data.part$bid_size_1)

ggplot(data=data.part,aes(x=bid_size_1,y=ask_size_1,col=outcome.part,alpha=0.001))+geom_point()

ggplot(data=data.part,aes(x=bid_size_1,y=ask_size_1,col=outcome.part,alpha=0.001))+geom_point()


agg=aggregate(outcome~cut(as.numeric(data.part$ask_1),50),data=data.part,FUN=mean)
colnames(agg)=c("interval","frequency")
ggplot(agg,aes(x=interval,y=frequency))+geom_bar(stat="identity")

agg=aggregate(outcome~cut(as.numeric(data.part$ask_1-data.part$bid_1),breaks=100),data=data.part,FUN=mean)
colnames(agg)=c("interval","frequency")
ggplot(agg,aes(x=interval,y=frequency))+geom_bar(stat="identity")


table(data.part$bid_1-data.part$ask_1)/dim(data.part)[1]
table(data.part$bid_1-data.part$ask_1,data.part$outcome)/rowSums(table(data.part$bid_1-data.part$ask_1,data.part$outcome))

# vs time
(ggplot(data=data.part)+geom_line(aes(x=ID,y=bid_1))+geom_line(aes(x=ID,y=bid_2,colour="green"))
+geom_line(aes(x=ID,y=ask_1,colour="blue"))+geom_line(aes(x=ID,y=ask_2,colour="red")))

#
plot(aggregate(outcome~ID,data=data.part,FUN=sum))

qplot((aggregate(outcome~ID,data=data.part,FUN=sum))$outcome)

ggplot(data=data.part)+geom_line(aes(x=ID,y=bid_1,color=outcome))


# bid2-bid1-ask1-ask2
table(colSums(!(table(data.part$bid_size_1,data.part$ID)==0)))
table(colSums(!(table(data.part$bid_size_2,data.part$ID)==0)))

table(data.part$ask_1-data.part$ask_2)
table(data.part$bid_1-data.part$bid_2)
table(data.part$bid_1-data.part$ask_1)

diff1=rep(0,dim(data.part)[1])
diff2=rep(0,dim(data.part)[1])
for (i in 9:16)
{
  diff1[i]=data.part$ask_1[i]-data.part$ask_1[i-8]
}
for (i in 17:dim(data.part)[1])
{
  diff1[i]=data.part$ask_1[i]-data.part$ask_1[i-8]
  diff2[i]=data.part$ask_1[i]-data.part$ask_1[i-16]
}

data.part$diff1=diff1
data.part$diff2=diff2

table(diff1)/sum(table(diff1))
table(diff2)/sum(table(diff2))


(table(diff1,data.part$outcome)/rowSums(table(diff1,data.part$outcome)))[4:8,]
(table(diff2,data.part$outcome)/rowSums(table(diff2,data.part$outcome)))[4:8,]

(table(diff1-diff2,data.part$outcome)/rowSums(table(diff1-diff2,data.part$outcome)))[4:8,]

qplot(diff1,diff2)

table(diff1-diff2)/sum(table(diff1-diff2))

qplot(data.part$bid_size_1-data.part$bid_size_2,bins=100)

df=aggregate(bid_size_1*bid_1~ID,data=data.part,FUN=mean)
colnames(df)=c("ID","Avg")
ggplot(aes(x=ID,y=Avg),data=df)+geom_line()

ggplot(aes(x=ID,y=bid_size_1),data=data.part[1:50000,])+geom_line()

plot(aggregate(bid_size_1/bid_1~ID,data=data.part,FUN=mean))

library(zoo)

separate_scales=function(df,var_name_list)
{
  for (var_name in var_name_list)
  {
  print(var_name)
  print(paste(var_name,"roll_diff",sep="_"))
  df[paste(var_name,"roll",sep="_")]=rollmean(df[var_name],k=400,fill=c("extend","extend","extend"))
  arr1<<-diff(df[,paste(var_name,"roll",sep="_")])
  df[paste(var_name,"roll_diff",sep="_")]=c(0.0,arr1)
  df[paste(var_name,"fluct",sep="_")]=df[var_name]-df[paste(var_name,"roll",sep="_")]
  }
  print(names(df))
  return(df)
}



df1=separate_scales(data.part,c("outcome","bid_1"))

ggplot(aes(x=df1$ID,y=df1$outcome_roll_diff,col=(diff1_roll)),data=df1)+geom_line()

ggplot(aes(x=df1$diff1_roll,y=df1$outcome_roll),data=df1)+geom_point()

cor(df1$outcome_roll_diff,df1$diff1_roll)

data.part$bid_size_1_roll=rollmean(data.part$bid_size_1,k=400,fill=c("extend","extend","extend"))
data.part$bid_size_1_fluct=data.part$bid_size_1-data.part$bid_size_1_roll
data.part$bid_size_2_roll=rollmean(data.part$bid_size_2,k=200,fill=c("extend","extend","extend"))
data.part$bid_size_2_fluct=data.part$bid_size_2-data.part$bid_size_1_roll

data.part$bid_size_1_roll=rollmean(data.part$bid_size_1,k=400,fill=c("extend","extend","extend"))
data.part$bid_size_1_fluct=data.part$bid_size_1-data.part$bid_size_1_roll
data.part$bid_size_2_roll=rollmean(data.part$bid_size_2,k=200,fill=c("extend","extend","extend"))
data.part$bid_size_2_fluct=data.part$bid_size_2-data.part$bid_size_1_roll

qplot(ID,bid_size_1_roll,data=data.part)

qplot(ID,bid_size_1_fluct,data=data.part)

#local approach

separate_scales=function(df,var_name_list)
{
  for (var_name in var_name_list)
  {
    print(var_name)
    print(paste(var_name,"roll_diff",sep="_"))
    df[paste(var_name,"roll",sep="_")]=rollmean(df[var_name],k=400,fill=c("extend","extend","extend"))
    arr1<<-diff(df[,paste(var_name,"roll",sep="_")])
    df[paste(var_name,"roll_diff",sep="_")]=c(0.0,arr1)
    df[paste(var_name,"fluct",sep="_")]=df[var_name]-df[paste(var_name,"roll",sep="_")]
  }
  print(names(df))
  return(df)
}



data.part$nb_trade_doff=c(0,diff(data.part$nb_trade))

data.part$speed=c(0,diff(data.part$nb_trade)/diff(data.part$offset))

data.part$speed[data.part$speed<0]=0

avg_nb_trades=aggregate(speed~offset,data=data.part,FUN=mean)

avg_nb_trades

diff(avg_nb_trades[,1])

diff(avg_nb_trades[,2])/diff(avg_nb_trades[,1])

diff(avg_nb_trades[,2])/diff(avg_nb_trades[,1])

#try predict
library(caret)

vars=c("offset","bid_size_1","ask_size_1","bid_size_2","ask_size_2","bid_entry_1",
       "ask_entry_1","bid_entry_2","ask_entry_2",  
       "bid_entropy_1","ask_entropy_1","bid_entropy_2","ask_entropy_2","bid_sqentry_1","ask_sqentry_1",
       "bid_sqentry_2","ask_sqentry_2","nb_trade","nb_trade_diff","outcome")

data.filtered=data.part[,vars]
data.filtered$outcome=as.factor(data.filtered$outcome)

fit.glm=train(outcome~.,method="glm",data.filtered)

pred.glm=predict(fit.glm,newdata=data.filtered)

mean(pred.glm==data.filtered$outcome)
