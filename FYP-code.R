###############LDA###########################################################
A1<-read.csv("FYP.breast-cancer.csv",header = T)
colnames(A1)
Y<-A1$diagnosis
table(A1$diagnosis)
#make each column be numeric

A1$diagnosis[which(A1$diagnosis == "B")] <- "majority" # be majority
A1$diagnosis[which(A1$diagnosis == "M")] <- "minority"# be minority
A1$diagnosis[which(A1$diagnosis == "majority")] <- 1 # be majority
A1$diagnosis[which(A1$diagnosis == "minority")] <- 0# be minority


table(A1$diagnosis)#1 is majority

sa0 <- subset(A1, A1$diagnosis == 0)# minority
sa1 <- subset(A1, A1$diagnosis == 1)#majority
########3:7 test:train
sample0_index<-sample(nrow(sa0),round(nrow(sa0)*0.3))
sample1_index<-sample(nrow(sa1),round(nrow(sa1)*0.3))
train0<-sa0[-sample0_index,]
train1<-sa1[-sample1_index,]
train<-rbind(train0,train1)

test0<-sa0[sample0_index,]
test1<-sa1[sample1_index,]
test<-rbind(test0,test1)



#################################################################################

#source("npc.R")
#fit<-npc(X,Y,method="lda")

# 确定排名阈值 Rank Threshold
#sigma == error

#n<-87
#delta<-0.05
#s <- 1-pbinom(0:(n-1),size=n,prob=1-alpha)
#loc <-  min(which(s<=delta))

####NP-LDA
# NP umbrella algorithm 使用LDA
alpha <- 0.05; error <- 0.05
N <- nrow(train)# 数据大小


# 生成数据
#s0 <- x[which(y==0),] # minority
#s1 <- x[which(y==1),] #majority
#length = ncol
n <- round(nrow(train0) / 2)#出现了带小数点情况
#s0 <- cbind(rep(0, n), s0)
#s1 <- cbind(rep(1, n), s1)

#s<-rbind(train0,train1)
#colnames(s) <- c('label', 'value')

M <- 100
I<-matrix(NA,nrow(test),M)#1,2,total,
E<-matrix(NA,3,M)
# 加载LDA所需的库
library(MASS)
#source(file="SMOTe.R")
# NPClassifier
for (i in 1:M) {
  sa0 <- subset(A1, A1$diagnosis == 0)# minority
  sa1 <- subset(A1, A1$diagnosis == 1)#majority
  ########3:7 test:train
  sample0_index<-sample(nrow(sa0),round(nrow(sa0)*0.3))
  sample1_index<-sample(nrow(sa1),round(nrow(sa1)*0.3))
  train0<-sa0[-sample0_index,]
  train1<-sa1[-sample1_index,]
  train<-rbind(train0,train1)
  
  test0<-sa0[sample0_index,]
  test1<-sa1[sample1_index,]
  test<-rbind(test0,test1)
  # 训练模型
  #train and test
  model <- lda(diagnosis ~. , data = train[,-c(13:32)])
  #
  # total data
  fx<-predict(model,test[,-c(2,13:32)])
  pred_class<-as.matrix(fx$class)
  I[,i]<-pred_class
  ind0 <- which(test$diagnosis==0)
  type1[,i] <- mean(I[ind0,i]!=test$diagnosis[ind0])
  ind1 <- which(test$diagnosis==1)
  type2[,i] <- mean(I[ind1,i]!=test$diagnosis[ind1])
  typeall[,i] <- mean(I[,i]!=test$diagnosis)
}
sum(is.na(I))


mean(typeall)
mean(type1)
mean(type2)
sum(type1>0.05)/M
# 创建一个包含100个随机数的示例数据集

data <- as.vector(type1)
# 绘制箱线图
boxplot(data, 
        main="Boxplot of LDA", # 设置图表标题
        ylab="Values",         # 设置y轴标签
        col="lightblue",       # 设置箱线图的颜色
        border="blue",         # 设置箱线图的边框颜色
        horizontal=TRUE)       # 设置箱线图为水平显示（可选）

################################################################################################################################























################################################################################################################





#######NP-LDA
stall.packages("nproc")
library(nproc)

A1<-read.csv("FYP.breast-cancer.csv",header = T)
colnames(A1)
Y<-A1$diagnosis
table(A1$diagnosis)
#make each column be numeric

A1$diagnosis[which(A1$diagnosis == "B")] <- "majority" # be majority
A1$diagnosis[which(A1$diagnosis == "M")] <- "minority"# be minority
A1$diagnosis[which(A1$diagnosis == "majority")] <- 1 # be majority
A1$diagnosis[which(A1$diagnosis == "minority")] <- 0# be minority


Y<-as.numeric(A1$diagnosis)
Y<-as.matrix(Y)
table(A1$diagnosis)#1 is majority

sa0 <- subset(A1, A1$diagnosis == 0)# minority
sa1 <- subset(A1, A1$diagnosis == 1)#majority
########3:7 test:train
sample0_index<-sample(nrow(sa0),round(nrow(sa0)*0.3))
sample1_index<-sample(nrow(sa1),round(nrow(sa1)*0.3))
train0<-sa0[-sample0_index,]
train1<-sa1[-sample1_index,]
train<-rbind(train0,train1)

test0<-sa0[sample0_index,]
test1<-sa1[sample1_index,]
test<-rbind(test0,test1)



#################################################################################

#source("npc.R")
#fit<-npc(X,Y,method="lda")

# 确定排名阈值 Rank Threshold
#sigma == error

#n<-87
#delta<-0.05
#s <- 1-pbinom(0:(n-1),size=n,prob=1-alpha)
#loc <-  min(which(s<=delta))

####NP-LDA
# NP umbrella algorithm 使用LDA
alpha <- 0.05; error <- 0.05
N <- nrow(train)# 数据大小


# 生成数据
#s0 <- x[which(y==0),] # minority
#s1 <- x[which(y==1),] #majority
#length = ncol
n <- round(nrow(train0) / 2)#出现了带小数点情况
#s0 <- cbind(rep(0, n), s0)
#s1 <- cbind(rep(1, n), s1)

#s<-rbind(train0,train1)
#colnames(s) <- c('label', 'value')

M <- 100
I<-matrix(NA,nrow(test),M)#1,2,total,
E<-matrix(NA,3,M)
# 加载LDA所需的库
library(MASS)
#source(file="SMOTe.R")
# NPClassifier
type1<-matrix(NA,1,M)
type2<-matrix(NA,1,M)
typeall<-matrix(NA,1,M)
for (i in 1:M) {
  sa0 <- subset(A1, A1$diagnosis == 0)# minority
  sa1 <- subset(A1, A1$diagnosis == 1)#majority
  ########3:7 test:train
  sample0_index<-sample(nrow(sa0),round(nrow(sa0)*0.3))
  sample1_index<-sample(nrow(sa1),round(nrow(sa1)*0.3))
  train0<-sa0[-sample0_index,]
  train1<-sa1[-sample1_index,]
  train<-rbind(train0,train1)
  
  test0<-sa0[sample0_index,]
  test1<-sa1[sample1_index,]
  test<-rbind(test0,test1)
  Y<-as.numeric(train$diagnosis)
  Y<-as.matrix(Y)
  # 训练模型
  #train and test
  model <- npc(train[,-c(2,13:32)],Y,method = 'lda')

  # total data
  fx<-predict(model,test[,-c(2,13:32)])
  pred_class<-as.matrix(fx$pred.label)
  I[,i]<-pred_class
  ind0 <- which(test$diagnosis==0)
  type1[,i] <- mean(I[ind0,i]!=test$diagnosis[ind0])
  ind1 <- which(test$diagnosis==1)
  type2[,i] <- mean(I[ind1,i]!=test$diagnosis[ind1])
  typeall[,i] <- mean(I[,i]!=test$diagnosis)
  
}
sum(is.na(I))

mean(typeall)
mean(type1)
mean(type2)
sum(type1>0.05)/M



# 创建一个包含100个随机数的示例数据集

data <- as.vector(type1)
# 绘制箱线图
boxplot(data, 
        main="Boxplot of NP-LDA", # 设置图表标题
        ylab="Values",         # 设置y轴标签
        col="lightblue",       # 设置箱线图的颜色
        border="blue",         # 设置箱线图的边框颜色
        horizontal=TRUE)       # 设置箱线图为水平显示（可选）

fit1=nproc(train[,-c(2,13:32)],Y,method = 'lda')
plot(fit1)

?nproc












#######NP-sLDA
stall.packages("nproc")
library(nproc)

A1<-read.csv("FYP.breast-cancer.csv",header = T)
colnames(A1)
Y<-A1$diagnosis
table(A1$diagnosis)
#make each column be numeric

A1$diagnosis[which(A1$diagnosis == "B")] <- "majority" # be majority
A1$diagnosis[which(A1$diagnosis == "M")] <- "minority"# be minority
A1$diagnosis[which(A1$diagnosis == "majority")] <- 1 # be majority
A1$diagnosis[which(A1$diagnosis == "minority")] <- 0# be minority


Y<-as.numeric(A1$diagnosis)
Y<-as.matrix(Y)
table(A1$diagnosis)#1 is majority

sa0 <- subset(A1, A1$diagnosis == 0)# minority
sa1 <- subset(A1, A1$diagnosis == 1)#majority
########3:7 test:train
sample0_index<-sample(nrow(sa0),round(nrow(sa0)*0.3))
sample1_index<-sample(nrow(sa1),round(nrow(sa1)*0.3))
train0<-sa0[-sample0_index,]
train1<-sa1[-sample1_index,]
train<-rbind(train0,train1)

test0<-sa0[sample0_index,]
test1<-sa1[sample1_index,]
test<-rbind(test0,test1)



#################################################################################

#source("npc.R")
#fit<-npc(X,Y,method="lda")

# 确定排名阈值 Rank Threshold
#sigma == error

#n<-87
#delta<-0.05
#s <- 1-pbinom(0:(n-1),size=n,prob=1-alpha)
#loc <-  min(which(s<=delta))

####NP-LDA
# NP umbrella algorithm 使用LDA
alpha <- 0.05; error <- 0.05
N <- nrow(train)# 数据大小


# 生成数据
#s0 <- x[which(y==0),] # minority
#s1 <- x[which(y==1),] #majority
#length = ncol
n <- round(nrow(train0) / 2)#出现了带小数点情况
#s0 <- cbind(rep(0, n), s0)
#s1 <- cbind(rep(1, n), s1)

#s<-rbind(train0,train1)
#colnames(s) <- c('label', 'value')

M <- 100
I<-matrix(NA,nrow(test),M)#1,2,total,
E<-matrix(NA,3,M)
# 加载LDA所需的库
library(MASS)
#source(file="SMOTe.R")
# NPClassifier
for (i in 1:M) {
  sa0 <- subset(A1, A1$diagnosis == 0)# minority
  sa1 <- subset(A1, A1$diagnosis == 1)#majority
  ########3:7 test:train
  sample0_index<-sample(nrow(sa0),round(nrow(sa0)*0.3))
  sample1_index<-sample(nrow(sa1),round(nrow(sa1)*0.3))
  train0<-sa0[-sample0_index,]
  train1<-sa1[-sample1_index,]
  train<-rbind(train0,train1)
  
  test0<-sa0[sample0_index,]
  test1<-sa1[sample1_index,]
  test<-rbind(test0,test1)
  Y<-as.numeric(train$diagnosis)
  Y<-as.matrix(Y)
  # 训练模型
  #train and test
  model <- npc(train[,-c(2,13:32)],Y,method = 'slda')
  
  # total data
  fx<-predict(model,test[,-c(2,13:32)])
  pred_class<-as.matrix(fx$pred.label)
  I[,i]<-pred_class
  ind0 <- which(test$diagnosis==0)
  type1[,i] <- mean(I[ind0,i]!=test$diagnosis[ind0])
  ind1 <- which(test$diagnosis==1)
  type2[,i] <- mean(I[ind1,i]!=test$diagnosis[ind1])
  typeall[,i] <- mean(I[,i]!=test$diagnosis)
}
sum(is.na(I))


mean(typeall)
mean(type1)
mean(type2)
sum(type1>0.05)/M



# 创建一个包含100个随机数的示例数据集

data <- as.vector(type1)
# 绘制箱线图
boxplot(data, 
        main="Boxplot of NP-sLDA", # 设置图表标题
        ylab="Values",         # 设置y轴标签
        col="lightblue",       # 设置箱线图的颜色
        border="blue",         # 设置箱线图的边框颜色
        horizontal=TRUE)       # 设置箱线图为水平显示（可选）







A1<-read.csv("FYP.breast-cancer.csv",header = T)
colnames(A1)
Y<-A1$diagnosis
table(A1$diagnosis)
#make each column be numeric

A1$diagnosis[which(A1$diagnosis == "B")] <- "majority" # be majority
A1$diagnosis[which(A1$diagnosis == "M")] <- "minority"# be minority
A1$diagnosis[which(A1$diagnosis == "majority")] <- 1 # be majority
A1$diagnosis[which(A1$diagnosis == "minority")] <- 0# be minority


table(A1$diagnosis)#1 is majority

sa0 <- subset(A1, A1$diagnosis == 0)# minority
sa1 <- subset(A1, A1$diagnosis == 1)#majority
########3:7 test:train
sample0_index<-sample(nrow(sa0),round(nrow(sa0)*0.3))
sample1_index<-sample(nrow(sa1),round(nrow(sa1)*0.3))
train0<-sa0[-sample0_index,]
train1<-sa1[-sample1_index,]
train<-rbind(train0,train1)

test0<-sa0[sample0_index,]
test1<-sa1[sample1_index,]
test<-rbind(test0,test1)


################################################################################
####################################manual#####################################
####table(y,pred$pred.label)

#n<-87
#delta<-0.05
#s <- 1-pbinom(0:(n-1),size=n,prob=1-alpha)
#loc <-  min(which(s<=delta))

# NP umbrella algorithm 使用LDA
alpha <- 0.05; error <- 0.05
N <- nrow(train)# 数据大小


# 生成数据
#s0 <- x[which(y==0),] # minority
#s1 <- x[which(y==1),] #majority
#length = ncol
n <- round(nrow(train0) / 2)#出现了带小数点情况
#s0 <- cbind(rep(0, n), s0)
#s1 <- cbind(rep(1, n), s1)

#s<-rbind(train0,train1)
#colnames(s) <- c('label', 'value')

M <- 100
I<-matrix(NA,nrow(test),M)#1,2,total,
E<-matrix(NA,3,M)
# 加载LDA所需的库
library(MASS)



#a2<-(W-W_mu)^2
#source(file="SMOTe.R")
# NPClassifier
for (u in 1:M) {
  sam <- sample(nrow(train0), nrow(train0) / 2)#n minor, half of minor
  s01 <- train0[sam, ]
  s02 <- train0[-sam, ]
  s01<-as.matrix(train0[,-c(2,13:32)])
  mu0<-apply(s01,2,mean)#one row ,each colunm mean
  n0 <- nrow(s01) # number of observations
  # Initialize the covariance matrix
  cov_matrix0 <- matrix(0, ncol(s01), ncol(s01))
  # Compute the sum of the outer products
  for(i in 1:n0) {
    centered_vector <- s01[i, ] - mu0
    cov_matrix0 <- cov_matrix0 + (centered_vector %*% t(centered_vector))
  }
  
  X1<-as.matrix(train1[,-c(2,13:32)])
  mu1<-apply(X1,2,mean)#one row
  n1 <- nrow(X1) # number of observations
  # Initialize the covariance matrix
  cov_matrix1 <- matrix(0, ncol(X1), ncol(X1))
  
  # Compute the sum of the outer products
  for(i in 1:n1) {
    centered_vector1 <- X1[i, ] - mu1
    cov_matrix1 <- cov_matrix1 + (centered_vector1 %*% t(centered_vector1))
  }
  # Compute the pooled covariance matrix
  #sigma
  pooled_cov_matrix <- (cov_matrix0 + cov_matrix1) / (n0 + n1 - 2)
  
  mud<-mu1-mu0
  AM<-pooled_cov_matrix%*%mud#A
  
  alpha<-0.05
  
  n02<-nrow(s02)
  W<-matrix(NA,1,n02)
  X02<-as.matrix(s02[,-c(2,13:32)])
  for(i in 1:n02){
    X02i<-as.matrix(X02[i,])
    W[,i] <- t(AM) %*% X02i
  }
  sum(is.na(W))
  
  W_mu<-mean(W)
  delta<-0.05
  df<-nrow(s02)-1
  
  F_M <- qt(delta, df)
  cita<-qnorm(1-alpha)
  
  e<-10^(-3)
  
  S_M<-sqrt(sum((W-W_mu)^2)/(n02-1))#S
  
  sigma<-pooled_cov_matrix
  d<-ncol(sa0)
  eigen_values <- eigen(sigma)$values
  max_eigenvalue <- max(eigen_values)
  
  K11<-(1-sqrt(d/(n0+n1-2)))^2
  K12<-((n0+n1-2)^e)/(sqrt(n0+n1-2)*d^(1/6))
  K13<-t(AM) %*% AM
  K1<-sqrt((1/(K11-K12))*max_eigenvalue*K13)
  
  #Lamda
  
  
  K3<-F_M*(S_M/sqrt(n02))
  
  
  K<-K1*cita+W_mu-K3
  test_1<-as.matrix(test[,-2])
  
  pred_class<-matrix(NA,nrow(test_1),1)
  for(i in 1:nrow(test_1)){
    score<-t(AM)%*%as.matrix(test_1[i,-c(12:32)])
    pred_class[i,]<-ifelse(score>K,1,0)
  }
  
  I[,u]<-pred_class[,1]
  # test data
  ind0 <- which(test$diagnosis==0)
  type1[,i] <- mean(I[ind0,i]!=test$diagnosis[ind0])
  ind1 <- which(test$diagnosis==1)
  type2[,i] <- mean(I[ind1,i]!=test$diagnosis[ind1])
  typeall[,i] <- mean(I[,i]!=test$diagnosis)
}
sum(is.na(I))


mean(typeall)
mean(type1)
mean(type2)
sum(type1>0.05)/M

data <- as.vector(type1)
# 绘制箱线图
boxplot(data, 
        main="Boxplot of pNP-LDA", # 设置图表标题
        ylab="Values",         # 设置y轴标签
        col="lightblue",       # 设置箱线图的颜色
        border="blue",         # 设置箱线图的边框颜色
        horizontal=TRUE)       # 设置箱线图为水平显示（可选）




# 创建一个空的图形窗口
par(mfrow=c(2, 5)) # 5行6列，总共30个图

# 使用循环为每组数据绘制直方图
for (i in 1:30) {
  hist(A1$radius_mean, xlab="radius_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$texture_mean, xlab="texture_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$perimeter_mean, xlab="perimeter_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$area_mean, xlab="area_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$smoothness_mean, xlab="smoothness_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$compactness_mean, xlab="compactness_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$concavity_mean, xlab="concavity_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$concave.points_mean,xlab="concave.points_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$symmetry_mean, xlab="symmetry_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
  hist(A1$fractal_dimension_mean, xlab="fractal_dimension_mean", ylab="Frequency",col="lightblue",border=NULL,main="")
}
















save(I,file="np100lda_I.RData")
save(type1,file = "np100lda_type1.RData")
save(type2,file = "np100lda_type2.RData")
save(typeall,file = "np100lda_typeall.RData")
#save()

class_np<-round(apply(I,1,mean))
cbind(test,class_np)
test_stroke <- factor(test$stroke, levels = c(0, 1))
pred_class_conf <- factor(pred_class, levels = c(0, 1))

conf_matrix <- table(Actual = test_stroke, Predicted = pred_class_conf)


print(conf_matrix)
T0<-conf_matrix[1,1] #actual=0 predict=0
F0<-conf_matrix[2,1] #actual=1 predict=0
T1<-conf_matrix[2,2] #actual=1 predict=1
F1<-conf_matrix[1,2] #actual=0 predict=1

FN<-F0/(T1+F0)
FP<-F1/(T0+F1)

Fa<-(F0+F1)/(F0+F1+T0+T1)









