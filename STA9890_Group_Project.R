#Imbalanced 
#NO_INFEC cate,NO_MMORB,MTRAN, PRIORTERM,PRIORDEAD,RF_CESARN, DOB_YY,IP_GON cate, RESTATUS, MM_AICU, MHISPX,MRAVE6,PRECARE,RDMETH_REC,FHISPX

#Missing vlaues  
#MAGE_IMPFLG, MAR_IMP, IMP_SEX, MRACEIMP, DLMP_YY,ILP_R 

#Redundant  
# Dwgt_R num #redundant, DBWT 
#MRACE15 cate - MRAVE6
#MRACE31 cate - MRAVE6
#ILLB_R num - ILP_R repeating & imbalance
#ILOP_R num - ILP_R repeating & imbalance
#FRACE15 -FRACE5 repeating
#FRACE31 -FRACE5 repeating
#PAY cate - PAY_REC


rm(list = ls())    #delete objects
cat("\014")        #clear console
library(tidyverse); library(modelr);
library(glmnet)
library(glmnetUtils)
library(dplyr)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)


## read data
d <- read_csv("./US_births(2018).csv")

d2 = d
#numeric code missing values to na
d2['BMI'] <- na_if(d2['BMI'], 99.9) #86200 missing
d2['CIG_0'] <- na_if(d2['CIG_0'], 99)                
d2['ATTEND'] <- na_if(d2['ATTEND'], 9) 
d2['DLMP_MM'] <- na_if(d2['DLMP_MM'], 99) 
d2['DBWT'] <- na_if(d2['DBWT'],9999) #normally distributed    
#MAGER # normally distributed, no missing value
d2['M_Ht_In'] <- na_if(d2['M_Ht_In'], 99) 
d2['PREVIS'] <- na_if(d2['PREVIS'], 99)  
d2['WTGAIN'] <- na_if(d2['WTGAIN'], 99) 
d2['DOB_TT'] <- na_if(d2['DOB_TT'], 9999) 
#SEX #no missing value 
d2['FEDUC'] <- na_if(d2['FEDUC'],9) #cate w order
d2['BFACIL'] <- na_if(d2['BFACIL'],9)
#DOB_MM #no missing valude 
d2['FRACE6'] <- na_if(d2['FRACE6'], 9)
#DOB_WK #no missing value
d2['PAY_REC'] <-na_if(d2['PAY_REC'],9)
#DMAR #no missing value
d2['RF_CESAR'] <- na_if(d2['RF_CESAR'], 'U')
d2['MEDUC'] <- na_if(d2['MEDUC'], 9) #cate w order
d2['LD_INDL'] <- na_if(d2['LD_INDL'], 'U')
d2['PRIORLIVE'] <- na_if(d2['PRIORLIVE'],99) 
d2['FAGECOMB'] <- na_if(d2['FAGECOMB'], 99)  
d2['MBSTATE_REC'] <- na_if(d2['MBSTATE_REC'], 3)
d2['PWgt_R'] <- na_if(d2['PWgt_R'],999)
d2['NO_RISKS'] <- na_if(d2['NO_RISKS'],9)

d2 <- d2 %>% mutate(PAY_REC = ifelse(PAY_REC >1, 2, PAY_REC)) # 1 or 2 <- binary
d2 <- d2 %>% mutate(CIG_0 = ifelse(CIG_0 >0, 1, CIG_0)) # no of cig either 0 or more than 0, binary
d2 <- d2 %>% mutate(PRIORLIVE = ifelse(PRIORLIVE >0, 1, PRIORLIVE)) #number of prior lives ether 0 or more than 0, binary
d2['DOB_TT'] = floor(d2['DOB_TT']/100) #HHMM -> HH, drop mins


d2 = select(d2, BMI, CIG_0,DLMP_MM, DOB_MM, DBWT, MAGER,M_Ht_In,PREVIS,WTGAIN,DOB_TT,FAGECOMB,
                  FEDUC,MEDUC,FRACE6,MBSTATE_REC,PRIORLIVE,PAY_REC,SEX,ATTEND,BFACIL,DMAR,RF_CESAR, 
                  LD_INDL,DOB_WK,PWgt_R, NO_RISKS)

#Remove all na.
d2 <- na.omit(d2)

#original data is too large, select a sample. 
sample         =     sample(1:dim(d2)[1], 5000)
d2 = d2[sample,]


#make categorical random variables as dummy variables. 
all.terms = as.formula(DBWT ~ 
                         PWgt_R +
                         DLMP_MM +
                         DOB_MM  +
                         BMI     +
                         MAGER   +
                         M_Ht_In +
                         PREVIS  +
                         WTGAIN +
                         DOB_TT +
                         FAGECOMB +
                         CIG_0 +
                         PAY_REC +
                         PRIORLIVE +
                         FEDUC + 
                         MEDUC + 
                         NO_RISKS +
                         as.factor(FRACE6) +
                         as.factor(MBSTATE_REC) +
                         as.factor(SEX)   + 
                         as.factor(ATTEND)  +
                         as.factor(BFACIL) + 
                         as.factor(DMAR) +
                         as.factor(RF_CESAR) + 
                         as.factor(LD_INDL) +
                         as.factor(DOB_WK) 
)
lm.fit   =     lm(all.terms, data = d2)
X = model.matrix(lm.fit)
y = d2$DBWT 

#(d) For one on the 100 samples, show the side-by-side boxplots of train and test residuals 
M               =     100   
Rsq.test.rid    =    rep(0,M)  
Rsq.train.rid   =    rep(0,M)
rid.time        =    rep(0,M)

Rsq.test.las    =     rep(0,M)  
Rsq.train.las   =     rep(0,M)
las.time        =     rep(0,M)

Rsq.test.eln    =     rep(0,M)  
Rsq.train.eln   =     rep(0,M)
eln.time        =     rep(0,M)

Rsq.test.rf    =     rep(0,M)  
Rsq.train.rf   =     rep(0,M)
rf.time        =     rep(0,M)


n = dim(X)[1]
p = dim(X)[2]-1


one_sample          =     sample(1:M, 1) 
for (m in c(1:M)) {
  
  train            =     sample(1:n, floor(0.8*n))
 

  X.train          =     X[train, ]
  y.train          =     y[train ]
  X.test           =     X[-train, ]
  y.test           =     y[-train]
  
    
  #Ridge
  rid.start       <- Sys.time()
  cv.fit.rid       =     cv.glmnet(X.train, y.train, intercept = FALSE, alpha = 0, nfolds = 10)
  d2.rid           =     glmnet(X.train, y.train,intercept = FALSE, alpha = 0, lambda = cv.fit.rid$lambda.min)
  rid.end          <- Sys.time()
  rid.time[m]      =      rid.end - rid.start
  
  y.train.hat      =     predict(d2.rid, newx = X.train, type = "response")
  y.test.hat       =     predict(d2.rid, newx = X.test, type = "response")
  
  Rsq.test.rid[m]  =     1-mean((y.test - y.test.hat)^2)/mean((y.test - mean(y.test))^2)
  Rsq.train.rid[m] =     1-mean((y.train - y.train.hat)^2)/mean((y.train - mean(y.train))^2)  
  
  
  #Lasso
  las.start       <- Sys.time()
  cv.fit.las      =     cv.glmnet(X.train, y.train, intercept = FALSE, alpha = 1, nfolds = 10)
  d2.las          =     glmnet(X.train, y.train,intercept = FALSE, alpha = 1, lambda = cv.fit.las$lambda.min)
  las.end         <- Sys.time()
  las.time[m]        = las.end - las.start
  
  y.train.hat   = predict(d2.las, newx = X.train,type = "response")
  y.test.hat   = predict(d2.las, newx = X.test,type = "response")

  Rsq.test.las[m]   =     1-mean((y.test - y.test.hat)^2)/mean((y.test - mean(y.test))^2)
  Rsq.train.las[m]  =     1-mean((y.train - y.train.hat)^2)/mean((y.train - mean(y.train))^2)  
  
  
  #Elastic-net
  eln.start           <-     Sys.time()
  cv.fit.eln           =     cv.glmnet(X.train, y.train, intercept = FALSE, alpha = 0.5, nfolds = 10)
  d2.eln               =     glmnet(X.train, y.train,intercept = FALSE, alpha = 0.5, lambda = cv.fit.eln$lambda.min)
  eln.end              <-    Sys.time()
  eln.time[m]             =     eln.end - eln.start
  
  y.train.hat   = predict(d2.eln, newx = X.train, type = "response")
  y.test.hat   = predict(d2.eln, newx = X.test, type = "response")

  Rsq.test.eln[m]   =     1-mean((y.test - y.test.hat)^2)/mean((y.test - mean(y.test))^2)
  Rsq.train.eln[m]  =     1-mean((y.train - y.train.hat)^2)/mean((y.train - mean(y.train))^2)  
  
  
  #Randomforest
  #drop the intercept, make the matrices as dataframe
  
  rf.df            =     data.frame(X[,-1], y)
  d2.rf            =     randomForest(y~., data=rf.df, subset = train, mtry= floor(sqrt(p)), importance=TRUE)
  
  y.train.hat      =     predict(d2.rf, newdata = rf.df[train,], type="response")
  y.test.hat       =     predict(d2.rf, newdata = rf.df[-train,], type="response")
  
  y.train          =     rf.df[train, "y"]
  y.test           =     rf.df[-train, "y"]
  
  Rsq.test.rf[m]   =     1-mean((y.test - y.test.hat)^2)/mean((y.test - mean(y.test))^2)
  Rsq.train.rf[m]  =     1-mean((y.train - y.train.hat)^2)/mean((y.train - mean(y.train))^2)  
  
  
  #One of 100 samples
  if (one_sample  == m) {
    X.os.train          =     X[train, ]
    y.os.train          =     y[train ]
    X.os.test           =     X[-train, ]
    y.os.test           =     y[-train]
  }
  
  print(m)
}

#4
#(a)Show the side-by-side boxplots of R2 test, R2 train. We want to see two panels. One for training, and the other for testing. (1 slide) 
Rsq.train <- data.frame(Rsq.train.rid, Rsq.train.las, Rsq.train.eln, Rsq.train.rf)
colnames(Rsq.train)     =     c( "Ridge", "Lasso", "Elastic net", "Random forest")
b1 = boxplot(Rsq.train, ylab="R squared", main="Train data set", col=(c("blue","red", "green", "yellow")))

Rsq.test <- data.frame(Rsq.test.rid, Rsq.test.las, Rsq.test.eln, Rsq.test.rf)
colnames(Rsq.test)     =     c( "Ridge", "Lasso", "Elastic net", "Random forest")
b2 = boxplot(Rsq.test, ylab="R squared", main="Test data set", col=(c("blue","red", "green", "yellow")))




#plot one of 100 sample cross validation curve
cv.os.ridge    =      cv.glmnet(X.os.train, y.os.train, intercept = FALSE, alpha=0, nfolds = 10)
plot(cv.os.ridge)
title("Ridge", line = 3)

cv.os.las    =      cv.glmnet(X.os.train, y.os.train, intercept = FALSE, alpha=1, nfolds = 10)
plot(cv.os.las)
title("Lasso", line = 3)

cv.os.eln    =      cv.glmnet(X.os.train, y.os.train, intercept = FALSE, alpha=0.5, nfolds = 10)
plot(cv.os.eln)
title("Elastic net", line = 3)

#Avg time to fit for each method of 100 samples 
mean(rid.time)
mean(las.time)
mean(eln.time)

#(d) For one on the 100 samples, show the side-by-side boxplots of train and test residuals (1 slide).
X.os.train          
y.os.train          
X.os.test          
y.os.test          



#(d) For one on the 100 samples, show the side-by-side boxplots of train and test residuals 
#Ridge residual
cv.os.rid                =      cv.glmnet(X.os.train, y.os.train, intercept = FALSE, alpha=0, nfolds = 10)
os.rid                   =      glmnet(X.os.train, y.os.train ,intercept = FALSE, alpha = 0, lambda = cv.os.rid$lambda.min)
y.os.train.hat.rid       =      predict(os.rid, newx= X.os.train, type ="response")
y.os.test.hat.rid        =      predict(os.rid, newx= X.os.test, type ="response")

y.os.train.red.rid          =      y.os.train - y.os.train.hat.rid
y.os.test.red.rid           =      y.os.test - y.os.test.hat.rid

#Lasso residual
cv.os.las                =      cv.glmnet(X.os.train, y.os.train, intercept = FALSE, alpha=1, nfolds = 10)
os.las                   =      glmnet(X.os.train, y.os.train ,intercept = FALSE, alpha = 1, lambda = cv.os.las$lambda.min)
y.os.train.hat.las       =      predict(os.las, newx= X.os.train, type ="response")
y.os.test.hat.las        =      predict(os.las, newx= X.os.test, type ="response")

y.os.train.red.las          =      y.os.train - y.os.train.hat.las
y.os.test.red.las           =      y.os.test - y.os.test.hat.las

#Elastic - net residual
cv.os.eln                =      cv.glmnet(X.os.train, y.os.train, intercept = FALSE, alpha=0.5, nfolds = 10)
os.eln                   =      glmnet(X.os.train, y.os.train ,intercept = FALSE, alpha = 0.5, lambda = cv.os.eln$lambda.min)
y.os.train.hat.eln       =      predict(os.eln, newx= X.os.train, type ="response")
y.os.test.hat.eln        =      predict(os.eln, newx= X.os.test, type ="response")

y.os.train.red.eln          =      y.os.train - y.os.train.hat.eln
y.os.test.red.eln           =      y.os.test - y.os.test.hat.eln

#Randomforest residual
df.train.rf      =     data.frame(X.os.train[,-1], y.os.train) #make the x matrix to data frame.
df.test.rf       =     data.frame(X.os.test[,-1], y.os.test)

os.rf            =     randomForest(y.os.train~., data=df.train.rf, mtry= floor(sqrt(p)), importance=TRUE)

y.os.train.hat.rf   =     predict(os.rf, newdata = df.train.rf, type="response")
y.os.test.hat.rf   =      predict(os.rf, newdata = df.test.rf, type="response")

y.os.train.red.rf          =      y.os.train - y.os.train.hat.rf
y.os.test.red.rf           =      y.os.test - y.os.test.hat.rf


os.train.red = data.frame(y.os.train.red.rid, y.os.train.red.las, y.os.train.red.eln, y.os.train.red.rf)
colnames(os.train.red) = c('Ridge','Lasso', 'Elastic net', 'Random forest')
boxplot(os.train.red, main='Train data set', ylab='Residuals', col=(c("blue","red", "green", "yellow")))

os.test.red = data.frame(y.os.test.red.rid, y.os.test.red.las, y.os.test.red.eln, y.os.test.red.rf)
colnames(os.test.red) = c('Ridge','Lasso', 'Elastic net', 'Random forest')
boxplot(os.test.red, main='Test data set', ylab='Residuals', col=(c("blue","red", "green", "yellow")))



#5. For all the data do the following:
#rid
for (m in (1:1)){
  all.rid.start <- Sys.time()
  cv.rid           =     cv.glmnet(X, y, intercept = FALSE, alpha = 0, nfolds = 10)
  d2.fit.rid       =     glmnet(X, y,intercept = FALSE, alpha = 0, lambda = cv.rid$lambda.min)
  all.rid.end <- Sys.time()
  all.rid.time = all.rid.end - all.rid.start
}
#90 % interval and time to fit in the model.
quantile(Rsq.test.rid, prob = c(0.05, 0.95))
all.rid.time

#Lasso
for (m in (1:1)){
  all.las.start <- Sys.time()
  cv.las           =     cv.glmnet(X, y, intercept = FALSE, alpha = 1, nfolds = 10)
  d2.fit.las           =     glmnet(X, y,intercept = FALSE, alpha = 1, lambda = cv.las$lambda.min)
  all.las.end <- Sys.time()
  all.las.time = all.las.end - all.las.start
}
#90 % interval and time to fit in the model.
quantile(Rsq.test.las, prob = c(0.05, 0.95))
all.las.time

#Elastic-net
for (m in (1:1)){
  all.eln.start <- Sys.time()
  cv.eln               =     cv.glmnet(X, y, intercept = FALSE, alpha = 0.5, nfolds = 10)
  d2.fit.eln           =     glmnet(X, y,intercept = FALSE, alpha = 0.5, lambda = cv.eln$lambda.min)
  all.eln.end <- Sys.time()
  all.eln.time = all.eln.end - all.eln.start
}
#90 % interval and time to fit in the model.
quantile(Rsq.test.eln, prob = c(0.05, 0.95))
all.eln.time

#Random forest
rf.df            =     data.frame(X[,-1], y)
for (m in (1:1)){
  all.rf.start <- Sys.time()
  d2.fit.rf  =  randomForest(y~., data=rf.df, mtry= floor(sqrt(p)), importance=TRUE)
  all.rf.end <- Sys.time()
  all.rf.time = all.rf.end - all.rf.start
}
#90 % interval and time to fit in the model.
quantile(Rsq.test.rf, prob = c(0.05, 0.95))
all.rf.time


# Present bar-plots of the estimated coefficients (lasso, ridge, elastic-net), the importance of the parameters(standarized)
features         =     colnames(X)[-1]
s       =    apply(X, 2, sd) 

#Elastic net coefficient
#cv.eln          =     cv.glmnet(X, y, intercept = FALSE, alpha = 0.5, nfolds = 10)
#d2.fit.eln      =     glmnet(X.train, y.train,intercept = FALSE, alpha = 0.5, lambda = cv.eln$lambda.min)
beta.hat.eln     =     d2.fit.eln$beta[ ,d2.fit.eln$lambda==cv.eln$lambda.min]*s
beta.hat.eln     =     beta.hat.eln[-1]#take out the intercept.
betaS.eln         =     data.frame(features, as.vector(beta.hat.eln))
colnames(betaS.eln)     =     c( "feature", "value")

#Ridge coefficient
#cv.rid           =     cv.glmnet(X, y, intercept = FALSE, alpha = 0, nfolds = 10)
#d2.fit.rid       =     glmnet(X, y,intercept = FALSE, alpha = 0, lambda = cv.rid$lambda.min)

beta.hat.rid     =     d2.fit.rid$beta[ ,d2.fit.rid$lambda==cv.rid$lambda.min]*s
beta.hat.rid     =     beta.hat.rid[-1]#take out the intercept.
betaS.rid         =     data.frame(features, as.vector(beta.hat.rid))
colnames(betaS.rid)     =     c( "feature", "value")

#Lasso coefficient
#cv.las          =     cv.glmnet(X, y, intercept = FALSE, alpha = 1, nfolds = 10)
#d2.fit.las      =     glmnet(X, y,intercept = FALSE, alpha = 1, lambda = cv.las$lambda.min)
beta.hat.las     =     d2.fit.las$beta[ ,d2.fit.las$lambda==cv.las$lambda.min]*s
beta.hat.las     =     beta.hat.las[-1]#take out the intercept.
betaS.las         =     data.frame(features, as.vector(beta.hat.las))
colnames(betaS.las)     =     c( "feature", "value")

#Random Forest Coefficients 
beta.imp.rf          =  d2.fit.rf$importance[,1]
betaS.rf             =  data.frame(features, as.vector(beta.imp.rf))
colnames(betaS.rf)     =     c( "feature", "value")

#change the coefficients' order
betaS.las$feature     =  factor(betaS.las$feature, levels = betaS.eln$feature[order(betaS.eln$value, decreasing = TRUE)])
betaS.eln$feature     =  factor(betaS.eln$feature, levels = betaS.eln$feature[order(betaS.eln$value, decreasing = TRUE)])
betaS.rid$feature     =  factor(betaS.rid$feature, levels = betaS.eln$feature[order(betaS.eln$value, decreasing = TRUE)])
betaS.rf$feature      =  factor(betaS.rf$feature, levels = betaS.eln$feature[order(betaS.eln$value, decreasing = TRUE)])


lsPlot =  ggplot(betaS.las, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Lasso") + ylab("Value") 


enPlot =  ggplot(betaS.eln, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black") +
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Elastic net") + ylab("Value")


rgPlot =  ggplot(betaS.rid, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Ridge") + ylab("Value")


rfPlot =  ggplot(betaS.rf, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Random forest")+ylab("Variable Importance")

grid.arrange(enPlot, lsPlot, rgPlot, rfPlot, nrow = 4)


