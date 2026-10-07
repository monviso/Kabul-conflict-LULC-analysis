######Run XGBoost classification model
#============================================================================================
#Landsast metrix are derived from Google Earth Engine at:
#LADSAT: https://code.earthengine.google.com/60c8cefd56c1b3c11c3c0014089b9b07
#DEM/Slope: https://code.earthengine.google.com/4d321de200caadffb750e3f64f1b59fb
#the aboveGEE code download images ready for wall-to-wall mapping into Google drive
##XGBoost_data.csv can be easely produced by running extractions on the expoerted GEE images using the polygons available at https://zenodo.org/records/20218399 (where also final wall-to wall raster are available)
#============================================================================================
library(caTools)
library(xgboost)
library(ParBayesianOptimization)
library(doParallel)
library(dplyr)
library(caret)
library(Boruta)


##get model ready table

df<-read.csv("~/data_XGBoost_L7.csv")
#removing unwanted classes and clean
df1<-df[-which(df$class%in%c(0,12,13)),]
df1<-df1[which(complete.cases(df1)==T),]

lv<-unique(df1$class)
df1$class<-as.factor(df1$class)
df1<-df1[,-which(colnames(df1)%in%c(paste0("elev.",seq(1:10)), paste0("slope.",seq(1:10))))]

##dividing in training and validation by class 
for (i in 1:length(lv)){
  show(lv[i])
  df2<-df1[which(df1$class==lv[i]),]
  split <- sample.split(seq(1,nrow(df2)), SplitRatio = 0.7) 
  train1<-df2[which(split==T),]
  test1<-df2[which(split==F),]
  if (i==1){train<-train1; test<-test1}else{train<-rbind(train,train1); test<- rbind(test, test1)}
}

###subsetting train data
sample_more <- function(pop, n, seed = NULL) {
  set.seed(seed) # For reproducibility, if desired. Defaults to NULL, which is no seed
  m <- length(pop)
  if(n <= m) {  # handles case when n is smaller than the population size 
    sample(pop, n, replace = FALSE) 
  } else { # handle case when n is greater than population size
    c(sample(pop, m, replace = T), sample(pop, n-m, replace = T))
  }
}

nb_tr<-10
set.seed(1)
for (i in 1:length(lv)){
  show(lv[i])
  t1<-train[which(train$class==lv[i]),]
  rw<-floor(nrow(t1)/nb_tr)*nb_tr
  t1<-t1[sample(nrow(t1),rw),]
  t1$sq<-rep(seq(1,nb_tr),rw/nb_tr)
  
  
  if (i==1){tA<-t1}else{tA<-rbind(tA,t1)}
  
  
}

####run model in parallel

##remove all 0 predictors
rem<-c()
for (i in 7:ncol(tA)){
  ss<-sd(tA[,i])
  if(ss==0){
    rem<-c(rem, colnames(tA[i]))}
}
tA<-tA[,-which(colnames(tA)%in%rem)]

##XGBoost approach
##Bortua -absed preliminary varible selection

boruta_data <- tA[, c("class", names(tA)[7:2218])]

boruta_data$class <- as.factor(boruta_data$class)

set.seed(123)
BorutaOntA <- Boruta(
  class ~ .,
  data = boruta_data,
  getImp = getImpXgboost,
  doTrace = 2
)

conf<-names(BorutaOntA$finalDecision[which(BorutaOntA$finalDecision=="Confirmed")])
tA2<-tA[,c(4,5, 2219,which(colnames(tA)%in%conf))]

tA2$class3<-as.numeric(tA2$class)-1

X<-as.matrix(tA2[,c(4:135)])
Y<-as.numeric(tA2$class3)
Folds <- list(
  Fold1 = as.integer(seq(1,nrow(tA2),by = 3))
  , Fold2 = as.integer(seq(2,nrow(tA2),by = 3))
  , Fold3 = as.integer(seq(3,nrow(tA2),by = 3))
)


obj_func <- function(eta, max_depth, min_child_weight, subsample, lambda, alpha, gamma, colsample_bytree) {
  
  param <- list(
    
    # Hyper parameters 
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    lambda = lambda,
    alpha = alpha,
    gamma=gamma,
    colsample_bytree=colsample_bytree,
    
    # Tree model 
    booster = "gbtree",
    
    # Regression problem 
    #objective = "binary:logistic",
    objective = "multi:softmax",
    #objective = "reg:squarederror",
    
    # Use the Mean Absolute Percentage Error
    eval_metric ="merror", 
    #"merror",
    #"mae",
    nthread=8)
  
  xgbcv <- xgb.cv(params = param,
                  data = X,
                  label = Y,
                  num_class=11,
                  nround = 50,
                  folds = Folds,
                  prediction = TRUE,
                  early_stopping_rounds = 5,
                  verbose = 0,
                  maximize = F)
  
  lst <- list(
    
    # First argument must be named as "Score"
    # Function finds maxima so inverting the output
    #rr=as.matrix(xgbcv$evaluation_log),
    Score =-min(xgbcv$evaluation_log$test_merror_mean),
    
    # Get number of trees for the best performing model
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}
bounds <- list(eta = c(0.0001, 1),
               max_depth = c(1L, 400L),
               min_child_weight = c(0.1, 100),
               subsample = c(0.1, 1),
               lambda = c(0.1, 100),
               alpha = c(0.1, 100),
               gamma =c(0,100),
               colsample_bytree =c (0, 1))


cl <- makeCluster(8,outfile="")

registerDoParallel(cl)
clusterExport(cl,c('Folds','X', 'Y', 'bounds', 'obj_func'))

clusterEvalQ(cl,expr= {
  library(xgboost)
  library(ParBayesianOptimization)
})
set.seed(1234)
bayes_out_ucb <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, 
                          iters.n = 10, plotProgress = T, parallel = T, acq="ucb") #otherHalting=list(timeLimit=60))

stopCluster(cl)

opt_params <- append(list(booster = "gbtree", 
                          objective= "multi:softmax",
                          eval_metric = "merror"), 
                     getBestPars(bayes_out_ucb))

set.seed(12)
xgbcv <- xgb.cv(params = opt_params,
                data = X,
                label = Y,
                num_class=11,
                nround = 20,
                folds = Folds,
                prediction = TRUE,
                early_stopping_rounds = 5,
                verbose = 0,
                maximize = F)

# Get optimal number of rounds
nrounds = xgbcv$best_iteration

# Fit a xgb model
set.seed(12)
mdl <- xgboost(data = X, label = Y, 
               num_class=11,
               params = opt_params, 
               maximize = F, 
               early_stopping_rounds = 5, 
               nrounds = nrounds, 
               verbose = 0)


XT<-as.matrix(test[,which(colnames(test)%in%colnames(X))])
pr<-predict(mdl, XT)
test_cv<-data.frame(predicted=pr+1, actual=test$class)
tbx<-table(test_cv)
tbx<-table(test_cv)
cm<-confusionMatrix(tbx, mode = "everything")
capture.output(cm, file = "../results/XGBmulti_class_perf.csv")


