#load required libraries
library(data.table)

#load the data 

s2<-read.csv("~/Kabul_pred.csv")
s2<-read.csv("C:/Users/mr3882/OneDrive - Northern Arizona University/KABUL_paper_rev/data_to_share/Kabul_pred.csv")
##list all variable of which we want to find the best combiantion of four that maximize model's performance

vrb<-c("TOT_IMM","TOT_IMM_l1","TOT_IMM_l2",  
       "tot_pop","tot_pop_l1","tot_pop_l2",  
       "casualties_inKabul", "casualties_inKabul_l1","casualties_inKabul_l2",
       "casualties_outKabul", "casualties_outKabul_l1", "casualties_outKabul_l2",
       "ODA_grants_tot_billions","ODA_grants_tot_billions_l1", "ODA_grants_tot_billions_l2",
       "ODA_loan_tot_billions","ODA_loan_tot_billions_l1","ODA_loan_tot_billions_l2",
       "Tech_coop", "Tech_coop_l1","Tech_coop_l2")

#find all possible combioantion of the above variables
pr<-combn(vrb, 4)

#to simplify models strucuture (also considering observation number and time series lenght that would noty allow for multiple lags)
#we remove all combiantion that have the same predictor with l1 and l2 included
for (i in  1:ncol(pr)){
  ww<-c(pr[,i])
  ww<-gsub("_l1","",ww)
  ww<-gsub("_l2","",ww)
  if (length(unique(ww))!=length(ww)){ pr[,i]<-NA}
}

DT <- as.data.table(pr)
DT<-DT[,which(unlist(lapply(DT, function(x)!all(is.na(x))))),with=F]
DT<-as.matrix(DT)


##best model selection based on AIC
#vector of classes to fit
cls<-c("cl_ch1", "cl_ch2", "cl_ch3", "cl_ch4")
aic3<-c()

##loop to find the best model for each class and aasing the class name
for (j in 1:length(cls)){
for (i in 1:ncol(DT)){
  ff2<-as.formula(paste0(cls[j],"~",DT[1,i], "+",DT[2,i], "+",DT[3,i], "+",DT[4,i] ))
  
  g3<-lm(ff2 ,data=s2 )
  aic3[i]<-ifelse(mean(abs(g3$residual))<1,NA,AIC(g3))
  
}


mm3<-which.min(aic3)

ff3<-as.formula(paste0(cls[j],"~",DT[1,mm3], "+",DT[2,mm3], "+",DT[3,mm3], "+",DT[4,mm3] ))

g3<-lm(ff3,data=s2)
assign(cls[j], g3)

}

library(stargazer)
stargazer(cl_ch1, type = "text", title = "Class 1 LM Summary", out = "model_summary.txt")

summary(cl_ch4)
AIC(cl_ch1)
