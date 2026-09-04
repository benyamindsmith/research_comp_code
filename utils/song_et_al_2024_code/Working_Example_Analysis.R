#--------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------#

#                          A general M-estimation theory in semi-supervised framework
#             part 5: To demonstrate to users how to use our proposed method in the case study example

#--------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------#

rm(list = ls())

####################(1) Required Packages ##########################################

library(MASS)
library(caret)
library(quantreg)
source("semi_supervised_methods.R") 
# NOTE: Ensure that the R script "semi_supervised_methods.R"  
# is in the same working directory as this file.



#####################(2) Global Parameters #########################################

tau_0=0.5                         # the quantile level; only useful for quantile working models 

type="quantile"                   # We consider quantile regression working model in the case study example.



####################(3) Data Preprocessing ##########################################

###(3.1) Read the simulated data### 
WorkingData<- as.data.frame(read.csv('WorkingExampleMatrix.csv', header = T))


###(3.2) Select the observations of 7 important predictors, the response and the variable "selection"###
ImportantVariables<- c("StTotal","Perc.Industrial","Perc.Residential","Perc.Vacant","Perc.Commercial",
                       "Perc.OwnerOcc","Perc.Minority","MedianHouseholdIncome","selection")
WorkingExampleIV<- WorkingData[,ImportantVariables]
WorkingExampleIV$MedianHouseholdIncome<- WorkingExampleIV$MedianHouseholdIncome/10^3  #aviod this column is too big 


###(3.3) According to the variable "selection", we identify the labelled data set and the unlabelled data set### 
index_labelled<- which(WorkingExampleIV$selection==1)
index_unlabelled<- which(WorkingExampleIV$selection==0)
col_response<- which(colnames(WorkingExampleIV)=="StTotal")
col_selection<- which(colnames(WorkingExampleIV)=="selection")

data_labelled<- as.matrix(cbind(WorkingExampleIV[index_labelled,col_response],
                                WorkingExampleIV[index_labelled,-c(col_response,col_selection)]))
data_unlabelled <- as.matrix(WorkingExampleIV[index_unlabelled,-c(col_response,col_selection)])


  
#########################(4) Estimators ##############################################
  
  ### the supervised one
  estimation_supervised <- PSSE(data_labelled,data_unlabelled,c1=1,type=type,sd=TRUE,tau=tau_0) 
                                                  #NOTE: when "c1=1", our proposed estimator degenerates to the supervised one
  hattheta_supervised <- estimation_supervised$Hattheta  # coefficient estimate 
  sd_hattheta_supervised<- estimation_supervised$sd.of.hattheta # sd estimate 
  
  
  ### our proposed one
  estimation_proposed <- PSSE(data_labelled,data_unlabelled,type=type,sd=TRUE,tau=tau_0)
  hattheta_proposed <- estimation_proposed$Hattheta  # coefficient estimate 
  sd_hattheta_proposed<- estimation_proposed$sd.of.hattheta # sd estimate 
  alpha<- estimation_proposed$alpha
  
  ## DRESS proposed by Kawakita and Kanamori (2013) 
  estimation_DRESS<- DRESS(data_labelled,data_unlabelled,type=type,sd=TRUE,tau=tau_0,L=alpha)
  hattheta_DRESS <- estimation_DRESS$Hattheta # coefficient estimate 
  sd_hattheta_DRESS<- estimation_DRESS$sd.of.hattheta # sd estimate 
  


############################(5) Tables ##############################################

results_total <- round(rbind(hattheta_supervised,sd_hattheta_supervised,
                             hattheta_DRESS,sd_hattheta_DRESS,(sd_hattheta_supervised/sd_hattheta_DRESS)^2,
                             hattheta_proposed,sd_hattheta_proposed,(sd_hattheta_supervised/sd_hattheta_proposed)^2),3)


rownames(results_total) <- c("Estimate","SEE","Estimate","SEE","ARE","Estimate","SEE","ARE")
colnames(results_total) <-c("intercept", "Perc.Industrial","Perc.Residential","Perc.Vacant","Perc.Commercial",
                            "Perc.OwnerOcc","Perc.Minority","MedianHouseholdIncome")

results_total




# save the results in a txt file 
sink(paste("WorkingEXampleResults.txt",sep=""),append=F,split = T)
cat("\n #----------------------------------------Regression analysis results for the simulated version of real data-----------------# \n")
print(results_total)
cat("Methods: supervised, DRESS, proposed\n")
sink()




