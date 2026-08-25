#--------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------#

#                          A general M-estimation theory in semi-supervised framework
#                     part 4: To produce the subtable of Table 6 in Sectio 4.1 of our paper 

#--------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------#
####################(1) Required Packages ##########################################
library(MASS)
library(caret)
library(quantreg)
source("semi_supervised_methods.R") 
source("dataGeneration.R")  
source("SupervisedEstimation.R")
# NOTE: Ensure that the R scripts "semi_supervised_methods.R", "dataGeneration.R" 
# and "SupervisedEstimation.R" are in the same working directory as this file.
#####################(2) Global Parameters #########################################
n=300                             # the labelled data size sequence
N=500                             # the unlabelled data size
p=7                               # the dimension of the predictor vector 
rep=1000                          # the number of replications 
option="ii"                       # this quantity represents the data setting, now "ii" corresponds to 
                                  #   setting (ii) in our paper. Other choices of "option" can be found
                                  #   in the definition of the function "GenerateData" from the file
                                  #   "dataGeneration.R". 
polyOrder<- 4                     # the polynomial order. It is useful when we construct the polynomial
                                  #    functions of X as Z. Generally, we select the polynomial order by our 
                                  #    proposed selector GBIC_ppo. Since this file is to run the subfigures,
                                  #    we use the same selected polynomial order for all replications under certain
                                  #    option. If the polynomial order isn't given, the main function of our method
                                  #   "PSSE" would select the polynomial order by the selector GBIC_ppo;
                                  #    more details can be found in the file "semi_supervised_methods.R". 
tau_0=0.5                         # the quantile level; only useful for quantile working models 
#####################(3) Target Parameter #########################################
set.seed(1230988)
LargeLabelledData<-GenerateData(n=10^5,p=p,option=option)$Data.labelled
target_parameter=SupervisedEst(LargeLabelledData,tau=tau_0,option=option)$Est.coef
# NOTE: As we specified in the paper, the true value of the target parameter
# is computed by generating labelled data of size $10^5$. 
####################(4) Replications ##############################################
results_supervised <- vector()
results_proposed <- vector()
results_PI<- vector()
results_EASE <- vector()
results_DRESS <- vector()
sd_proposed_matrix <- vector()
cp_proposed_matrix <- vector()
for (k in 1:rep)
{ 
  set.seed(k+20220122)
  #print(c("k",k))
  #######################(4.1) Data generation ##############################
  DesiredData<- GenerateData(n=n,N=N,p=p,option=option)
  data_labelled = DesiredData$Data.labelled  # the labelled data 
  data_unlabelled = DesiredData$Data.unlabelled # the unlabelled data 
  #######################(4.2) supervised estimator #########################
  hattheta_supervised=SupervisedEst(data_labelled,tau=tau_0,option=option)$Est.coef
  ####################(4.3) semi-supervised estimators ######################
  ### determine the type of working model by the quantity "option"
  if (option%in%c("i","W1","S1")){
    type= "linear" 
  } else if (option%in%c("ii","W2","S2")){
    type="logistic"
  } else if(option%in%c("iii","W3","S3")){
    type="quantile"
  }
  ### our proposed one
  estimation_proposed <- PSSE(data_labelled,data_unlabelled,type=type,sd=TRUE,tau=tau_0,alpha=polyOrder)
  hattheta_proposed <- estimation_proposed$Hattheta  # coefficient estimate 
  sd_hattheta_proposed<- estimation_proposed$sd.of.hattheta # sd estimate 
  cp_hattheta_proposed<- (abs(hattheta_proposed-target_parameter)<=1.96*sd_hattheta_proposed) # coverage rate
  if (type=="linear"){
    ### PI proposed by Azriel et al. (2021)
    estimation_PI <- PI(data_labelled,data_unlabelled)
    hattheta_PI <- estimation_PI$Hattheta
    ### EASE proposed by Chakrabortty and Cai (2018)
    estimation_EASE <- EASE(data_labelled,data_unlabelled,K=5,H=40,r=2)
    hattheta_EASE <- estimation_EASE$Hattheta
  }else{
    hattheta_PI=NaN
    hattheta_EASE=NaN
  }
  ## DRESS proposed by Kawakita and Kanamori (2013) 
  estimation_DRESS<- DRESS(data_labelled,data_unlabelled,type=type,tau=tau_0,L=polyOrder)
  hattheta_DRESS <- estimation_DRESS$Hattheta
  ###################(4.4) save results ##########################
  results_supervised<-rbind(results_supervised,hattheta_supervised) 
  results_proposed <- rbind(results_proposed,hattheta_proposed)
  results_PI <- rbind(results_PI,hattheta_PI)
  results_EASE <- rbind(results_EASE,hattheta_EASE)
  results_DRESS <- rbind(results_DRESS,hattheta_DRESS)
  sd_proposed_matrix<- rbind(sd_proposed_matrix,sd_hattheta_proposed)
  cp_proposed_matrix<- rbind(cp_proposed_matrix,cp_hattheta_proposed)
}
############################(5) Tables ##############################################
results_total <- round(cbind(target_parameter,
                       colMeans(results_supervised)-target_parameter,apply(results_supervised,2,sd),
                       colMeans(results_PI)-target_parameter,apply(results_PI,2,sd), apply(results_supervised,2,var)/apply(results_PI,2,var),
                       colMeans(results_EASE)-target_parameter,apply(results_EASE,2,sd),apply(results_supervised,2,var)/apply(results_EASE,2,var),
                       colMeans(results_DRESS)-target_parameter,apply(results_DRESS,2,sd),apply(results_supervised,2,var)/apply(results_DRESS,2,var),
                       colMeans(results_proposed)-target_parameter,apply(results_proposed,2,sd),apply(results_supervised,2,var)/apply(results_proposed,2,var),
                       colMeans(sd_proposed_matrix),colMeans(cp_proposed_matrix)
),3)
colnames(results_total) <- c("real_value","bias","SE","bias","SE","ARE","bias","SE","ARE","bias","SE","ARE","bias","SE","ARE","SEE","CP")
rownames(results_total) <-NULL
sink(paste("subtable.txt",sep=""),append=F,split = T)
cat("\n Estimation results of theta* for the logistic regression working model \n")
cat("Methods: supervised, PI, EASE, DRESS, proposed\n")
cat(paste("The setting of (p,n,N):",p,n,N,sep=" "))
cat("\n")
print(results_total)
sink()