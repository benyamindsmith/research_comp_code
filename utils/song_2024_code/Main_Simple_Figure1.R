#--------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------#

#                          A general M-estimation theory in semi-supervised framework
#                     part 1: To produce the subfigure (a) of Figure 1 in Sectio 4.1 of our paper 

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
n=250                             # the labelled data size sequence
p=4                               # the dimension of the predictor vector 
option="i"                        # this quantity represents the data setting, now "i" corresponds to 
                                  #   setting (i) in our paper. Other choices of "option" can be found
                                  #   in the definition of the function "GenerateData" from the file
                                  #   "dataGeneration.R". 
tau_0=0.5                         # the quantile level; only useful for quantile working model 
rep=1000                          # the number of replications
####################(3) Replications ##############################################
alpha_replications <- vector()
for (k in 1:rep)
{ 
  set.seed(k+20220122)
  print(c("k",k))
  #######################(3.1) Data generation ##############################
  DesiredData<- GenerateData(n=n,p=p,option=option)
  data_labelled = DesiredData$Data.labelled  # the labelled data 
  #########################(3.2) GBIC_ppo ###################################
  # determine the type of working model by the quantity "option"
  if (option%in%c("i","W1","S1")){
    type= "linear" 
  } else if (option%in%c("ii","W2","S2")){
    type="logistic"
  } else if(option%in%c("iii","W3","S3")){
    type="quantile"
  }
  # data splitting 
  labelled_data_part1<- data_labelled[1:round(n/2),]
  labelled_data_part2<- data_labelled[(round(n/2)+1):n,]
  # the supervised estimate
  hattheta_supervised_part1<- SupervisedEst(labelled_data_part1,tau=tau_0,option=option)$Est.coef
  hattheta_supervised_part2<- SupervisedEst(labelled_data_part2,tau=tau_0,option=option)$Est.coef
  # the first derivative of the loss function L using the supervised estimate 
  L_first_derivative_part1<- L_first_derivative(labelled_data_part1,hattheta_supervised_part2,type=type,tau_level=tau_0)
  L_first_derivative_part2<- L_first_derivative(labelled_data_part2,hattheta_supervised_part1,type=type,tau_level=tau_0)
  # new data integrating the first derivatives of loss function
  Ynew<- rbind(L_first_derivative_part1,L_first_derivative_part2)
  covariates_labelled<- data_labelled[,-1]
  # GBIC_ppo to select the polynomial order 
  gamma=10
  GBICscrores<-apply(as.matrix(1:gamma,1,gamma), 1, function(t) GBIC_ppo(Ynew,covariates_labelled,t))
  alpha<- which.min(GBICscrores)
  ###################(3.3) save results ####################################
  alpha_replications <- c(alpha_replications,alpha)
}
############################(4) Figures ##############################################
dev.new()
pdf("subfigure_GBIC_ppo.pdf")
df1<- data.frame(alpha_replications)
colnames(df1) <-"poly_order"
alpha1_plot<-ggplot(data = df1, mapping = aes(x =poly_order)) +
  geom_histogram( mapping = aes(x =poly_order),color="black", fill="white",bins=30)+
  labs(title=paste("Setting (",option,")",sep=""),x="Polynomial Order", y = "Frequency")

p1=alpha1_plot+ theme_bw() +theme(plot.title = element_text(hjust = 0)) +theme(aspect.ratio = 1)+
  theme(plot.title = element_text(size=10))+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+scale_y_continuous(expand = c(0,0))
p1
dev.off()


