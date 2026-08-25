#--------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------#

#                          A general M-estimation theory in semi-supervised framework
#                     part 2: To produce the subfigure (a) of Figure 3 in Sectio 4.1 of our paper 

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
n=500                             # the labelled data size sequence
N=500                             # the unlabelled data size
p=4                               # the dimension of the predictor vector 
rep=1000                          # the number of replications 
option="i"                        # this quantity represents the data setting, now "i" corresponds to 
                                  #   setting (i) in our paper. Other choices of "option" can be found
                                  #   in the definition of the function "GenerateData" from the file
                                  #   "dataGeneration.R". 
polyOrder<- 4                     # the polynomial order. It is useful when we construct the polynomial
                                  #    functions of X as $Z$. Generally, we select the polynomial order by our 
                                  #    proposed selector GBIC_ppo. Since this file is to run the subfigures,
                                  #    we use the same selected polynomial order for all replications under certain
                                  #    option. If the polynomial order isn't given, the main function of our method
                                  #   "PSSE" would select the polynomial order by the selector GBIC_ppo;
                                  #    more details can be found in the file "semi_supervised_methods.R". 
tau_0=0.5                         # the quantile level; only useful for quantile working models 
c1_seq = seq(0,1,length=51)       # the $C_1$ sequence 
#####################(3) Target Parameter #########################################
set.seed(1230988)
LargeLabelledData<-GenerateData(n=10^5,p=p,option=option)$Data.labelled
target_parameter=SupervisedEst(LargeLabelledData,tau=tau_0,option=option)$Est.coef
# NOTE: As we specified in the paper, the true value of the target parameter
# is computed by generating labelled data of size $10^5$. 
####################(4) Replications ##############################################
MSE_c1_different<- numeric()
for(c_1 in c1_seq){
  results_proposed <- vector()
  for (k in 1:rep)
  { 
    set.seed(k+20220122)
    print(c("c_1,k",c_1,k))
    #######################(4.1) Data generation ##############################
    DesiredData<- GenerateData(n=n,N=N,p=p,option=option)
    data_labelled = DesiredData$Data.labelled  # the labelled data 
    data_unlabelled = DesiredData$Data.unlabelled # the unlabelled data 
    #######################(4.2) Our estimator  ##############################
    ### determine the type of working model by the quantity "option"
    if (option%in%c("i","W1","S1")){
      type= "linear" 
    } else if (option%in%c("ii","W2","S2")){
      type="logistic"
    } else if(option%in%c("iii","W3","S3")){
      type="quantile"
    }
    ### our proposed one
    estimation_proposed <- PSSE(data_labelled,data_unlabelled,type=type,c1=c_1,tau=tau_0,alpha=polyOrder)
    hattheta_proposed <- estimation_proposed$Hattheta
    ###################(4.3) save results ####################################
    results_proposed <- rbind(results_proposed,hattheta_proposed)
  }
  ###################(4.4) calculate MSE ########################################
  MSE_proposed <- sum((colMeans(results_proposed)-target_parameter)^2+apply(results_proposed,2,sd)^2) 
  MSE_c1_different<- c(MSE_c1_different,MSE_proposed)
}
############################(5) Figures ##############################################
dev.new()
pdf("subfigure_MSE_c1.pdf")
df1<- data.frame(c1_seq,MSE_c1_different)
alpha1_plot<-ggplot(data = df1, mapping = aes(x =c1_seq, y = MSE_c1_different)) + geom_line()+
  labs(title=paste("p=",p,", n=", n, ", N=",N,sep=""),x=expression('c'[1]), y = "MSE")
p1=alpha1_plot+ theme_bw() +theme(plot.title = element_text(hjust = 0)) +theme(aspect.ratio = 1)+
  theme(legend.position = c(0.1,0.067),panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
p1
dev.off()




