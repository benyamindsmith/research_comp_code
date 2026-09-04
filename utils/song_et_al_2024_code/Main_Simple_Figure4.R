#--------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------#

#                          A general M-estimation theory in semi-supervised framework
#                   part 3: To produce the subfigure (a) of Figure 4 in Sectio 4.1 of our paper 

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
seq_n=seq(250,500,length=6)       # the labelled data size sequence
N=500                             # the unlabelled data size
p=4                               # the dimension of the predictor vector 
rep=1000                          # the number of replications 
option="ii"                       # this quantity represents the data setting, now "ii" corresponds to 
                                  #   setting (ii) in our paper. Other choices of "option" can be found
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
#####################(3) Target Parameter #########################################
set.seed(1230988)
LargeLabelledData<-GenerateData(n=10^5,p=p,option=option)$Data.labelled
target_parameter=SupervisedEst(LargeLabelledData,tau=tau_0,option=option)$Est.coef
# NOTE: As we specified in the paper, the true value of the target parameter
# is computed by generating labelled data of size $10^5$. 
####################(4) Replications ##############################################
MSE_total<- vector()
for(n in seq_n)
{
  results_supervised <- vector()
  results_proposed <- vector()
  results_PI<- vector()
  results_EASE <- vector()
  results_DRESS <- vector()
  for (k in 1:rep)
  { 
    set.seed(k+20220122)
    print(c("n","k",n,k))
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
    estimation_proposed <- PSSE(data_labelled,data_unlabelled,type=type,tau=tau_0,alpha=polyOrder)
    hattheta_proposed <- estimation_proposed$Hattheta
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
  } 
  # compute MSE for all methods 
  MSE_supervised <-  sum((colMeans(results_supervised)-target_parameter)^2+apply(results_supervised,2,sd)^2)
  MSE_proposed <- sum((colMeans(results_proposed)-target_parameter)^2+apply(results_proposed,2,sd)^2) 
  MSE_PI <- sum((colMeans(results_PI)-target_parameter)^2+apply(results_PI,2,sd)^2) 
  MSE_EASE <- sum((colMeans(results_EASE)-target_parameter)^2+apply(results_EASE,2,sd)^2) 
  MSE_DRESS <- sum((colMeans(results_DRESS)-target_parameter)^2+apply(results_DRESS,2,sd)^2) 
  # save MSE for each replication 
  MSE_Different_n<- c(MSE_supervised,MSE_proposed,MSE_PI,MSE_EASE,MSE_DRESS)
  MSE_total<- rbind(MSE_total,MSE_Different_n)
}
####################(5) Figures ##############################################
### identify NaN columns (when "type!=linear", PI proposed by Azriel et al. (2021) and EASE proposed by Chakrabortty and Cai (2018) don't work)
colIndex<- which(!is.na(MSE_total[1,]))
maxMSE<- max(MSE_total[,colIndex])
if(type=="linear"){
  avaiableMethods<- c('Supervised','Proposed','PI','EASE','DRESS')
  scale_shape<- c(0,1,2,3,7)
  scale_colour<- c("red", "black", "blue","green","purple")
  scale_linetype<- c("dashed","dotted","solid","dotdash","longdash")
}else{
  avaiableMethods<- c('Supervised','Proposed','DRESS')
  scale_shape<- c(0,1,2)
  scale_colour<- c("red", "black", "blue")
  scale_linetype<- c("dashed","dotted","solid")
}
### subfigure 
dev.new()
pdf("subfigure_MSE.pdf")
u_0 <- rep(seq_n, times = length(colIndex))
Method <- rep(avaiableMethods,each = length(seq_n))
alpha_2<- as.vector(MSE_total[,colIndex])
df2 <- data.frame(u_0 = u_0, type = Method, alpha_2 = alpha_2)
alpha2_plot=ggplot(data = df2, mapping = aes(x =u_0, y = alpha_2, linetype = Method,color = Method)) + geom_line()+
  geom_point(aes(shape=Method))+
  scale_shape_manual(values=scale_shape)+
  ylim(NA, maxMSE*1.1)+
  scale_colour_manual(values = scale_colour)+
  scale_linetype_manual(values =scale_linetype)+
  labs(title=paste("p=",p,", N=",N,sep=""), x="n", y="MSE")
p2=alpha2_plot+ theme_bw() +theme(plot.title = element_text(hjust = 0)) +theme(aspect.ratio = 1)+
  theme(legend.position = c(0.74, 0.76))+
  theme(legend.background = element_rect(size=0.3, linetype="solid", 
                                         colour ="black"),legend.key.size = unit(0.15, 'cm'))+
  theme(plot.title = element_text(size=10))+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
p2
dev.off()