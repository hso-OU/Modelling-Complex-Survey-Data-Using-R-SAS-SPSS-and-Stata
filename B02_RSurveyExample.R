### R script 
## set the working directory to the source file in Rstudio 
setwd((dirname(rstudioapi::getActiveDocumentContext()$path)))
## import the CLSA dataset 
CLSAData <-  read.csv( file="CLSASmallExample.csv", header=TRUE, sep = ",")

#Specify the stratum variable 
CLSAData$StraVar<- with(CLSAData, GEOSTRAT_TRM)  


## Defining the orders of variables 
CLSAData$ENV_AFRDWLK_MCQ<-ordered( CLSAData$ENV_AFRDWLK_MCQ,  
   levels=c("Strongly_Agree","Agree","Disagree", "Strongly_Disagree"))  

CLSAData$Education<- factor(CLSAData$Education, levels=c("Low Education",
                    "Medium Education","Higher Education lower","Higher Education upper"))
CLSAData$Education <- relevel(CLSAData$Education, ref = 1)
CLSAData$WEA_MRTL_CURRENT<-factor(CLSAData$WEA_MRTL_CURRENT,
                    levels=c("Single", "Married", "Widowed", "Divorced", "Separated"))  
CLSAData$WEA_MRTL_CURRENT_NEW<- "Others"
CLSAData$WEA_MRTL_CURRENT_NEW[which(CLSAData$WEA_MRTL_CURRENT=="Married" )]<-"Married"
CLSAData$WEA_MRTL_CURRENT_NEW[which(CLSAData$WEA_MRTL_CURRENT=="Single" )]<-"Single"

## Defining some new variables 
CLSAData$HWT_DHT_M_TRM_sq<-CLSAData$HWT_DHT_M_TRM^2
CLSAData$BMI<- with(CLSAData, HWT_WGHT_KG_TRM/HWT_DHT_M_TRM^2) 
  
## Education: University indicator
CLSAData$ED_HIGH_TRM <- "Non_university" 
CLSAData$ED_HIGH_TRM[which(CLSAData$ED_UDR11_TRM %in% 8:10 )]<- "University" 


## Preparing for the survey package 
library (survey)
## specify how to treat strata with single observation in variance calculation
options(survey.lonely.psu = "certainty")
## Define the survey design  with inflation weights 
CLSA.design<- svydesign( ids= ~ entity_id,  
                         strata  = ~ StraVar, 
                         weights = ~ WGHTS_INFLATION_TRM,
                         data= CLSAData, nest =TRUE )
## Define the survey design  with analystic weights 
CLSA.design.anly<- svydesign( ids= ~ entity_id,  
                              strata  = ~ StraVar, 
                              weights = ~ WGHTS_ANALYTIC_TRM, data= CLSAData, nest =TRUE )

#Estimation of frequencies
(Table.Freq <- svytotal(  ~(ENV_AFRDWLK_MCQ), design= CLSA.design))  
#Estimation of population means
(Table.Mean<- svymean( ~ HWT_DHT_M_TRM+HWT_WGHT_KG_TRM , CLSA.design ))  
#Estimation of ratios of population means
(Table.ratio <- svyratio( numerator= ~HWT_WGHT_KG_TRM, denominator = ~ HWT_DHT_M_TRM_sq, design= CLSA.design ))
#Estimation of population quantiles
# Quant.Est<-svyquantile( ~ HWT_DHT_M_TRM+HWT_WGHT_KG_TRM , 
#                         quantile=c(0.025,0.05,0.1,0.5, 0.9,0.95, 0.975), 
#                         alpha=0.05, interval.type="Wald",design= CLSA.design,
#                         ties=c("rounded"), ci= TRUE, se=TRUE );
Quant.Est<-oldsvyquantile( ~ HWT_DHT_M_TRM+HWT_WGHT_KG_TRM ,
                        quantile=c(0.025,0.05,0.1,0.5, 0.9,0.95, 0.975),
                        alpha=0.05, interval.type="Wald",design= CLSA.design,
                        ties=c("rounded"), ci= TRUE, se=TRUE );
Quant.Est; SE(Quant.Est);
(Table.quantiles<- rbind(t(Quant.Est$quantiles), SE(Quant.Est)))

Quant.Est<-svyquantile( ~ HWT_DHT_M_TRM+HWT_WGHT_KG_TRM , 
                        quantile=c(0.025,0.05,0.1,0.5, 0.9,0.95, 0.975), 
                        alpha=0.05, interval.type="mean",qrule="hf4",
                        design= CLSA.design, 
                        ties=c("rounded"), ci= TRUE, se=TRUE );

(Table.quantiles<- cbind( c(Quant.Est$HWT_DHT_M_TRM [,c(1,4)] ),
                          c(Quant.Est$HWT_WGHT_KG_TRM[,c(1,4)]) ))
 

#Estimation of odds ratios, relative risks and risk differences
LogisticReg4.OR<-svyglm(ORH_EXP_DRM_MCQ ~  SEX_ASK_TRM ,  
                        family=quasibinomial(link="logit"), design=CLSA.design) 
exp(coef(LogisticReg4.OR)[2])      ## odds ratio
exp(confint(LogisticReg4.OR)[2,])  ## confidence interval


LogisticReg4.RR<-svyglm(ORH_EXP_DRM_MCQ ~   SEX_ASK_TRM ,  
                        family=quasibinomial(link="log"), design=CLSA.design) 
exp(coef(LogisticReg4.RR)[2])      ## relative risk   
exp(confint(LogisticReg4.RR)[2,])  ## confidence interval

LogisticReg4.RD<-svyglm(ORH_EXP_DRM_MCQ ~  SEX_ASK_TRM  ,  
                        family=quasibinomial(link="identity"), design=CLSA.design) 
coef(LogisticReg4.RD)[2]       ## risk difference 
confint(LogisticReg4.RD)[2,]   ## Confidence interval 

(Table.odds<- c(exp(coef(LogisticReg4.OR)[2]),exp(confint(LogisticReg4.OR)[2,])
               ,exp(coef(LogisticReg4.RR)[2]),exp(confint(LogisticReg4.RR)[2,])
               ,coef(LogisticReg4.RD)[2]     ,confint(LogisticReg4.RD)[2,] ))
    
#Estimation of covariance
Table.covar <- print(svyvar( ~ HWT_DHT_M_TRM+HWT_WGHT_KG_TRM , CLSA.design), covariance=TRUE)


###Analytic weights and estimation of model parameters

CLSA.design.anly<- svydesign( ids= ~ entity_id,  
                              strata  = ~ StraVar, 
                              weights = ~ WGHTS_ANALYTIC_TRM, 
                              data= CLSAData, nest =TRUE )


#Linear regression
LinearReg<-svyglm( HWT_DHT_M_TRM~ HWT_WGHT_KG_TRM+ SEX_ASK_TRM + 
                     Age_group_5+Education+WGHTS_PROV_TRM 
                   , family="gaussian", design=CLSA.design.anly)
summary(LinearReg)

Table.LinearReg<- summary(LinearReg)$coefficient[,1:2]

#Linear regression (Prediction)
Pred1 <- predict( LinearReg, newdata= CLSAData[1,], se.fit=TRUE)
Pred1; confint(Pred1) 
Table.LinearReg.Predict<- c(Pred1, SE(Pred1), confint(Pred1)  )

#Logistic regression analysis
LogitReg<-svyglm(ORH_EXP_DRM_MCQ~ SEX_ASK_TRM+Age_group_5+ Education+ 
                   WGHTS_PROV_TRM,  family=quasibinomial,design=CLSA.design.anly)
summary(LogitReg)
Table.LogitReg<-summary(LogitReg)$coefficient[,1:2]

#Logistic regression (Prediction)
Pred2<-predict(LogitReg,newdata= CLSAData[1,],type= "link",se.fit=TRUE)
plogis(Pred2[1]); plogis(confint(Pred2))
predict(LogitReg, newdata=CLSAData[1,], type="response", se.fit=TRUE) 


Table.Logiteg.Predict<- c(predict(LogitReg, newdata=CLSAData[1,], type="response", se.fit=TRUE) ,
                          SE(predict(LogitReg, newdata=CLSAData[1,], type="response", se.fit=TRUE)),
                          plogis(confint(Pred2)) ) 

# Adjusted odds ratios, relative risks and risk ratios
## Ordinal logistic regression in domain analysis
OrdinalReg<- summary(
  svyolr(formula= ENV_AFRDWLK_MCQ ~ SEX_ASK_TRM + Age_group_5 + Education+ 
           WGHTS_PROV_TRM, design = CLSA.design.anly,
         na.action = na.omit, method = c("logistic")) )

Table.OrdinalReg <- coefficients(OrdinalReg)[,c(1,2)]

## Adjusted odds ratio	(initial value found by SAS program)
Adj.OR <- svyglm( ORH_EXP_DRM_MCQ ~ SEX_ASK_TRM +Age_group_5+ Education+WGHTS_PROV_TRM,  
                  family=quasibinomial(link="logit"),design=CLSA.design.anly)
exp(coef(Adj.OR)["SEX_ASK_TRMM"]); exp(confint(Adj.OR)["SEX_ASK_TRMM",])
## Adjusted risk ratio
Adj.RR <- svyglm( ORH_EXP_DRM_MCQ ~ SEX_ASK_TRM  +Age_group_5+ Education+WGHTS_PROV_TRM,  
                  family=quasibinomial(link="log"),design=CLSA.design.anly,
                  start= c(-1.43,-.13,   0.05,0.06,-0.39,0.05,  -.49,-.38,-.34,
                            0.69,0.55,-.27,-.54,0.76,-.0006,0.95,0.55,0.48) )
exp(coef(Adj.RR)["SEX_ASK_TRMM"]); exp(confint(Adj.RR)["SEX_ASK_TRMM",])
## Adjusted risk difference 
Adj.RD <- svyglm( ORH_EXP_DRM_MCQ ~ SEX_ASK_TRM +Age_group_5+ Education+WGHTS_PROV_TRM,  
              family=quasibinomial(link="identity"),design=CLSA.design.anly,
              start= c( 0.14, -.02,  0.05,0.094,.0085,0.108,  -.063,-.083,-.043, 
                        0.172,0.181,0.026,-.033, 0.231,0.050,0.236,0.125, 0.125))
coef(Adj.RD)["SEX_ASK_TRMM"] ; confint(Adj.RD)["SEX_ASK_TRMM",]

Table.Adj.OR.RR.RD <- c( exp(coef(Adj.OR)["SEX_ASK_TRMM"]), exp(confint(Adj.OR)["SEX_ASK_TRMM",]),
                         exp(coef(Adj.RR)["SEX_ASK_TRMM"]), exp(confint(Adj.RR)["SEX_ASK_TRMM",]),
                             coef(Adj.RD)["SEX_ASK_TRMM"] ,     confint(Adj.RD)["SEX_ASK_TRMM",] ) 

#Domain Analysis
## Estimation of domain proportions
Temp1<- svymean(~ENV_AFRDWLK_MCQ, design=subset(CLSA.design,ED_HIGH_TRM=="Non_university") )
Temp2<- svymean(~ENV_AFRDWLK_MCQ, design=subset(CLSA.design,ED_HIGH_TRM=="University") )

Table.Domain.Mean<- cbind(c(Temp1,Temp2), c(SE(Temp1),SE(Temp2)) )       


# For people who did not attend an university 
Quant.NUni<- svyquantile( ~ HWT_DHT_M_TRM+HWT_WGHT_KG_TRM , 
                          quantile=c(0.025,0.05,0.1,0.5,0.9,0.95,0.975), alpha=0.05 ,interval.type="Wald", 
                          design= subset(CLSA.design,ED_HIGH_TRM=="Non_university"), 
                          ties=c("rounded"), ci= TRUE, se=TRUE )
Quant.NUni; SE(Quant.NUni); 
# For people who attended an university 
Quant.Uni<-svyquantile( ~ HWT_DHT_M_TRM+HWT_WGHT_KG_TRM , 
                        quantile=c(0.025,0.05,0.1,0.5,0.9,0.95,0.975), alpha=0.05 , interval.type="Wald", 
                        design= subset(CLSA.design,ED_HIGH_TRM=="University"), 
                        ties=c("rounded"), ci= TRUE, se=TRUE )
Quant.Uni; SE(Quant.Uni);

(Table.Domain.quantiles<- rbind( cbind(t(Quant.NUni$quantiles), t(Quant.Uni$quantiles) )[,c(1,3,2,4)] ,
                                 cbind(SE(Quant.NUni)         , SE(Quant.Uni))          [,c(1,3,2,4)] ))

 
##Linear regression in domain analysis
LinearReg_EN<-svyglm(HWT_DHT_M_TRM~HWT_WGHT_KG_TRM+SEX_ASK_TRM+ 
                       Age_group_5 + Education + WGHTS_PROV_TRM, 
                     family="gaussian", design=subset(CLSA.design.anly, startlanguage=="en" ) )
summary(LinearReg_EN)


Table.Domain.LinearReg<- coefficients(summary(LinearReg_EN))[,1:2]


## Logistic regression in domain analysis
LogisticReg3_EN<-svyglm(ORH_EXP_DRM_MCQ ~ SEX_ASK_TRM + Age_group_5 +Education + 
                          WGHTS_PROV_TRM,  family=quasibinomial, 
                        design=subset(CLSA.design.anly, startlanguage=="en"))
(Table.Domain.LogistReg<-  summary(LogisticReg3_EN))

## Ordinal logistic regression in domain analysis
OrdinalReg_EN<- summary(
  svyolr(formula= ENV_AFRDWLK_MCQ ~ SEX_ASK_TRM + Age_group_5 + Education+ 
               WGHTS_PROV_TRM, design = subset(CLSA.design.anly, startlanguage == "en"),
               na.action = na.omit, method = c("logistic")) )

(Table.Domain.OrdinalReg <- coefficients(OrdinalReg_EN)[,c(1,2)])

 
## Adjusted odds ratio, relative risks and risk ratios in domain analysis

## Adjusted odds ratio	
Domain.Adj.OR<-svyglm(ORH_EXP_DRM_MCQ~SEX_ASK_TRM+Age_group_5+Education+
                 WGHTS_PROV_TRM, family=quasibinomial(link="logit"), 
               design=subset(CLSA.design.anly, startlanguage == "en"))
exp(coef(Domain.Adj.OR)["SEX_ASK_TRMM"]); exp(confint(Domain.Adj.OR)["SEX_ASK_TRMM",])
## Adjusted risk ratio
Domain.Adj.RR<-svyglm(ORH_EXP_DRM_MCQ ~ SEX_ASK_TRM+Age_group_5+Education+
                 WGHTS_PROV_TRM, family=quasibinomial(link="log"),
               design=subset(CLSA.design.anly, startlanguage == "en"), 
               start= c(-1.02,0.03,   -0.25,-0.25,-0.51,-0.31,  -0.39,-0.33,-0.36,
                        -0.19,0.46,-0.62,-1.43,0.85,-0.26,0.16,-0.02,0.24))
exp(coef(Domain.Adj.RR)["SEX_ASK_TRMM"]); exp(confint(Domain.Adj.RR)["SEX_ASK_TRMM",])
## Adjusted risk difference 
Domain.Adj.RD<-svyglm( ORH_EXP_DRM_MCQ ~ SEX_ASK_TRM+Age_group_5+Education+
                  WGHTS_PROV_TRM, family=quasibinomial(link="identity"), 
                design=subset(CLSA.design.anly, startlanguage == "en"),  
start= c( 0.1562,-0.0008,   0.0049, 0.0627,0.0242,0.027,  -0.0275,-0.0305,
-0.0018, -0.0082, 0.2129,-0.0261,-0.1108,0.315,0.0068,0.0753,0.0498,0.1219))
coef(Domain.Adj.RD)["SEX_ASK_TRMM"] ; confint(Domain.Adj.RD)["SEX_ASK_TRMM",]

Table.Domain.OR.RD.RR <- c(exp(coef(Domain.Adj.OR)["SEX_ASK_TRMM"]), exp(confint(Domain.Adj.OR)["SEX_ASK_TRMM",]),
                           exp(coef(Domain.Adj.RR)["SEX_ASK_TRMM"]), exp(confint(Domain.Adj.RR)["SEX_ASK_TRMM",]),
                               coef(Domain.Adj.RD)["SEX_ASK_TRMM"],      confint(Domain.Adj.RD)["SEX_ASK_TRMM",]      ) 


## What if we ignore domain analysis
# It is correct to specify the subpopulations
svytotal(~ENV_AFRDWLK_MCQ, design=subset(CLSA.design,BMI<19) )
svytotal(~ENV_AFRDWLK_MCQ, design=subset(CLSA.design,BMI>=19))
 # It is not appropriate if we divide the dataset and re-declare the survey design 
  CLSAData.low.BMI <-CLSAData[which(CLSAData$BMI<19),]
 CLSAData.high.BMI<-CLSAData[which(CLSAData$BMI>=19),]
 CLSA.design.low.BMI<- svydesign( ids= ~ entity_id,  strata  = ~ StraVar, 
                                  weights = ~ WGHTS_INFLATION_TRM, data= CLSAData.low.BMI, nest =TRUE )
 
 CLSA.design.high.BMI<- svydesign( ids= ~ entity_id, strata  = ~ StraVar, 
                                   weights = ~ WGHTS_INFLATION_TRM, data= CLSAData.high.BMI, nest =TRUE )
 
 svytotal(~ENV_AFRDWLK_MCQ, design=CLSA.design.low.BMI)
 svytotal(~ENV_AFRDWLK_MCQ, design=CLSA.design.high.BMI)

Spec.low.BMI  <- svytotal(~ENV_AFRDWLK_MCQ, design=subset(CLSA.design,BMI<19) )
Spec.high.BMI <- svytotal(~ENV_AFRDWLK_MCQ, design=subset(CLSA.design,BMI>=19))
Non.low.BMI   <- svytotal(~ENV_AFRDWLK_MCQ, design=CLSA.design.low.BMI)
Non.high.BMI  <-  svytotal(~ENV_AFRDWLK_MCQ, design=CLSA.design.high.BMI)
 
 (Table.Domain.Ignore <- rbind( cbind( Spec.high.BMI,SE(Spec.high.BMI), Non.high.BMI ,SE(Non.high.BMI)),
                               cbind( Spec.low.BMI  ,SE(Spec.low.BMI) , Non.low.BMI  ,SE(Non.low.BMI))) )


## save the tables and estimates as R.Data

save( CLSA.design, CLSA.design.anly, Table.Freq, Table.Mean, Table.ratio,
      Quant.Est, Table.quantiles, Table.odds, Table.covar, 
      Table.LinearReg, Table.LinearReg.Predict, Table.LogitReg,
      Table.Logiteg.Predict, Table.OrdinalReg,Table.Adj.OR.RR.RD,
      Table.Domain.Mean, Table.Domain.quantiles,Table.Domain.LinearReg,
      Table.Domain.LogistReg, Table.Domain.OrdinalReg, 
      Table.Domain.OR.RD.RR,  Table.Domain.Ignore, file=
        "EstimationResults/R/RResults.RData"
  )

