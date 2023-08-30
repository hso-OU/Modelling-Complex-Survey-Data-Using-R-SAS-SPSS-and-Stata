###Simulation for the CLSA Dataset 
#Basic survey variables 
###               ID: entity_id
###          Stratum: GEOSTAT_TRM 
### Inflation weight: WGHTS_INFLATION_TRM
###  Analytic weight: WGHTS_ANALYTIC_TRM
###Stratum variables: WGHTS_PROV_TRM1 
###Stratum variables: SEX_ASK_TRM 
###Stratum variables: Age_Grp
###Stratum variables: Education
###


#Estimation of population frequencies 
###        Response: ENV_AFRDWLK_MCQ

#Estimation of 1)the population means, 
#              2)the population ratios of means, and
#              3)the population quantiles
###        Response: HWT_DHT_M_TRM
###        Response: HWT_WGHT_KG_TRM

# Estimation of population odds ratios, relative risks and risk differences
###        Response: ORH_EXP_DRM_MCQ
###       Covariate: SEX_ASK_TRM

# Estimation of population covariance 
###        Response: HWT_DHT_M_TRM
###        Response: HWT_WGHT_KG_TRM


# Linear regression analysis
###        Response: HWT_DHT_M_TRM
###       Covariate: HWT_WGHT_KG_TRM
###       Covariate: WGHTS_PROV_TRM1 
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education


# Logistic regression analysis
###        Response: ORH_EXP_DRM_MCQ 
###       Covariate: WGHTS_PROV_TRM1 
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education


# Multinomial logistic regression analysis
###        Response: WEA_MRTL_CURRENT_MCQ2
###       Covariate: WGHTS_PROV_TRM1 
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education



# Ordinal logistic regression analysis
###        Response: ENV_AFRDWLK_MCQ2
###       Covariate: WGHTS_PROV_TRM1 
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education

# Adjusted odds ratios, relative risks and risk ratios
###        Response: ORH_EXP_DRM_MCQ
###       Covariate: WGHTS_PROV_TRM1 
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education

# Domain estimation
## Estimation of domain proportions
###          Domain:  ED_HIGH_TRM
###        Response: ENV_AFRDWLK_MCQ

## Estimation of domain quantiles
###          Domain:  ED_HIGH_TRM
###        Response: HWT_DHT_M_TRM
###        Response: HWT_WGHT_KG_TRM

## Linear regression in domain analysis
###          Domain: startlanguage;
###        Response: HWT_DHT_M_TRM
###       Covariate: WGHTS_PROV_TRM
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education

## Logistic regression in domain analysis
###          Domain: startlanguage;
###        Response: ORH_EXP_DRM_MCQ
###       Covariate: WGHTS_PROV_TRM
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education

## Multinomial logistic regression in domain analysis
###          Domain: startlanguage;
###        Response: WEA_MRTL_CURRENT_MCQ
###       Covariate: WGHTS_PROV_TRM
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education

## Ordinal logistic regression in domain analysis
###          Domain: startlanguage;
###        Response: ENV_AFRDWLK_MCQ
###       Covariate: WGHTS_PROV_TRM
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education

## Adjusted odds ratio, relative risks and risk ratios in domain analysis
###          Domain: startlanguage;
###        Response: ORH_EXP_DRM_MCQ
###       Covariate: WGHTS_PROV_TRM
###       Covariate: SEX_ASK_TRM 
###       Covariate: Age_Grp
###       Covariate: Education

## Details of the variables 
###               ID: entity_id
###          Stratum: GEOSTAT_TRM 
### Inflation weight: WGHTS_INFLATION_TRM 
###  Analytic weight: WGHTS_ANALYTIC_TRM
###Stratum variables: WGHTS_PROV_TRM1 
###Stratum variables: SEX_ASK_TRM 
###Stratum variables: Age_Grp
###Stratum variables: Education

##Constructing the strata 
Prov.vec   <- c( "AB", "BC", "MB","NB","NL","NS","ON","PE","QC","SK")
Prov.DCS   <- c( "AB", "BC", "MB",     "NL","NS","ON",     "QC") 
Prov.NonDCS<- c( "NB", "PE", "SK")     

Prov.prop  <- c(71000,79000,32000,14000,11000,16000,82000,2800,2e+05,12000)

Age.grp    <- c("45-54","55-64","65-74","75-85")
Sex.grp    <- c("female","male")

## Education   1:11
ED_UDR11.prop<- c( 8000, 17000, 12000, 66000,30000,58000,11000,
                  40000,100000,75000,7000)
## SEX  Female Male
Sex_Ask.prop<- c(F=306000, M=212000)

## non_DCS  FASLE TRUE
DCS.prop<- c( DCS=250000, non_DCS=270000 )

## Age  45:85 as mixture of two normal  
Age.gen.func <- function(n=1){ 
  temp.vec <- rbinom(n,size=1,prob=0.27)+1
  temp.vec <- rnorm(n, mean= c(61,65)[temp.vec],sd=c(7.7,2.7)[temp.vec])
  temp.vec <- round(temp.vec,0)
  out.vec  <- which(! temp.vec%in% c(45:85))
  out.n    <- length(out.vec)
  if (out.n>0) temp.vec[out.vec] <- Age.gen.func( n= out.n)
  temp.vec
}


#Response 
## startlanguage  en fr 
startlanguage.prop<- c(en=320000,fr=200000)

## Experience dry mouth 
ORH_EXP_DRM.Prop<-rbind( No=c(F=230000,M=170000),
                        Yes=c(F= 80000,M= 40000))
## Afraid to walk alone after dark in local area
ENV_AFRDWLK.Prop<-rbind( Strongly_Agree = c(F=  6900,M=  1700),
                                  Agree = c(F= 31000,M= 15000), 
                               Disagree = c(F=170000,M=130000), 
                      Strongly_Disagree = c(F= 76000,M= 62000))
#For age group c("45-48","49-54","55-64","65-74","75-85") 
ENV_AFRDWLK.Age.Prop<-rbind( Strongly_Agree = c(  0 ,  0,   0 ,500, 500),
                                      Agree = c(  0 ,  0, 500 ,500,   0), 
                                   Disagree = c(  0 ,500, 500 ,  0,   0), 
                          Strongly_Disagree = c(500 ,500,   0 ,  0,   0))

## Current marital status 
WEA_MRTL_CURRENT.Prop<-rbind( Single  = c(F=  33000, M= 20000), 
                              Married = c(F= 180000, M=170000),
                              Widowed = c(F=  37000, M=  6000),
                             Divorced = c(F=  38000, M=  8300),
                            Seperated = c(F=   5000, M=  6200))

## Generating function based on SEX
Gen.Sex.func<- function( Prob.Mat, Sex.vec ){
  Tags<- rownames(Prob.Mat)
  No.Tags<- length(Tags)
  ans<-sapply(Sex.vec, function(x)sum(1:No.Tags*rmultinom(1,1,prob=Prob.Mat[,x])))
  Tags[ans]
}

Gen.Sex.Age.func<- function( Prob.Mat, Sex.vec, Prob.Age.Mat, Age.vec ){
     Tags <- rownames(Prob.Mat)
  No.Tags <- length(Tags)
      Len <- length(Sex.vec)
  Age.Grp.Num<- 1 + (Age.vec>=49) + (Age.vec>=55) + (Age.vec>=65)+ (Age.vec>=75)
   #age group=c("45-48","49-54","55-64","65-74","75-85")
  ans<-sapply(1:Len, function(x)sum(1:No.Tags*rmultinom(1,1,prob=Prob.Mat[,Sex.vec[x]]+Prob.Age.Mat[,Age.Grp.Num[x]]) ) )
  Tags[ans]
}

## Height 
HWT_DHT_M_TRM.func<-function( sex.vec ){
  sex.vec1 <- (sex.vec =="M") + 1 ; n<-length(sex.vec)
  temp.vec <- rbinom(n,size=1,prob=c(0.97,0.13)[sex.vec1])+1
  temp.vec <- rnorm(n, mean= c(1.76,1.63,1.73,1.83)[temp.vec+2*sex.vec1-2]
                        ,sd= c(0.10,0.06,0.05,0.05)[temp.vec+2*sex.vec1-2])
  temp.vec 
}
#####HWT_DHT_M_TRM.func(c("F","M","M"))
## Weight 
HWT_DWT_K_TRM.func<- function(height,sex.vec){ 
  rnorm(length(height), mean= -66.66+84.55*height+3.21*(sex.vec=="M"), sd=14.3 )
}

####HWT_DWT_K_TRM.func( HWT_DHT_M_TRM.func(c("F","M","M")), c("F","M","M"))



##Generating a (pseudo) population (520000)
set.seed(1234)
#N<-520000
N <- 14e6  #13655060
Ppl.Data<- data.frame(
 entity_id = 1:N
,WGHTS_PROV_TRM   = Prov.vec[ colSums(1:10*rmultinom(N,size=1,prob =Prov.prop)) ]
,SEX_ASK_TRM =c("F","M")[rbinom(n=N, size=1, prob= Sex_Ask.prop[2]/sum(Sex_Ask.prop))+1]
,AGE_NMBR_TRM= Age.gen.func(n=N) 
,DCS.groups  =c("Non_DCS","DCS1","DCS2")[rbinom(n=N, size=1, prob= DCS.prop[1]/sum(DCS.prop))*
                                        (rbinom(n=N, size=1, prob= 0.5)+1 ) +1]  ## Simulation of  DCS1 or DCS2  for BC ON and QC 
,ED_UDR11_TRM      = colSums(1:11*rmultinom(N,size=1,prob =ED_UDR11.prop) )
,startlanguage_MCQ = c("en","fr")[rbinom(n=N, size=1, prob=startlanguage.prop[2]/sum(startlanguage.prop))+1]
) 

## Change DCS to Non_DCS for Non-DCS province
Ppl.Data$DCS.groups[which(Ppl.Data$WGHTS_PROV_TRM %in% Prov.NonDCS)]<-"Non_DCS"

## Combining  DCS1 and DCS2 in  AB MB NS NL
Ppl.Data$DCS.groups[with(Ppl.Data, which(WGHTS_PROV_TRM %in% c("AB","MB","NS","NL") & DCS.groups !="Non_DCS"))]<- "DCS1"

## Removing  DCS1 and DCS2
Ppl.Data$DCS.vec<- as.character(Ppl.Data$DCS.groups)
Ppl.Data$DCS.vec[which(Ppl.Data$DCS.vec !="Non_DCS")] <- "DCS"


Ppl.Data$ENV_AFRDWLK_MCQ = with(Ppl.Data, Gen.Sex.Age.func(ENV_AFRDWLK.Age.Prop,SEX_ASK_TRM, ENV_AFRDWLK.Age.Prop,AGE_NMBR_TRM))
Ppl.Data$ORH_EXP_DRM_MCQ = Gen.Sex.func(ORH_EXP_DRM.Prop     , Ppl.Data$SEX_ASK_TRM) 
Ppl.Data$WEA_MRTL_CURRENT= Gen.Sex.func(WEA_MRTL_CURRENT.Prop, Ppl.Data$SEX_ASK_TRM)  
Ppl.Data$HWT_DHT_M_TRM   = HWT_DHT_M_TRM.func(Ppl.Data$SEX_ASK_TRM)
Ppl.Data$HWT_DWT_K_TRM   = HWT_DWT_K_TRM.func(Ppl.Data$HWT_DHT_M_TRM,
                                              Ppl.Data$SEX_ASK_TRM)

## Education 
temp.vec <- c("Low Education" , "Medium Education", 
              "Higher Education lower" , "Higher Education upper"  ) 
temp.vec2 <- c(1,1,1,2,2,2,3,3,4,4,2)
Ppl.Data$Education =  temp.vec[temp.vec2[Ppl.Data$ED_UDR11_TRM] ] 

Ppl.Data$Sample_Age_Gpr<-  Age.grp[ with(Ppl.Data, (45<=AGE_NMBR_TRM)+
       (55<=AGE_NMBR_TRM)+(65<=AGE_NMBR_TRM)+ (75<=AGE_NMBR_TRM) )]

Ppl.Data$WGHTS_GEOSTRAT_TRM<- with(Ppl.Data, paste(WGHTS_PROV_TRM,DCS.vec,sep="_") )

Ppl.Data$Sampling_Strata<- with(Ppl.Data, 
          paste(WGHTS_PROV_TRM, DCS.vec, SEX_ASK_TRM
                              , Sample_Age_Gpr,  sep="_") )

########Sample Size##########
#table(Ppl.Data$Sampling_Strata)

Sampling.mat<-expand.grid(Prov=Prov.vec, DCS.vec  =c("DCS","Non_DCS")
                         ,SEX =c("F","M"), Age.grp = Age.grp )  
Sampling.mat<-Sampling.mat[-which(Sampling.mat$Prov%in% Prov.NonDCS 
                                  & Sampling.mat$DCS.vec=="DCS"),]
rownames(Sampling.mat)<-1:136
Prov.Size   <- c(  AB= 90 , BC=100,MB=90,NB=60,NL=60,NS=60,ON=85,PE=55,QC=200,SK=50)
DCS.Size    <- c( DCS= 420, NON.DCS = 280)/700
#Sex.Size <- c(   F= 510,       M = 350)/860
Sex.Size <- c(   F= 500,       M = 490)/990

Sampling.mat$Sample_size<- round( with(Sampling.mat,Prov.Size[Prov]*
       Sex.Size[SEX] * DCS.Size[DCS.vec]^(1-Prov%in% Prov.NonDCS)  ) )
Sampling.mat$Sampling_Strata<-  with(Sampling.mat, 
                                     paste(Prov, DCS.vec, SEX, Age.grp,  sep="_") )
temp.vec <- table(Ppl.Data$Sampling_Strata)
Sampling.mat<-merge(Sampling.mat
                 , data.frame(Sampling_Strata=names(temp.vec ), N_h=c(temp.vec) )
                 , by="Sampling_Strata" ) 
library(dplyr)
## using the actual number of CLSA participants
CLSAParticipant <- read.csv("CLSAParticipant.csv")
### Removing Space
CLSAParticipant$Prov    <- as.factor( gsub(" ", "",as.character(CLSAParticipant$Prov) , fixed = TRUE) )
CLSAParticipant$Age.grp <- as.factor( gsub(" ", "",as.character(CLSAParticipant$Age.grp) , fixed = TRUE) )
CLSAParticipant$Sampling_Strata<-  with(CLSAParticipant,
                                        paste(Prov, DCS.vec, SEX, Age.grp,  sep="_") )

Sampling.mat$Sample_size<-NULL
Sampling.mat <- merge(Sampling.mat
                      , CLSAParticipant[,c("Sampling_Strata","Sample_size")]
                      , by="Sampling_Strata" )


## sample sizes 
Sampling.mat$n_h <- Sampling.mat$Sample_size
## 
Sampling.mat$GEOSTRAT_TRM <- with(Sampling.mat, paste( Prov, DCS.vec,sep="_") ) 

## basic design weight, initial weight
Sampling.mat$basic_weight <- Sampling.mat$N_h /Sampling.mat$n_h 
head(Sampling.mat)



Ppl.Data1<-merge(x=Ppl.Data ,
    y=Sampling.mat[,c("Sampling_Strata","Sample_size", "N_h", "n_h", "basic_weight","GEOSTRAT_TRM")],
    by= "Sampling_Strata")

##################################################
set.seed(2345)
Sample.Data <- Ppl.Data1 %>% group_by(Sampling_Strata) %>% sample_n(Sample_size)

Sample.Data$Age_group_5<- c("45-48","49-54","55-64","65-74","75-85")[with( Sample.Data, 
  1 + (AGE_NMBR_TRM>=49) +  (AGE_NMBR_TRM>=55) + (AGE_NMBR_TRM>=65) +(AGE_NMBR_TRM>=75) )]

##Page 15 of datasuppdoc_sampwgts_bl_v1.2_2020nov10.pdf

### Rescaled Initial Weights (RIL):
#First, within each frame sample, and then within each province crossed with sex crossed with age decade, we rescaled the initial weights to sum to sample size, 
#### RIL strata
Sample.Data$RIL_Strata  <- with(Sample.Data, 
                                paste( WGHTS_PROV_TRM, SEX_ASK_TRM, Sample_Age_Gpr ,sep="_"))

#### total number of participants in each RTL stratum
Stratum.total.RIL<- Sample.Data %>% group_by(RIL_Strata) %>%   summarise(RIL_np = n())
Sample.Data <- left_join(Sample.Data ,Stratum.total.RIL, by= c("RIL_Strata") )

#### total basic (design) weight in each RTL stratum
Stratum.total.basic.weight.RIL   <- Sample.Data %>% group_by(RIL_Strata) %>% 
                                     summarise(RIL_total_basic_weights = sum(basic_weight))
Sample.Data <- left_join(Sample.Data ,Stratum.total.basic.weight.RIL, by= c("RIL_Strata"))

Sample.Data$RIL <- with(Sample.Data, basic_weight*RIL_np/RIL_total_basic_weights )



### CIN1 weights 
NHS.Weight <- read.csv("NHS2011Weight.csv")

NHS.CIN1        <- NHS.Weight  %>% group_by(WGHTS_PROV_TRM,DCS.groups,Education) %>% summarise(sum_of_NHS_weights = sum(NHS_Weight))
Sum_RIL_prDCSed <- Sample.Data %>% group_by(WGHTS_PROV_TRM,DCS.groups,Education) %>% summarise(  sum_RIL_prDCSed  = sum(RIL))
Sample.Data <- left_join(Sample.Data ,Sum_RIL_prDCSed, by= c("WGHTS_PROV_TRM","DCS.groups","Education") )
Sample.Data <- left_join(Sample.Data ,NHS.CIN1       , by= c("WGHTS_PROV_TRM","DCS.groups","Education") )

Sample.Data$CIN1 <- with(Sample.Data,  sum_of_NHS_weights*RIL/ sum_RIL_prDCSed   )

## ## Trimming the outliner (instead of merging cells in the sample weight document) 
Sample.Data$CIN1_modified <- Sample.Data$CIN1
#Sample.Data$CIN1_modified[which(Sample.Data$CIN1>4e4)]<- 4e4
Sample.Data$CIN1_modified[which(Sample.Data$CIN1>6e3)]<- 6e3

### CIN2 weights 
NHS.CIN2          <- NHS.Weight  %>% group_by(WGHTS_PROV_TRM, SEX_ASK_TRM,Age_group_5 ) %>% summarise(sum_of_NHS_weights_II = sum(NHS_Weight))
Sum_CIN1_prSEXage <- Sample.Data %>% group_by(WGHTS_PROV_TRM, SEX_ASK_TRM,Age_group_5 ) %>% summarise(sum_CIN1_prSEXage     = sum(CIN1_modified))
Sample.Data <- left_join(Sample.Data ,Sum_CIN1_prSEXage, by= c("WGHTS_PROV_TRM", "SEX_ASK_TRM", "Age_group_5") )
Sample.Data <- left_join(Sample.Data ,NHS.CIN2         , by= c("WGHTS_PROV_TRM", "SEX_ASK_TRM", "Age_group_5") )

Sample.Data$CIN2 <- with(Sample.Data,  sum_of_NHS_weights_II*CIN1_modified/ sum_CIN1_prSEXage )
## ## Trimming the outliner  
Sample.Data$CIN2_modified <- Sample.Data$CIN2
# Sample.Data$CIN2_modified[which(Sample.Data$CIN2>4e4)]<- 4e4
Sample.Data$CIN2_modified[which(Sample.Data$CIN2>6e3)]<- 6e3

### CIN3 weights 
NHS.CIN3                      <- NHS.Weight  %>% group_by(WGHTS_PROV_TRM, SEX_ASK_TRM,DCS.groups) %>% 
                                                summarise(sum_of_NHS_weights_III = sum(NHS_Weight))
Sum_of_CIN2_modified_prSEXdcs <- Sample.Data %>% group_by(WGHTS_PROV_TRM, SEX_ASK_TRM,DCS.groups ) %>%
                                                summarise(sum_of_CIN2_modified_prSEXdcs= sum(CIN2_modified))
Sample.Data <- left_join(Sample.Data ,Sum_of_CIN2_modified_prSEXdcs, by= c("WGHTS_PROV_TRM", "SEX_ASK_TRM", "DCS.groups"))
Sample.Data <- left_join(Sample.Data ,NHS.CIN3                     , by= c("WGHTS_PROV_TRM", "SEX_ASK_TRM", "DCS.groups"))

Sample.Data$CIN3 <- with(Sample.Data,  sum_of_NHS_weights_III*CIN2_modified/ sum_of_CIN2_modified_prSEXdcs )

#### no need to trim the small outliner
Sample.Data$CIN3_altered <- Sample.Data$CIN3

##CIN3_altered_calib_to_CIN1 weights


Sum_of_CIN3_altered_prDCSed  <- Sample.Data %>% group_by(WGHTS_PROV_TRM,DCS.groups,Education) %>% 
                                              summarise(sum_of_CIN3_altered_prDCSed  = sum(CIN3_altered))
Sample.Data <- left_join(Sample.Data, Sum_of_CIN3_altered_prDCSed , by= c("WGHTS_PROV_TRM","DCS.groups","Education") )

Sample.Data$CIN3_altered_calib_to_CIN1 <-  with(Sample.Data, sum_of_NHS_weights*CIN3_altered /sum_of_CIN3_altered_prDCSed) 

#CIN2_II weights:

#NHS.CIN2          <- NHS.Weight  %>% group_by(WGHTS_PROV_TRM, SEX_ASK_TRM,Age_group_5 ) %>% summarise(sum_of_NHS_weights_II = sum(NHS_Weight))
Sum_of_CIN3_altered_calib_to_CIN1_prSEXage <- Sample.Data %>% group_by(WGHTS_PROV_TRM, SEX_ASK_TRM,Age_group_5 ) %>% 
                                summarise(sum_of_CIN3_altered_calib_to_CIN1_prSEXage = sum(CIN3_altered_calib_to_CIN1))
Sample.Data <- left_join(Sample.Data ,Sum_of_CIN3_altered_calib_to_CIN1_prSEXage, by= c("WGHTS_PROV_TRM", "SEX_ASK_TRM", "Age_group_5") )

Sample.Data$CIN2_II <-  with(Sample.Data,sum_of_NHS_weights_II*CIN3_altered_calib_to_CIN1/ sum_of_CIN3_altered_calib_to_CIN1_prSEXage ) 

# trimmed weight 
Sample.Data$CIN2_II_trimmed <- Sample.Data$CIN2_II
# Sample.Data$CIN2_II_trimmed[which(Sample.Data$CIN2_II_trimmed> 4e4)]<- 4e4 
Sample.Data$CIN2_II_trimmed[which(Sample.Data$CIN2_II_trimmed> 6e3)]<- 6e3 

# Then CIN2_II_trimmed weights were re-calibrated within province by sex crossed with 5 age groups:
Sum_of_CIN2_II_trimmed <- Sample.Data %>% group_by(WGHTS_PROV_TRM, SEX_ASK_TRM,Age_group_5 ) %>% 
                                 summarise(sum_of_CIN2_II_trimmed = sum(CIN2_II_trimmed))
Sample.Data <- left_join(Sample.Data ,Sum_of_CIN2_II_trimmed , by= c("WGHTS_PROV_TRM", "SEX_ASK_TRM", "Age_group_5") )
Sample.Data$CIN2_II_trimmed_calib_to_CIN2 <-  with(Sample.Data, sum_of_NHS_weights_II*CIN2_II_trimmed/sum_of_CIN2_II_trimmed)


Sample.Data$WGHTS_INFLATION_TRM<- Sample.Data$CIN2_II_trimmed_calib_to_CIN2

Sum_of_WGHTS_INFLATION_TRM <- Sample.Data %>% group_by(WGHTS_PROV_TRM ) %>% 
                                 summarise(sum_of_WGHTS_INFLATION_TRM = sum(WGHTS_INFLATION_TRM))

Total_num_Prov <- Sample.Data %>% group_by(WGHTS_PROV_TRM ) %>% 
                  summarise(Total_num_Prov = n())

Sample.Data <- left_join(Sample.Data ,Sum_of_WGHTS_INFLATION_TRM , by= c("WGHTS_PROV_TRM") )
Sample.Data <- left_join(Sample.Data ,Total_num_Prov             , by= c("WGHTS_PROV_TRM") )


Sample.Data$WGHTS_ANALYTIC_TRM <- with(Sample.Data, Total_num_Prov*WGHTS_INFLATION_TRM/ sum_of_WGHTS_INFLATION_TRM )

#Sample.Data %>% group_by(WGHTS_PROV_TRM ) %>% summarise(avg_analytic_wght = mean(WGHTS_ANALYTIC_TRM))
## convert prov variable back to numeric format 
# Sample.Data$WGHTS_PROV_TRM <-
# sapply( as.character(Sample.Data$WGHTS_PROV_TRM), function(x)which( c("AB","BC","MB","NB","NL","NS","ON","PE","QC","SK") == x))

Sample.Data$HWT_WGHT_KG_TRM<- Sample.Data$HWT_DWT_K_TRM
Sample.Data$startlanguage<-Sample.Data$startlanguage_MCQ

write.csv(Sample.Data[,c( "entity_id"      , "WGHTS_PROV_TRM"   , "SEX_ASK_TRM"    , "AGE_NMBR_TRM" ,
                          "ED_UDR11_TRM"   , "ENV_AFRDWLK_MCQ"  , "ORH_EXP_DRM_MCQ", "HWT_DHT_M_TRM", 
                          "HWT_WGHT_KG_TRM", "startlanguage",   "Education"    , "GEOSTRAT_TRM" , 
                          "Age_group_5"    , "WGHTS_INFLATION_TRM"  , "WGHTS_ANALYTIC_TRM", "WEA_MRTL_CURRENT" ) ]
          , file="CLSARealExample.csv", row.names=FALSE)

set.seed(3456)
###sampling a smaller dataset for illustration
write.csv(Sample.Data[sample(1: dim(Sample.Data)[1], 800)
                      ,c( "entity_id"      , "WGHTS_PROV_TRM"   , "SEX_ASK_TRM"    , "AGE_NMBR_TRM" ,
                          "ED_UDR11_TRM"   , "ENV_AFRDWLK_MCQ"  , "ORH_EXP_DRM_MCQ", "HWT_DHT_M_TRM", 
                          "HWT_WGHT_KG_TRM", "startlanguage",   "Education"    , "GEOSTRAT_TRM" , 
                          "Age_group_5"    , "WGHTS_INFLATION_TRM"  , "WGHTS_ANALYTIC_TRM", "WEA_MRTL_CURRENT" ) ]
          , file="CLSASmallExample.csv", row.names=FALSE)



################################
################################
################################
################################
# CLSAExample <- read.csv("D:/ubuntu/CLSAExample/CLSARealExample.csv", header=TRUE)
# library (survey)
# options(survey.lonely.psu = "certainty")
# 
# CLSAExample$Age_Gpr<-  as.factor( with( CLSAExample, (45<=AGE_NMBR_TRM)+
#                                   (55<=AGE_NMBR_TRM)+(65<=AGE_NMBR_TRM)+ (75<=AGE_NMBR_TRM) ) )
# 
# CLSAExample$StraVar<- with(CLSAExample, WGHTS_GEOSTRAT_TRM)  
# 
# Province.uniq<-c( "Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia",  "Ontario", "Prince Edward Island", "Quebec","Saskatchewan")
# 
# CLSAExample$WGHTS_PROV_TRM1<-Province.uniq[CLSAExample$WGHTS_PROV_TRM]
# temp.vec <- c("1:Low Education" , "2:Medium Education", 
#               "3:Higher Education lower" , "4:Higher Education upper"  ) 
# temp.vec2 <- c(1,1,1,2,2,2,3,3,4,4,2)
# CLSAExample$Education <- temp.vec[temp.vec2[CLSAExample$ED_UDR11_TRM] ] 
# 
# CLSAExample$GEOSTAT_TRM<- CLSAExample$WGHTS_GEOSTRAT_TRM
# CLSAExample$nonDCS<-stringr::str_detect(CLSAExample$GEOSTAT_TRM, "_Non_DCS_")
# CLSAExample$HWT_DWT_K_TRM[which(CLSAExample$HWT_DWT_K_TRM>900)]<-NA
# 
# ## recombine the lowEd and not_LowEd
# sum(CLSAExample$WGHTS_TRIMMED_TRM)
# ## 520000
#  
# 
# table(CLSAExample$Education)
# 
# table(CLSAExample$WGHTS_PROV_TRM1)
# table(CLSAExample$nonDCS)     # NB= 60, PE=56, SK =50 # others: DCS:  278 // Non-DCS: 420
# table(CLSAExample$SEX_ASK_TRM)
# table(CLSAExample$Age_Gpr)  ### including province and DCS 
# 
# ## target sample 
# 
# 
# head(CLSAExample$WGHTS_ANALYTIC_TRM)
# head(CLSAExample$WGHTS_TRIMMED_TRM)
# 
# CLSA.design<- svydesign( ids= ~ entity_id,  strata  = ~ StraVar, 
#                          weights = ~ WGHTS_TRIMMED_TRM, 
#                          data= CLSAExample, nest =TRUE )
# ## called INFLATION_TRM
# 
# ## the marginal table for constructing the population
# svytable(~WGHTS_PROV_TRM,design= CLSA.design)
# svytable( ~AGE_NMBR_TRM, design= CLSA.design)
# svytable( ~SEX_ASK_TRM,  design= CLSA.design)
# svytable( ~GEOSTAT_TRM,  design= CLSA.design)
# svytable( ~ED_UDR11_TRM, design= CLSA.design)
# svytable( ~nonDCS,       design= CLSA.design)
# svytable( ~startlanguage_MCQ,design= CLSA.design)
# 
# 
# 
# ## response 
# signif( svytable( ~ORH_EXP_DRM_MCQ+SEX_ASK_TRM,design= CLSA.design), 2) 
# signif( svytable( ~ENV_AFRDWLK_MCQ+SEX_ASK_TRM,design= CLSA.design), 2)
# signif( svytable( ~WEA_MRTL_CURRENT_MCQ+SEX_ASK_TRM,design= CLSA.design), 2)
# 
# 
# 
# pseudo.table<-svytable( ~AGE_NMBR_TRM+SEX_ASK_TRM,  design= CLSA.design)
# 
# paste0( signif(svytable( ~ED_UDR11_TRM,  design= CLSA.design),2), collapse = ",")
# paste0( signif( svytable(~WGHTS_PROV_TRM,design= CLSA.design),2), collapse = ",")
# 
#  
#  
# age.temp<-svytable( ~AGE_NMBR_TRM, design= CLSA.design)
# age.temp<- rep(45:85, age.temp)
# mixmdl = mixtools::normalmixEM(age.temp)
# plot(mixmdl,which=2)
# lines(density(age.temp), lty=2, lwd=2)       
# mixmdl[c("lambda","mu","sigma")]
# 
# temp<- svytable( ~HWT_DHT_M_TRM+SEX_ASK_TRM,design= CLSA.design) 
# 
# H.F.temp<- rep(as.numeric(rownames(temp)), temp[,"F"])
# H.M.temp<- rep(as.numeric(rownames(temp)), temp[,"M"])
# 
# mixHF = mixtools::normalmixEM(H.F.temp)
# plot(mixHF,which=2)
# lines(density(H.F.temp), lty=2, lwd=2)       
# mixHF[c("lambda","mu","sigma")]
# 
# mixHM = mixtools::normalmixEM(H.M.temp)
# plot(mixHM,which=2)
# lines(density(H.M.temp), lty=2, lwd=2)       
# mixHM[c("lambda","mu","sigma")]
# 
# 
# ##weight 
# svytable(~HWT_DWT_K_TRM+SEX_ASK_TRM,design= CLSA.design) 
# summary(temp<- svyglm( HWT_DWT_K_TRM~HWT_DHT_M_TRM+SEX_ASK_TRM,design= CLSA.design) )
# sd(resid(temp))
# 
# 
# 
