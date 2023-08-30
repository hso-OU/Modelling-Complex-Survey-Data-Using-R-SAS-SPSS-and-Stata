  LIBNAME FileIN '/home/honyius/sasuser.v94' ACCESS=readonly;   
  LIBNAME FileOUT '/home/honyius/sasuser.v94' ;
  
Proc import out=FileOUT.CLSAExample
datafile="/home/honyius/sasuser.v94/CLSASmallExample.csv"
dbms=dlm replace ;
  delimiter=",";
GETNAMES=YES;
DATAROW=2;
guessingrows=max ;    
run;
   
PROC Format; 
VALUE   $agree  "Strongly_Agree" = "1:Strongly_Agree"  
                         "Agree" = "2:Agree" 
                      "Disagree" = "3:Disagree" 
             "Strongly_Disagree" = "4:Strongly_Disagree";
RUN;
 
DATA CLSADATA ;
set  FileOUT.CLSAExample;
StraVar = GEOSTRAT_TRM;
HWT_DHT_M_TRM_sq = HWT_DHT_M_TRM**2;
BMI =  HWT_WGHT_KG_TRM/HWT_DHT_M_TRM_sq;
ED_HIGH_TRM = "Non_university" ;
if( 8<= ED_UDR11_TRM <= 10) then  ED_HIGH_TRM = "University" ;
WEA_MRTL_CURRENT_New= "Others " ;
WEA_MRTL_CURRENT_New= "Others" ;
if(WEA_MRTL_CURRENT = "Single" ) then WEA_MRTL_CURRENT_New = "Single ";
if(WEA_MRTL_CURRENT = "Married") then WEA_MRTL_CURRENT_New = "Married";
ENV_AFRDWLK_MCQ  = put(ENV_AFRDWLK_MCQ,$agree.);
run; 

/*Estimation of frequencies*/
PROC SURVEYFREQ  data= CLSAData ;        
TABLE  ENV_AFRDWLK_MCQ   ;        
ods output OneWay=FileOUT.Frequency;                                   
strata GEOSTRAT_TRM ; 
weight WGHTS_INFLATION_TRM;                                   
RUN; 

/*Estimation of population means*/
PROC SURVEYMEANS data= CLSAData ;        
VAR  HWT_DHT_M_TRM HWT_WGHT_KG_TRM;                                        
strata GEOSTRAT_TRM ; 
weight WGHTS_INFLATION_TRM;     
ods output Statistics=FileOUT.Means;                                     
RUN;

/*Estimation of ratios of population means*/
PROC SURVEYMEANS data= CLSAData   ratio;   
var HWT_WGHT_KG_TRM   HWT_DHT_M_TRM_sq;                    
ratio  HWT_WGHT_KG_TRM /  HWT_DHT_M_TRM_sq ;
strata GEOSTRAT_TRM ;                       
weight WGHTS_INFLATION_TRM;      
ods output  Ratio = FileOUT.Ratios ;                           
RUN;   

/*Estimation of population quantiles*/
PROC SURVEYMEANS data= CLSAData 
QUANTILE= (0.025 0.05 0.1 0.5 0.9 0.95 0.975) NONSYMCL;    
VAR    HWT_DHT_M_TRM HWT_WGHT_KG_TRM ;
STRATA GEOSTRAT_TRM ;  
WEIGHT WGHTS_INFLATION_TRM;     
ods output  Quantiles = FileOUT.Quantiles ;                                   
RUN;

/*Estimation of odds ratios, relative risks and risk differences*/

PROC Format; 
VALUE   $CBin  "Yes" = "1:Yes"  "No" = "2:No" ;
VALUE   $genB  "F" = "2:Female" "M" = "1:Male";
RUN;	

PROC SURVEYFREQ data=CLSAData  ORDER= FORMATTED ; 
TABLE  SEX_ASK_TRM * ORH_EXP_DRM_MCQ / OR RISK  ;
ODS output OddsRatio =  FileOUT.ORRDRR  ; 
STRATA GEOSTRAT_TRM;
WEIGHT WGHTS_INFLATION_TRM;
format ORH_EXP_DRM_MCQ $CBin. SEX_ASK_TRM $genB.;
RUN;  	


/*Analytic weights and estimation of model parameters*/
PROC SURVEYREG data=CLSAData order=formatted;
CLASS  WGHTS_PROV_TRM(ref="AB") Age_group_5(ref="45-48") 
       SEX_ASK_TRM(ref ="F") Education(ref="Low Education");
STRATA GEOSTRAT_TRM ;
MODEL  HWT_DHT_M_TRM = HWT_WGHT_KG_TRM  SEX_ASK_TRM Age_group_5 
       Education WGHTS_PROV_TRM / solution ;
WEIGHT WGHTS_ANALYTIC_TRM;
STORE  out=LinearReg; 
ods output  ParameterEstimates = FileOUT.LinearReg ;
RUN;

/* Linear regression (Prediction)*/
PROC plm source=LinearReg ALPHA=0.05 ;
score data=CLSAData(obs=1) out=FileOUT.LinearRegPredict predicted STDERR LCLM UCLM / ilink;
run;	

PROC print data=FileOUT.LinearRegPredict; 
VAR  predicted STDERR LCLM UCLM ;
format predicted 10.9 STDERR 10.9 LCLM 10.9 UCLM 10.9;
run;

/*Logistic regression analysis*/ 

PROC SURVEYLOGISTIC data=CLSAData ;
class  ORH_EXP_DRM_MCQ SEX_ASK_TRM(ref='F') Age_group_5(ref='45-48') 
       Education (ref='Low Education')  WGHTS_PROV_TRM(ref='AB')  /param=ref;                               
model  ORH_EXP_DRM_MCQ (event='Yes')= SEX_ASK_TRM Age_group_5 Education WGHTS_PROV_TRM;
strata GEOSTRAT_TRM;
weight WGHTS_ANALYTIC_TRM;
store  out= LogitReg; 
ODS output  ParameterEstimates = FileOUT.LogitReg;
RUN;

PROC plm source=LogitReg ALPHA=0.05 ;
score data=CLSAData(obs=1) out=FileOUT.LogitRegPredictict predicted STDERR LCLM UCLM / ilink;
run;	


/*Multinomial logistic regression analysis*/
PROC SURVEYLOGISTIC data=CLSAData ;            
CLASS WEA_MRTL_CURRENT_NEW(ref="Single") WGHTS_PROV_TRM(ref="AB") Age_group_5(ref="45-48") 
       SEX_ASK_TRM(ref ="F") Education(ref="Low Education")/param=ref;
MODEL WEA_MRTL_CURRENT_NEW = SEX_ASK_TRM Age_group_5 Education WGHTS_PROV_TRM /link=glogit; 
STRATA GEOSTRAT_TRM;             
WEIGHT WGHTS_ANALYTIC_TRM;  
ODS output  ParameterEstimates = FileOUT.MultiReg;              
RUN; 	


/*## Ordinal logistic regression analysis*/
PROC SURVEYLOGISTIC data=CLSAData;
CLASS ENV_AFRDWLK_MCQ WGHTS_PROV_TRM(ref="AB") Age_group_5(ref="45-48") 
       SEX_ASK_TRM(ref ="F") Education(ref="Low Education")/param=ref;
MODEL ENV_AFRDWLK_MCQ = SEX_ASK_TRM Age_group_5  Education WGHTS_PROV_TRM /clodds ;
STRATA GEOSTRAT_TRM;                           
WEIGHT WGHTS_ANALYTIC_TRM;
ODS output  ParameterEstimates = FileOUT.OrdinalReg;       
run;  

/*Adjusted odds ratio considering the survey design*/
PROC SURVEYLOGISTIC data=CLSAData;
CLASS ORH_EXP_DRM_MCQ  WGHTS_PROV_TRM(ref="AB") Age_group_5(ref="45-48") 
      SEX_ASK_TRM(ref ="F") Education(ref="Low Education")/param=ref;
MODEL ORH_EXP_DRM_MCQ(event='Yes') =  SEX_ASK_TRM Age_group_5 
       Education WGHTS_PROV_TRM /clodds ;
STRATA GEOSTRAT_TRM;                           
WEIGHT WGHTS_ANALYTIC_TRM;
ODS output CLOdds=FileOUT.AdjustOR ;      
run;  

/*Adjusted risk ratio without considering the survey design*/
PROC GENMOD data=CLSAData ;
CLASS ORH_EXP_DRM_MCQ SEX_ASK_TRM(ref='F') Age_group_5(ref='45-48')
      Education(ref='Low Education') WGHTS_PROV_TRM(ref='AB') /param=ref; 
MODEL ORH_EXP_DRM_MCQ(event='Yes') = SEX_ASK_TRM Age_group_5 
      Education WGHTS_PROV_TRM /dist=bin  link=log ; 
WEIGHT WGHTS_ANALYTIC_TRM;
ODS output ParameterEstimates=Est;
RUN;

Data FileOUT.AdjustRR; 
SET  Est(where= (Parameter ="SEX_ASK_TRM"));
AdjRR= exp(Estimate); 
AdjRRLL = exp( LowerWaldCL) ;
AdjRRUL = exp( UpperWaldCL) ;
RUN;

/*Adjusted risk difference without considering the survey design*/ 

PROC GENMOD data=CLSAData ;
CLASS ORH_EXP_DRM_MCQ SEX_ASK_TRM(ref='F') Age_group_5(ref='45-48')
      Education(ref='Low Education') WGHTS_PROV_TRM(ref='AB') /param=ref; 
MODEL ORH_EXP_DRM_MCQ(event='Yes') = SEX_ASK_TRM Age_group_5 
      Education WGHTS_PROV_TRM /dist=bin  link=identity  ; 
WEIGHT WGHTS_ANALYTIC_TRM;
ODS output ParameterEstimates=FileOUT.AdjustRD ;
RUN;

 
/*Domain Analysis  */
/* Estimation of domain proportions */
PROC SURVEYMEANS data= CLSAData mean;
VAR    ENV_AFRDWLK_MCQ;
DOMAIN ED_HIGH_TRM; 
STRATA GEOSTRAT_TRM;   
WEIGHT WGHTS_INFLATION_TRM; 
ODS output DOMAIN=FileOUT.DomainMeans;   
RUN;  	
 
/* Estimation of Quantile */ 
PROC SURVEYMEANS data= CLSAData 
QUANTILE= (0.025 0.05 0.1 0.5 0.9 0.95 0.975) NONSYMCL;    
VAR    HWT_DHT_M_TRM HWT_WGHT_KG_TRM ;
STRATA GEOSTRAT_TRM ;  
DOMAIN ED_HIGH_TRM; 
WEIGHT WGHTS_INFLATION_TRM;     
ods output  DomainQuantiles = FileOUT.DomainQuantiles ;                                   
RUN;


/*#Linear regression in domain analysis*/ 
PROC SURVEYREG data=CLSAData ;
CLASS   SEX_ASK_TRM(ref="F") Age_group_5(ref="45-48") 
        Education(ref="Low Education") WGHTS_PROV_TRM(ref ="AB");
MODEL   HWT_DHT_M_TRM = HWT_WGHT_KG_TRM SEX_ASK_TRM Age_group_5 Education 
        WGHTS_PROV_TRM / solution ;
WEIGHT  WGHTS_ANALYTIC_TRM;
STRATA  GEOSTRAT_TRM ;
DOMAIN  startlanguage; 
ODS output ParameterEstimates=FileOUT.LinearReg_EN;
RUN;
	
PROC PRINT data= FileOUT.LinearReg_EN;
WHERE  startlanguage = "en";
FORMAT _numeric_ 15.9; RUN; 



/*Logistic regression in domain analysis*/
PROC SURVEYLOGISTIC data=CLSAData ;            
CLASS ORH_EXP_DRM_MCQ(ref=first)   SEX_ASK_TRM(ref="F") Age_group_5(ref="45-48") 
      Education(ref="Low Education") WGHTS_PROV_TRM(ref ="AB") /param=ref ;
MODEL ORH_EXP_DRM_MCQ(event="Yes") = SEX_ASK_TRM Age_group_5 Education WGHTS_PROV_TRM /clodds ; 
STRATA GEOSTRAT_TRM ;             
WEIGHT WGHTS_ANALYTIC_TRM;    
DOMAIN  startlanguage;                                             
ODS output ParameterEstimates = FileOUT.LogitReg_EN;
RUN; 

PROC PRINT data= FileOUT.LogitReg_EN;
WHERE  startlanguage ="en";
FORMAT _numeric_ 15.9; RUN; 


/*Multinomial logistic regression in domain analysis*/
PROC SURVEYLOGISTIC data=CLSAData ;            
CLASS WEA_MRTL_CURRENT_NEW(ref="Single") WGHTS_PROV_TRM(ref="AB") Age_group_5(ref="45-48") 
       SEX_ASK_TRM(ref ="F") Education(ref="Low Education")/param=ref;
MODEL WEA_MRTL_CURRENT_NEW = SEX_ASK_TRM Age_group_5 Education WGHTS_PROV_TRM /link=glogit; 
STRATA GEOSTRAT_TRM;             
WEIGHT WGHTS_ANALYTIC_TRM;  
DOMAIN  startlanguage;     
ODS output  ParameterEstimates = FileOUT.MultiReg_EN;              
RUN; 	


Proc sort data= FileOUT.MultiReg_EN Out= MyParmEst ;
BY Response; 
WHERE  startlanguage = "en";
RUN; 

Proc print data= MyParmEst; RUN; 


/* Ordinal logistic regression in domain analysis */
PROC SURVEYLOGISTIC data=CLSAData;
CLASS ENV_AFRDWLK_MCQ WGHTS_PROV_TRM(ref="AB") Age_group_5(ref="45-48") 
       SEX_ASK_TRM(ref ="F") Education(ref="Low Education")/param=ref;
MODEL ENV_AFRDWLK_MCQ = SEX_ASK_TRM Age_group_5  Education WGHTS_PROV_TRM /clodds ;
STRATA GEOSTRAT_TRM;                           
WEIGHT WGHTS_ANALYTIC_TRM;
DOMAIN  startlanguage;     
ODS output  ParameterEstimates = FileOUT.OrdinalReg_EN;       
run;  



Proc print data= FileOUT.OrdinalReg_EN ;
WHERE  startlanguage = "en";
 RUN; 

/*  Adjusted odds ratio, relative risks and risk ratios in domain analysis*/
/*Adjusted odds ratio*/

PROC SURVEYLOGISTIC data=CLSAData ;
CLASS ORH_EXP_DRM_MCQ(ref=first)   SEX_ASK_TRM(ref="F") Age_group_5(ref="45-48") 
      Education(ref="Low Education") WGHTS_PROV_TRM(ref ="AB") /param=ref ;
MODEL ORH_EXP_DRM_MCQ(event="Yes") = SEX_ASK_TRM Age_group_5 Education 
       WGHTS_PROV_TRM /clodds ; 
STRATA GEOSTRAT_TRM;
WEIGHT WGHTS_ANALYTIC_TRM; 
DOMAIN startlanguage; 
ODS output CLOdds=FileOUT.AdjOR_EN ;
RUN;
PROC PRINT data=FileOUT.AdjOR_EN; 
FORMAT _numeric_ 10.8;
WHERE   startlanguage eq "en";
TITLE "Odds Ratio Estimates";
RUN;

/*Adjusted risk ratio without considering the survey design*/
PROC GENMOD data=CLSAData DESCENDING ;
CLASS ORH_EXP_DRM_MCQ(ref=first)   SEX_ASK_TRM(ref="F") Age_group_5(ref="45-48") 
      Education(ref="Low Education") WGHTS_PROV_TRM(ref ="AB") /param=ref ;
MODEL ORH_EXP_DRM_MCQ (event="Yes")= SEX_ASK_TRM Age_group_5  Education 
      WGHTS_PROV_TRM  /dist=bin  link=log; 
WEIGHT WGHTS_ANALYTIC_TRM;
where   startlanguage eq "en";
ODS output ParameterEstimates=Est ;
RUN;
Data FileOUT.AdjRR_EN; 
SET  Est(where= (Parameter ="SEX_ASK_TRM"));
AdjRR= exp(Estimate); 
AdjRRLL = exp( LowerWALDCL) ;
AdjRRUL = exp( UpperWALDCL) ;
RUN;
PROC PRINT data= FileOUT.AdjRR_EN;format _numeric_ 10.8; RUN; 

/*Adjusted risk difference without considering the survey design*/ 
PROC GENMOD data=CLSAData DESCENDING ;
CLASS ORH_EXP_DRM_MCQ(ref=first)   SEX_ASK_TRM(ref="F") Age_group_5(ref="45-48") 
      Education(ref="Low Education") WGHTS_PROV_TRM(ref ="AB") /param=ref ;
MODEL ORH_EXP_DRM_MCQ (event="Yes")= SEX_ASK_TRM Age_group_5  Education 
      WGHTS_PROV_TRM /dist=bin  link=identity;
WEIGHT WGHTS_ANALYTIC_TRM;
WHERE   startlanguage eq "en";
ODS output ParameterEstimates=FileOUT.AdjRD_EN ;
RUN;
PROC PRINT data= FileOUT.AdjRD_EN(where= (Parameter ="SEX_ASK_TRM"));
FORMAT _numeric_ 10.8; RUN;






