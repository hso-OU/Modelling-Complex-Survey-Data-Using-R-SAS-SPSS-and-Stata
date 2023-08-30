/* Change working directory*/
cd "\\files.math.uwaterloo.ca\hy2so\ResearchDocuments\CLSAExample" 

drop _all
import delimited \\files.math.uwaterloo.ca\hy2so\ResearchDocuments\CLSAExample\CLSASmallExample.csv , case(preserve) 
generate StraVar = GEOSTRAT_TRM  

generate HWT_DHT_M_TRM_sq  = HWT_DHT_M_TRM * HWT_DHT_M_TRM
generate BMI  = HWT_WGHT_KG_TRM / HWT_DHT_M_TRM_sq 

generate WEA_MRTL_CURRENT_New = "3:Others" 
replace  WEA_MRTL_CURRENT_New = "2:Married"  if WEA_MRTL_CURRENT == "Married"
replace  WEA_MRTL_CURRENT_New = "1:Single"   if WEA_MRTL_CURRENT == "Single"

generate ED_HIGH_TRM = "Non_university"   
replace  ED_HIGH_TRM = "University"         if ED_UDR11_TRM == 8 
replace  ED_HIGH_TRM = "University"         if ED_UDR11_TRM == 9 
replace  ED_HIGH_TRM = "University"         if ED_UDR11_TRM == 10

generate ENV_AFRDWLK_MCQ1 = "1:Strongly Agree"    if ENV_AFRDWLK_MCQ == "Strongly_Agree" 
replace  ENV_AFRDWLK_MCQ1 = "2:Agree"             if ENV_AFRDWLK_MCQ == "Agree" 
replace  ENV_AFRDWLK_MCQ1 = "3:Disagree"          if ENV_AFRDWLK_MCQ == "Disagree" 
replace  ENV_AFRDWLK_MCQ1 = "4:Strongly Disagree" if ENV_AFRDWLK_MCQ == "Strongly_Disagree"

drop  ENV_AFRDWLK_MCQ 
encode ENV_AFRDWLK_MCQ1, generate(ENV_AFRDWLK_MCQ) 


generate ORH_EXP_DRM_MCQ1 = 1 if  ORH_EXP_DRM_MCQ == "Yes" 
replace  ORH_EXP_DRM_MCQ1 = 0 if  ORH_EXP_DRM_MCQ == "No" 
drop      ORH_EXP_DRM_MCQ 
generate ORH_EXP_DRM_MCQ =  ORH_EXP_DRM_MCQ1


encode  SEX_ASK_TRM, generate (SEX_ASK_TRM1)  
drop    SEX_ASK_TRM
rename  SEX_ASK_TRM1  SEX_ASK_TRM

encode WEA_MRTL_CURRENT_New,  generate (WEA_MRTL_CURRENT_New1)
drop WEA_MRTL_CURRENT_New
rename WEA_MRTL_CURRENT_New1 WEA_MRTL_CURRENT_New


encode Age_group_5 , generate (Age_group_5_Num) 
drop  Age_group_5
rename  Age_group_5_Num Age_group_5 

encode Education , generate (Education_Num) 
drop Education 
rename  Education_Num Education 


encode WGHTS_PROV_TRM  , generate (WGHTS_PROV_TRM_Num) 
drop  WGHTS_PROV_TRM
rename WGHTS_PROV_TRM_Num  WGHTS_PROV_TRM 
 


svyset entity_id, strata(StraVar) weight(WGHTS_INFLATION_TRM) vce(linearized) singleunit(certainty) 
 /*Estimation of frequencies*/
svy linearized : tabulate ENV_AFRDWLK_MCQ, count se ci stubwidth(20) format(%10.0g)
estimates table, b(%10.0g) se(%10.0g)
 putexcel set "Freq", replace 
 putexcel (A1) =  matrix(r(coef))

/*Estimation of population means*/
svy linearized : mean HWT_DHT_M_TRM  HWT_WGHT_KG_TRM  
estimates table, b(%10.0g) se(%10.0g)
putexcel set "Mean", replace 
putexcel (A1) =  matrix(r(coef))

/*Estimation of ratios of population means*/
svy linearized : ratio (HWT_WGHT_KG_TRM/HWT_DHT_M_TRM_sq)
putexcel set "Ratio", replace 
estimates table, b(%10.0g) se(%10.0g)
putexcel (A1) =  matrix(r(coef))

 /*Estimation of odds ratios, relative risks and risk differences*/
putexcel set "ORRRRD", replace 
svy linearized: logistic ORH_EXP_DRM_MCQ SEX_ASK_TRM
estimates table, b(%10.0g) se(%10.0g)
putexcel (A1) =  matrix(r(coef))
svy linearized: glm ORH_EXP_DRM_MCQ   SEX_ASK_TRM, fam(binomial) link(log) eform	
estimates table, b(%10.0g) se(%10.0g)
putexcel (C1) =  matrix(r(coef))
svy linearized: glm ORH_EXP_DRM_MCQ   SEX_ASK_TRM, fam(binomial) link(identity)
estimates table, b(%10.0g) se(%10.0g)
putexcel (E1) =  matrix(r(coef)) 
 
 
svyset entity_id, strata(StraVar) weight(WGHTS_ANALYTIC_TRM) vce(linearized) singleunit(certainty)
/*Linear regression analysis*/
svy linearized : regress HWT_DHT_M_TRM HWT_WGHT_KG_TRM i.SEX_ASK_TRM i.Age_group_5 ib3.Education  i.WGHTS_PROV_TRM 
estimates table, b(%10.0g) se(%10.0g)
putexcel set "LinearReg", replace 
putexcel (A1) =  matrix(r(coef)) 

estimates save "LinearReg.ster", replace
 
/*Linear regression analysis(Predict)*/
estimates use "LinearReg.ster"
capture drop p1 se1
predict  p1  if entity_id == 724976,  xb  
predict  se1 if entity_id == 724976,  stdp 
display  p1
display  se1
display  p1 - invnormal(0.975)*se1
display  p1 + invnormal(0.975)*se1

putexcel set "LinearRegPredict", replace 
putexcel (A1) =  p1 
putexcel (B1) =  se1 


/*Logistic regression analysis*/
svy linearized: logit ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM 
estimates table, b(%10.0g) se(%10.0g)
putexcel set "LogitReg", replace 
putexcel (A1) =  matrix(r(coef)) 
estimates save "LogitReg.ster", replace
/*Logistic regression analysis(Predict)*/
estimates use "LogitReg.ster"
capture drop p2 se2
predict  p2  if entity_id == 724976  ,  xb
predict  se2 if entity_id == 724976  ,  stdp 
display  invlogit(p2)
display invlogit(p2 - invnormal(0.975)*se2)
display invlogit(p2 + invnormal(0.975)*se2)
putexcel set "LogitRegPredict", replace 
putexcel (A1) =  p2
putexcel (B1) =  se2 


/*Multinomial logistic regression analysis*/
svy linearized : mlogit  WEA_MRTL_CURRENT_New i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , baseoutcome(1)
estimates table, b(%10.0g) se(%10.0g)
putexcel set "MultiReg", replace 
putexcel (A1) =  matrix(r(coef)) 
/*Ordinal logistic regression analysis*/
svy linearized : ologit ENV_AFRDWLK_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM  
estimates table, b(%10.0g) se(%10.0g)
putexcel set "OrdinalReg", replace 
putexcel (A1) =  matrix(r(coef))  
/*Adjusted odds ratios, relative risks and risk ratios*/
svyset entity_id, strata(StraVar) weight(WGHTS_ANALYTIC_TRM) vce(linearized) singleunit(certainty)
svy linearized: glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , fam(binomial) link(logit) eform
estimates table, b(%10.0g) se(%10.0g)
putexcel set "AdjustORRRRD", replace 
putexcel (A1) =  matrix(r(coef))  
svy linearized: glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , fam(binomial) link(log) difficult iterate(30000) eform
estimates table, b(%10.0g) se(%10.0g)
putexcel (C1) =  matrix(r(coef))  
svy linearized: glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , fam(binomial) link(identity) difficult iterate(300)
 estimates table, b(%10.0g) se(%10.0g)
putexcel (E1) =  matrix(r(coef)) 

* The first change the survey design to that with inflation survey weight
svyset entity_id, strata(StraVar) weight(WGHTS_INFLATION_TRM)  vce(linearized) singleunit(certainty)
 /*Estimation of domain proportions*/ 
 putexcel set "DomainProp", replace 
* For people who never attained any university 
svy linearized, subpop(if ED_HIGH_TRM=="Non_university") : tabulate ENV_AFRDWLK_MCQ , cell se ci stubwidth(20) format(%10.0g)
estimates table, b(%10.0g) se(%10.0g)
putexcel (A1) =  matrix(r(coef)) 

* For people who have attained an university 
svy linearized, subpop(if ED_HIGH_TRM=="University") : tabulate ENV_AFRDWLK_MCQ ,  cell se ci stubwidth(20) format (%10.0g)
estimates table, b(%10.0g) se(%10.0g)
putexcel (C1) =  matrix(r(coef)) 

/*Linear regression in domain analysis*/ 
svyset entity_id, strata(StraVar) weight(WGHTS_ANALYTIC_TRM) vce(linearized) singleunit(certainty) 


svy linearized, subpop(if startlanguage == "en"): regress HWT_DHT_M_TRM HWT_WGHT_KG_TRM i.SEX_ASK_TRM i.Age_group_5 ib3.Education  i.WGHTS_PROV_TRM 
estimates table, b(%10.0g) se(%10.0g)
putexcel set "DomainLinearReg", replace 
putexcel (A1) =  matrix(r(coef)) 


/*Logistic regression in domain analysis*/ 
svy linearized, subpop(if startlanguage=="en"):logit ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM
estimates table, b(%10.0g) se(%10.0g)
putexcel set "DomainLogitReg", replace 
putexcel (A1) =  matrix(r(coef)) 

 
svy linearized, subpop(if  startlanguage=="en"): mlogit WEA_MRTL_CURRENT_New i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , baseoutcome(1)
estimates table, b(%10.0g) se(%10.0g)
putexcel set "DomainMultiReg", replace 
putexcel (A1) =  matrix(r(coef)) 
 
 
svy linearized, subpop(if  startlanguage=="en"):  ologit ENV_AFRDWLK_MCQ i.SEX_ASK_TRM  i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM  
 estimates table, b(%10.0g) se(%10.0g)
putexcel set "DomainOrdinalReg", replace 
putexcel (A1) =  matrix(r(coef))
 
 putexcel set "DomainAdjORRRRD", replace 
svyset entity_id, strata(StraVar) weight(WGHTS_ANALYTIC_TRM) vce(linearized) singleunit(certainty) 
*adjusted odds ratio
svy linearized, subpop(if startlanguage == "en"): glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM  , fam(binomial) link(logit) eform
estimates table, b(%10.0g) se(%10.0g)
putexcel (A1) =  matrix(r(coef))
*adjusted relative risk                 
svy linearized, subpop(if startlanguage == "en"): glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM  i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM  , fam(binomial) link(log) difficult  iterate(30000) eform	
estimates table, b(%10.0g) se(%10.0g)
putexcel (C1) =  matrix(r(coef))
*adjusted risk difference 
svy linearized,  subpop(if startlanguage == "en"): glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM  i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , fam(binomial) link(identity) difficult  iterate(30000)
estimates table, b(%10.0g) se(%10.0g)
putexcel (E1) =  matrix(r(coef))
 
 
svyset entity_id, strata(StraVar) weight(WGHTS_ANALYTIC_TRM) vce(linearized) singleunit(certainty) 
svy linearized: glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , fam(binomial) link(logit) eform
estimates table, b(%10.0g) se(%10.0g)
putexcel set "AdjustORRRRD", replace 
putexcel (A1) =  matrix(r(coef))  
svy linearized: glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , fam(binomial) link(log) difficult iterate(30000) eform
estimates table, b(%10.0g) se(%10.0g)
putexcel (C1) =  matrix(r(coef)) 
svy linearized: glm ORH_EXP_DRM_MCQ i.SEX_ASK_TRM i.Age_group_5 ib3.Education i.WGHTS_PROV_TRM , fam(binomial) link(identity) difficult iterate(100000)
estimates table, b(%10.0g) se(%10.0g)
putexcel (E1) =  matrix(r(coef)) 
 