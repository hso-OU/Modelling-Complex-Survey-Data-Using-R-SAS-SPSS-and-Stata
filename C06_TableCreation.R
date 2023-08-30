####  Create Table 
## set the working directory to the source file in Rstudio 
setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"EstimationResults",sep="/"))

Table.ID<-c("Frequencies", "Means", "Ratios","Quantiles","ORRRRD","Covariance", "LinearReg", "LinearPred", 
            "LogitReg", "LogitPred", "MultiReg","OrdinalReg", "AdjORRRRD", "DomainPropotions", 
            "DomainQuantile","DomainLinearReg","DomainLogisticReg", "DomainMultiReg","DomainOrdinalReg",
            "DomainAdjORRRRD","Ignore")
## load R results; name and index the tables 
R.namelist  <-  c( "CLSA.design", "CLSA.design.anly", "Table.Freq", "Table.Mean", "Table.ratio",
                    "Quant.Est", "Table.quantiles", "Table.odds", "Table.covar", 
                    "Table.LinearReg", "Table.LinearReg.Predict", "Table.LogitReg",
                    "Table.Logiteg.Predict", "Table.OrdinalReg","Table.Adj.OR.RR.RD",
                    "Table.Domain.Mean", "Table.Domain.quantiles","Table.Domain.LinearReg",
                    "Table.Domain.LogistReg", "Table.Domain.OrdinalReg", 
	                  "Table.Domain.OR.RD.RR", "Table.Domain.Ignore" )
R.number<- c(0,0,1,2,3,0,4,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21)

R.namelist2 <- vector("list", length = length(Table.ID))
for( x in 1:length(R.number)) { if(R.number[x]>0) R.namelist2[[ R.number[x]  ]] <- 
                                                c(R.namelist2[[ R.number[x]  ]], R.namelist[x]) }
load( paste( "R/RResults.RData",sep="") )

## load SAS results; name and index the tables 
library(haven)
SAS.namelist<-c("frequency", "means", "ratios", "quantiles", "orrdrr", "orrdrr2","linearreg","linearregpredict",
               "logitreg", "logitregpredictict","multireg", "ordinalreg","adjustor","adjustrr", "adjustrd",
               "domainmeans", "domainquantiles", "linearreg_en", "logitreg_en", "multireg_en", "ordinalreg_en",
               "adjor_en","adjrr_en", "adjrd_en")
SAS.number<- c(1,2,3,4,5,5,7,8,9,10,11,12,13,13,13,14,15,16,17,18,19,20,20,20 )

SAS.Read <- function( name.vec){
  i<-1
  temp <-      read_sas(paste("SAS/", name.vec[i] ,".sas7bdat",sep=""),  NULL) 
  while(  i < length(name.vec)){
    i<-i+1
  temp <- rbind(temp, read_sas(paste("SAS/", name.vec[i] ,".sas7bdat",sep=""),  NULL)  )
  }
  temp
}

SAS.namelist2 <- vector("list", length = length(Table.ID))
for( x in 1:length(SAS.number)) { if(SAS.number[x]>0) SAS.namelist2[[ SAS.number[x]  ]] <- 
                                                   c( SAS.namelist2[[ SAS.number[x]  ]],SAS.namelist[x]) }


## load SPSS results ; name and index the tables 
SPSS.namelist2<-list("Freq", "Mean", "Ratio", NA, "ORRRRD",NA,"LinearReg","LinearRegPredict",
                "LogitReg", "LogitRegPredict","MultiReg", "OrdinalReg",NA,
                "DomainFreq", NA, "DomainLinearReg", "DomainLogitReg", "DomainMultiReg", "DomainOrdinalReg",
                NA,NA)

library(readxl) 
temp.data <- read_excel(paste("SPSS/",SPSS.namelist2[[1]],".xls",sep=""), col_names = FALSE)



## load Stata results 
Stata.lines  <- readLines("Stata/StatResult2.txt") 

Stata.namelist2<-list("Freq", "Mean", "Ratio", NA, "ORRRRD",NA,"LinearReg","LinearRegPredict",
                     "LogitReg", "LogitRegPredict","MultiReg", "OrdinalReg", "AdjustORRRRD",
                     "DomainProp", NA, "DomainLinearReg", "DomainLogitReg", "DomainMultiReg", "DomainOrdinalReg",
                     "DomainAdjORRRRD",NA)


library(readxl)
temp.data <- read_excel(paste("Stata/",Stata.namelist2[[1]],".xlsx",sep=""), col_names = FALSE)

 

Table.ID<-c("Frequencies", "Means", "Ratios","Quantiles","ORRRRD","Covariance", "LinearReg", "LinearPred", 
            "LogitReg", "LogitPred", "MultiReg","OrdinalReg", "AdjORRRRD", "DomainPropotions", 
            "DomainQuantile","DomainLinearReg","DomainLogisticReg", "DomainMultiReg","DomainOrdinalReg",
            "DomainAdjORRRRD","Ignore")
## Table list 
library(knitr)
library(survey)
Table.list<- list()
an<-function(x){as.numeric(unlist(x))}
##Frequencies
R.data     <- eval(parse(text=R.namelist2[[1]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[1]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[1]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[1]],".xlsx",sep=""), col_names = FALSE)

Table.list[[1]]<-cbind( Category= c("Strongly Agree","Agree","Disagree","Strongly Disagree" ) 
                      ,    R.Total=c(R.data),            R.SE=SE(R.data) 
                      ,  SAS.Total=an(SAS.data[1:4,5]),     SAS.SE=SAS.data[1:4,6] 
                      , SPSS.Total=an(SPSS.data[14:17,3]), SPSS.SE=an(SPSS.data[14:17,4]) 
                      ,Stata.Total=an(Stata.data[,1]),     Stata.SE=an(sqrt(Stata.data[,2])))
 
kable(Table.list[[1]], digits = 0, format = "latex",linesep = c(""), row.names = FALSE,escape = TRUE)

## Estimation of population means
Tab.num<-2
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)

Table.list[[2]]<-data.frame( Category= c("\\texttt{HWT\\_DHT\\_M\\_TRM}"," ", "\\texttt{HWT\\_WGHT\\_KG\\_TRM}"," " )
                        ,Stat = c("Mean", "SE","Mean", "SE")
                        ,    R.Total = as.numeric(c(R.data, SE(R.data) )[c(1,3,2,4)]                           )  
                        ,  SAS.Total = as.numeric(c(an(SAS.data[1:2,3]), an(SAS.data[1:2,4]))[c(1,3,2,4)]      ) 
                        , SPSS.Total = as.numeric(c(an(SPSS.data[21:22,3]),an(SPSS.data[21:22,4]))[c(1,3,2,4)] )  
                        ,Stata.Total = as.numeric(c(an(Stata.data[,1]),an(sqrt(Stata.data[,2])))[c(1,3,2,4)]   ) ) 

kable(Table.list[[2]], digits = 4, format = "latex",linesep = c("","\\cline{2-6}",""), row.names = FALSE, escape = FALSE )


##Estimation of ratios of population means
Tab.num <- 3
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)

Table.list[[Tab.num]]<-data.frame( Stat = c("Estimate", "SE")
                             ,    R.Total = as.numeric(c(R.data[1], SE(R.data) )                       )  
                             ,  SAS.Total = as.numeric(c(an(SAS.data[1,3]), an(SAS.data[1,4]))         ) 
                             , SPSS.Total = as.numeric(c(an(SPSS.data[13,3]),an(SPSS.data[13,4]))      )  
                             ,Stata.Total = as.numeric(c(an(Stata.data[,1]),sqrt(an(Stata.data[,2])))  ) ) 

kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = c(""), row.names = FALSE, escape = FALSE )

## Estimation of population quantiles 
Tab.num <- 4
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)

Table.list[[Tab.num]]<-data.frame( 
 Stat      = c("Estimate",rep("",6), "SE",rep("",6)) 
,Quantile  = rep(c(0.025,0.05,0.1,0.5,0.9,0.95,0.975),2) 
,R.Total   = as.numeric(c(an(R.data[,1])))  
,SAS.Total = as.numeric(c(an(SAS.data[1:7,5:6])))
,R.Total2  = as.numeric(c(an(R.data[,2])))  
,SAS.Total2= as.numeric(c(an(SAS.data[1:7+7,5:6])))
)                                   
                                   
kable(Table.list[[Tab.num]], digits = 4, format = "latex",
      linesep = c(rep("",6),"\\hline", rep("",6)), row.names = FALSE, escape = FALSE )

                                   
## Estimation of odds ratios, relative risks and risk differences
Tab.num <- 5
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data1   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]][1],".sas7bdat",sep=""),  NULL) 
SAS.data2   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]][2],".sas7bdat",sep=""),  NULL) 
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)

# View(SPSS.data)
# View(Stata.data)

## Calculation for Stata
Stata.data<- array( an(Stata.data), c(2,6))
expit <- function( x) { 1/(1+exp(-x))}
Stata.temp<- c(   exp(Stata.data[1,1])
                , exp( Stata.data[1,1] - qnorm(0.975)*sqrt(Stata.data[1,2]) )
                , exp( Stata.data[1,1] + qnorm(0.975)*sqrt(Stata.data[1,2]) )
                , exp(Stata.data[1,3])
                , exp( Stata.data[1,3] - qnorm(0.975)*sqrt(Stata.data[1,4]) )
                , exp( Stata.data[1,3] + qnorm(0.975)*sqrt(Stata.data[1,4]) )
                ,    (Stata.data[1,5])
                ,    ( Stata.data[1,5] - qnorm(0.975)*sqrt(Stata.data[1,6]) )
                ,    ( Stata.data[1,5] + qnorm(0.975)*sqrt(Stata.data[1,6]) )                
)

Table.list[[Tab.num]]<-data.frame( 
Stat = c("Odds ratio (M vs F)"     , "95\\% lower confidence limit", "95\\% upper confidence limit"
        ,"Relative risk (M vs F)"  , "95\\% lower confidence limit", "95\\% upper confidence limit"
        ,"Risk difference (M vs F)", "95\\% lower confidence limit", "95\\% upper confidence limit")
 ,    R.Total = as.numeric(c(R.data))   
 ,  SAS.Total = as.numeric(c( c(an(t(SAS.data1[1:2,3:5]))), SAS.data2[4,c(3,5,6)] )) 
 , SPSS.Total = as.numeric(c( 1/(an(SPSS.data[33,c(4,6,5)])), an(t(SPSS.data[c(35,37),4:6])) ))  
 ,Stata.Total = Stata.temp ) 

kable(Table.list[[Tab.num]], digits = 4, format = "latex",
      linesep = c("","","\\midrule","","","\\midrule","",""), row.names = FALSE, escape = FALSE )

##Estimation of covariance
Tab.num <- 6
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))


Table.list[[Tab.num]]<-data.frame( Stat = c("Var(\\texttt{HWT\\_DHT\\_M\\_TRM})"
                                           ,"Var(\\texttt{HWT\\_WGHT\\_KG\\_TRM})"
                                           ,"Cov(\\texttt{HWT\\_DHT\\_M\\_TRM,HWT\\_WGHT\\_KG\\_TRM})")
  ,    R.Total = format(R.data[c(1,4,3),] , digits= 4)        )  
                                    
 
kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = c(""), row.names = FALSE, escape = FALSE )


##Linear regression analysis
Tab.num <- 7
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)

sep.vec<- c("","","\\multicolumn{9}{l}{\\underline{\\textit{Age Groups}}}\\\\ 
  \\multicolumn{9}{l}{\\textit{(relative to Age\\_Gpr0: Age 45-48)}}\\\\", "","",""
,"\\multicolumn{9}{l}{\\underline{\\textit{Education Levels}}} \\\\ 
  \\multicolumn{9}{l}{\\textit{(relative to Lower Education)}}\\\\","",""
,"\\multicolumn{9}{l}{\\underline{\\textit{Provinces}}} \\\\ 
  \\multicolumn{9}{l}{\\textit{(relative to Alberta)}} \\\\", rep("",8))

Table.list[[Tab.num]]<-data.frame(  Stat = c( "(Intercept)"
, "\\texttt{HWT\\_WGHT\\_KG\\_TRM}", "\\texttt{SEX\\_ASK\\_TRM=\"M\"}"
,"Age\\_Gpr1:Age 49-54", "Age\\_Gpr2:Age 55-64","Age\\_Gpr3:Age 65-74","Age\\_Gpr4:Age 75+"
,"Medium Education " ,"Higher Education lower" ,"Higher Education upper"
,"British Columbia"  ,"Manitoba"  ,"New Brunswick"  ,"Newfoundland \\& Labrador"
,"Nova Scotia"       ,"Ontario"   ,"Prince Edward Island" ,"Quebec" ,"Saskatchewan"            
)
,  R.Est    =  R.data[,1]      ,   R.SE =  R.data[,2]
,  SAS.Est  = an(SAS.data[c(1:3,5:8,12,10,11,14:22),2])
,  SAS.SE   = an(SAS.data[c(1:3,5:8,12,10,11,14:22),3])
, SPSS.Est  = an(SPSS.data[c(79,101,80,82:85, 87:89,91:99),2])
, SPSS.SE   = an(SPSS.data[c(79,101,80,82:85, 87:89,91:99),3])
, Stata.Est = an(Stata.data[c(23,1,3,5:8,12,9,10,14:22),1])
, Stata.SE  = sqrt(an(Stata.data[c(23,1,3,5:8,12,9,10,14:22),2]))
)  
                 

kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = sep.vec, row.names = FALSE, escape = FALSE )


##Linear regression analysis (Predict)
Tab.num <- 8
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read.csv(paste("SPSS/",SPSS.namelist2[[Tab.num]],".csv",sep=""))[1,]
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)

Table.list[[Tab.num]]<-data.frame( Stat = c("Predicted value","Standard error"
  ,"95\\% lower confidence limit" ,"95\\% upper confidence limit")
,    R.Total = R.data
,  SAS.Total = an(SAS.data[,22:25])
, SPSS.Total = c( round(an(SPSS.data[22:23]), digits=4 ),rep("Not provided",2) )
,Stata.Total = c(an(Stata.data[1]),(an(Stata.data[2])), 
                 an(Stata.data[1])+qnorm(c(.025,.975))*(an(Stata.data[2])))
)  


kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = c(""), row.names = FALSE, escape = FALSE )



##Logitistic regression analysis
Tab.num <- 9
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)




sep.vec<- c("","\\multicolumn{9}{l}{\\underline{\\textit{Age Groups}}}\\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Age\\_Gpr0: Age 45-48)}}\\\\", "","",""
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Education Levels}}} \\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Lower Education)}}\\\\","",""
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Provinces}}} \\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Alberta)}} \\\\", rep("",8))

Table.list[[Tab.num]]<-data.frame( 
  Stat = c( "(Intercept)"
, "\\texttt{SEX\\_ASK\\_TRM=\"M\"}"
,"Age\\_Gpr1:Age 49-54", "Age\\_Gpr2:Age 55-64","Age\\_Gpr3:Age 65-74","Age\\_Gpr4:Age 75+"
,"Medium Education " ,"Higher Education lower" ,"Higher Education upper"
,"British Columbia"  ,"Manitoba"  ,"New Brunswick"  ,"Newfoundland \\& Labrador"
,"Nova Scotia"       ,"Ontario"   ,"Prince Edward Island" ,"Quebec" ,"Saskatchewan"            
)
,  R.Est    =  R.data[,1]      ,   R.SE =  R.data[,2]
,  SAS.Est  = an(SAS.data[c(1:6,9,7,8,10:18),4])
,  SAS.SE   = an(SAS.data[c(1:6,9,7,8,10:18),5])
, SPSS.Est  = an(SPSS.data[c(83,84,86:89,91:93,95:103),3])
, SPSS.SE   = an(SPSS.data[c(83,84,86:89,91:93,95:103),4])
, Stata.Est = an(Stata.data[c(21,1,3:6,10,7,8,12:20),1])
, Stata.SE  = sqrt(an(Stata.data[c(21,1,3:6,10,7,8,12:20),2]))
)  


kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = sep.vec, row.names = FALSE, escape = FALSE )


##Logitistic regression analysis (Predict)
Tab.num <- 10
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read.csv(paste("SPSS/",SPSS.namelist2[[Tab.num]],".csv",sep=""))[1,]
Stata.data <- unlist(read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE))
View(SPSS.data)
View(Stata.data)

Table.list[[Tab.num]]<-data.frame( Stat = c("Predicted value","Standard error"
                                            ,"95\\% lower confidence limit" ,"95\\% upper confidence limit")
             ,    R.Total = R.data
             ,  SAS.Total = an(SAS.data[,22:25])
             , SPSS.Total = c(round(1-an(SPSS.data[24]) , digits=4 ),rep("Not provided",3) )
             ,Stata.Total = c(round(expit(Stata.data[1]), digits=4 ),"Not provided", 
                              round(expit(Stata.data[1] +qnorm(c(.025,.975))*(an(Stata.data[2]))), digits=4))
)  


kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = c(""), row.names = FALSE, escape = FALSE )




##Multinomial regression analysis
Tab.num <- 11
 
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SAS.data)
View(SPSS.data)
View(Stata.data)


sep.vec<- c("","\\multicolumn{7}{l}{\\underline{\\textit{Age Groups}}}\\\\ 
            \\multicolumn{7}{l}{\\textit{(relative to Age\\_Gpr0: Age 45-48)}}\\\\", "","",""
            ,"\\multicolumn{7}{l}{\\underline{\\textit{Education Levels}}} \\\\ 
            \\multicolumn{7}{l}{\\textit{(relative to Lower Education)}}\\\\","",""
            ,"\\multicolumn{7}{l}{\\underline{\\textit{Provinces}}} \\\\ 
            \\multicolumn{7}{l}{\\textit{(relative to Alberta)}} \\\\", rep("",8))

Table.list[[Tab.num]]<-data.frame( 
  Stat = c( "(Intercept)"
            , "\\texttt{SEX\\_ASK\\_TRM=\"M\"}"
            ,"Age\\_Gpr1:Age 49-54", "Age\\_Gpr2:Age 55-64","Age\\_Gpr3:Age 65-74","Age\\_Gpr4:Age 75+"
            ,"Medium Education " ,"Higher Education lower" ,"Higher Education upper"
            ,"British Columbia"  ,"Manitoba"  ,"New Brunswick"  ,"Newfoundland \\& Labrador"
            ,"Nova Scotia"       ,"Ontario"   ,"Prince Edward Island" ,"Quebec" ,"Saskatchewan"            
  )
 
  ,  SAS.Est  = an(SAS.data[c( 2*(1:6)-1, 17, 13, 15, 2*(10:18)-1, 2*(1:6), 18, 14, 16, 2*(10:18)),5])
  ,  SAS.SE   = an(SAS.data[c( 2*(1:6)-1, 17, 13, 15, 2*(10:18)-1, 2*(1:6), 18, 14, 16, 2*(10:18)),6])
  , SPSS.Est  = an(SPSS.data[c( (73:93)[-c(3,8,12)],(73:93)[-c(3,8,12)]+22),3] )
  , SPSS.SE   = an(SPSS.data[c( (73:93)[-c(3,8,12)],(73:93)[-c(3,8,12)]+22),4] )
  , Stata.Est = an(Stata.data[c(c(21,1,3:6,10,7,8,12:20),c(21,1,3:6,10,7,8,12:20)+22),1])
  , Stata.SE  = sqrt( an(Stata.data[c(c(21,1,3:6,10,7,8,12:20),c(21,1,3:6,10,7,8,12:20)+22),2]))
)  


kable(Table.list[[Tab.num]], digits = 4, format = "latex",
      linesep = c( sep.vec,"	\\midrule	\\multicolumn{7}{l}{\\textbf{3:Others}}\\\\",sep.vec),
      row.names = FALSE, escape = FALSE )


## Ordinal logistic regression analysis %12
Tab.num <- 12
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)




sep.vec<- c("\\multicolumn{9}{l}{\\underline{\\textit{Age Groups}}}\\\\ 
             \\multicolumn{9}{l}{\\textit{(relative to Age\\_Gpr0: Age 45-48)}}\\\\", rep("",3)
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Education Levels}}} \\\\ 
              \\multicolumn{9}{l}{\\textit{(relative to Lower Education)}}\\\\", rep("",2)
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Provinces}}} \\\\ 
              \\multicolumn{9}{l}{\\textit{(relative to Alberta)}} \\\\", rep("",8)
            ,"\\multicolumn{9}{l}{\\underline{\\textit{(Intercepts)}}} \\\\", rep("",2)
            )

Table.list[[Tab.num]]<-data.frame( 
  Stat = c(  "\\texttt{SEX\\_ASK\\_TRM=\"M\"}"
            ,"Age\\_Gpr1:Age 49-54", "Age\\_Gpr2:Age 55-64","Age\\_Gpr3:Age 65-74","Age\\_Gpr4:Age 75+"
            ,"Medium Education " ,"Higher Education lower" ,"Higher Education upper"
            ,"British Columbia"  ,"Manitoba"  ,"New Brunswick"  ,"Newfoundland \\& Labrador"
            ,"Nova Scotia"       ,"Ontario"   ,"Prince Edward Island" ,"Quebec" ,"Saskatchewan" 
            ,"Strongly Agree|Agree","Agree|Disagree", "Disagree|Strongly Disagree"
  )
  ,  R.Est    =  R.data[,1]      ,   R.SE =  R.data[,2]
  ,  SAS.Est  = an(SAS.data[c(4:20,1:3),4])*c(rep(-1,17),rep(1,3))
  ,  SAS.SE   = an(SAS.data[c(4:20,1:3),5])
  , SPSS.Est  = an(SPSS.data[c((74:93)[-c(2,7,11)],71:73),3])
  , SPSS.SE   = an(SPSS.data[c((74:93)[-c(2,7,11)],71:73),4])
  , Stata.Est = an(Stata.data[c(1,3:6,10,7,8,12:23),1])
  , Stata.SE  = sqrt(an(Stata.data[c(1,3:6,10,7,8,12:23),2]))
)  

kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = sep.vec, row.names = FALSE, escape = FALSE )


##Adjusted odds ratios, relative risks and risk ratios  %13
Tab.num <- 13
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data1   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]][1],".sas7bdat",sep=""),  NULL) 
SAS.data2   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]][2],".sas7bdat",sep=""),  NULL) 
SAS.data3   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]][3],".sas7bdat",sep=""),  NULL) 
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)

# View(SAS.data1)
# View(SAS.data2)
# View(SAS.data3)
# 
# View(SPSS.data)
# View(Stata.data)

## Calculation for Stata
Stata.data <- array( an(Stata.data[1,]), c(1,6))
expit <- function( x) { 1/(1+exp(-x))}
Stata.temp<- c(   exp(Stata.data[1,1])
                  , exp( Stata.data[1,1] - qnorm(0.975)*sqrt(Stata.data[1,2]) )
                  , exp( Stata.data[1,1] + qnorm(0.975)*sqrt(Stata.data[1,2]) )
                  , exp(Stata.data[1,3])
                  , exp( Stata.data[1,3] - qnorm(0.975)*sqrt(Stata.data[1,4]) )
                  , exp( Stata.data[1,3] + qnorm(0.975)*sqrt(Stata.data[1,4]) )
                  ,    (Stata.data[1,5])
                  ,    ( Stata.data[1,5] - qnorm(0.975)*sqrt(Stata.data[1,6]) )
                  ,    ( Stata.data[1,5] + qnorm(0.975)*sqrt(Stata.data[1,6]) )                
)

Table.list[[Tab.num]]<-data.frame( 
  Stat = c( "Adjusted Odds ratio (M vs F)"     , "95\\% lower confidence limit", "95\\% upper confidence limit"
           ,"Adjusted Relative risk (M vs F)"  , "95\\% lower confidence limit", "95\\% upper confidence limit"
           ,"Adjusted Risk difference (M vs F)", "95\\% lower confidence limit", "95\\% upper confidence limit")
  ,    R.Total = as.numeric(c(R.data))   
  ,  SAS.Total = an(c( SAS.data1[1,3:5], SAS.data2[1,10:12], SAS.data3[2,c(4,6,7)]))  
  ,Stata.Total = Stata.temp ) 

kable(Table.list[[Tab.num]], digits = 4, format = "latex",
      linesep = c("","","\\midrule","","","\\midrule","",""), row.names = FALSE, escape = FALSE )




## Estimation of domain proportions 
Tab.num <- 14
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)

Table.list[[Tab.num]]<-cbind( Category= rep(c("Strongly Agree","Agree","Disagree","Strongly Disagree" ),2) 
                        ,    R.Total=c(R.data[,1]),              R.SE=c(R.data[,2]) 
                        ,  SAS.Total=an(SAS.data[,5]),     SAS.SE=SAS.data[ ,6] 
                        , SPSS.Total=an(SPSS.data[(26:34)[-5],4]), SPSS.SE=an(SPSS.data[(26:34)[-5],5]) 
                        ,Stata.Total=an(Stata.data[,c(1,3)]),     Stata.SE=an(sqrt(Stata.data[,c(2,4)])))


sep.vec<- c(rep("",3), "\\multicolumn{9}{l}{Group: \\textit{University}}\\\\",rep("",3) )

kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = sep.vec, row.names = FALSE,escape = TRUE)


## Estimation of domain quantiles
Tab.num <- 15
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))  ## > U, non- UU, non-U
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)


Table.list[[Tab.num]]<-data.frame( Quantiles= rep(c("0.025","0.05","0.1","0.5","0.9","0.95","0.975"),2) 
,  R1 = R.data[,1]  ,SAS1 = an(SAS.data[1:7   ,7:8])
,  R2 = R.data[,2]  ,SAS2 = an(SAS.data[1:7+14,7:8])
,  R3 = R.data[,3]  ,SAS3 = an(SAS.data[1:7+7 ,7:8])
,  R4 = R.data[,4]  ,SAS4 = an(SAS.data[1:7+21,7:8]) 
)


sep.vec<- c(rep("",6), "\\multicolumn{1}{l}{SE}\\\\",rep("",6) )

#Table.list[[Tab.num]][,-1]<-round(Table.list[[Tab.num]][,-1], digits = 4)

kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = sep.vec, row.names = FALSE,escape = TRUE)






##Domain linear regression analysis 
Tab.num <- 16
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)

sep.vec<- c("","","\\multicolumn{9}{l}{\\underline{\\textit{Age Groups}}}\\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Age\\_Gpr0: Age 45-48)}}\\\\", "","",""
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Education Levels}}} \\\\ 
  \\multicolumn{9}{l}{\\textit{(relative to Lower Education)}}\\\\","",""
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Provinces}}} \\\\ 
  \\multicolumn{9}{l}{\\textit{(relative to Alberta)}} \\\\", rep("",8))

Table.list[[Tab.num]]<-data.frame(  Stat = c( "(Intercept)"
  , "\\texttt{HWT\\_WGHT\\_KG\\_TRM}", "\\texttt{SEX\\_ASK\\_TRM=\"M\"}"
  ,"Age\\_Gpr1:Age 49-54", "Age\\_Gpr2:Age 55-64","Age\\_Gpr3:Age 65-74","Age\\_Gpr4:Age 75+"
  ,"Medium Education " ,"Higher Education lower" ,"Higher Education upper"
  ,"British Columbia"  ,"Manitoba"  ,"New Brunswick"  ,"Newfoundland \\& Labrador"
  ,"Nova Scotia"       ,"Ontario"   ,"Prince Edward Island" ,"Quebec" ,"Saskatchewan"            
)
,  R.Est    =  R.data[,1]      ,   R.SE =  R.data[,2]
,  SAS.Est  = an(SAS.data[c(1:3,5:8,12,10,11,14:22)+23,2])
,  SAS.SE   = an(SAS.data[c(1:3,5:8,12,10,11,14:22)+23,3])
, SPSS.Est  = an(SPSS.data[c(79,101,80,82:85, 87:89,91:99),2])
, SPSS.SE   = an(SPSS.data[c(79,101,80,82:85, 87:89,91:99),3])
, Stata.Est = an(Stata.data[c(23,1,3,5:8,12,9,10,14:22),1])
, Stata.SE  = sqrt(an(Stata.data[c(23,1,3,5:8,12,9,10,14:22),2]))
)  


kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = sep.vec, row.names = FALSE, escape = FALSE )


##Domain Logitistic regression analysis
Tab.num <- 17
R.data     <- coefficients( eval(parse(text=R.namelist2[[Tab.num]])) )[,1:2]
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)


sep.vec<- c("","\\multicolumn{9}{l}{\\underline{\\textit{Age Groups}}}\\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Age\\_Gpr0: Age 45-48)}}\\\\", "","",""
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Education Levels}}} \\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Lower Education)}}\\\\","",""
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Provinces}}} \\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Alberta)}} \\\\", rep("",8))

Table.list[[Tab.num]]<-data.frame( 
  Stat = c( "(Intercept)"
            , "\\texttt{SEX\\_ASK\\_TRM=\"M\"}"
            ,"Age\\_Gpr1:Age 49-54", "Age\\_Gpr2:Age 55-64","Age\\_Gpr3:Age 65-74","Age\\_Gpr4:Age 75+"
            ,"Medium Education " ,"Higher Education lower" ,"Higher Education upper"
            ,"British Columbia"  ,"Manitoba"  ,"New Brunswick"  ,"Newfoundland \\& Labrador"
            ,"Nova Scotia"       ,"Ontario"   ,"Prince Edward Island" ,"Quebec" ,"Saskatchewan"            
  )
  ,  R.Est    =  R.data[,1]      ,   R.SE =  R.data[,2]
  ,  SAS.Est  = an(SAS.data[c(1:6,9,7,8,10:18)+18,4])
  ,  SAS.SE   = an(SAS.data[c(1:6,9,7,8,10:18)+18,5])
  , SPSS.Est  = an(SPSS.data[c(83,84,86:89,91:93,95:103)-8,3])
  , SPSS.SE   = an(SPSS.data[c(83,84,86:89,91:93,95:103)-8,4])
  , Stata.Est = an(Stata.data[c(21,1,3:6,10,7,8,12:20),1])
  , Stata.SE  = sqrt(an(Stata.data[c(21,1,3:6,10,7,8,12:20),2]))
)  


kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = sep.vec, row.names = FALSE, escape = FALSE )


##Multinomial regression in domain analysis
Tab.num <- 18

SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SAS.data)
View(SPSS.data)
View(Stata.data)


sep.vec<- c("","\\multicolumn{7}{l}{\\underline{\\textit{Age Groups}}}\\\\ 
            \\multicolumn{7}{l}{\\textit{(relative to Age\\_Gpr0: Age 45-48)}}\\\\", "","",""
            ,"\\multicolumn{7}{l}{\\underline{\\textit{Education Levels}}} \\\\ 
            \\multicolumn{7}{l}{\\textit{(relative to Lower Education)}}\\\\","",""
            ,"\\multicolumn{7}{l}{\\underline{\\textit{Provinces}}} \\\\ 
            \\multicolumn{7}{l}{\\textit{(relative to Alberta)}} \\\\", rep("",8))

Table.list[[Tab.num]]<-data.frame( 
  Stat = c( "(Intercept)"
            , "\\texttt{SEX\\_ASK\\_TRM=\"M\"}"
            ,"Age\\_Gpr1:Age 49-54", "Age\\_Gpr2:Age 55-64","Age\\_Gpr3:Age 65-74","Age\\_Gpr4:Age 75+"
            ,"Medium Education " ,"Higher Education lower" ,"Higher Education upper"
            ,"British Columbia"  ,"Manitoba"  ,"New Brunswick"  ,"Newfoundland \\& Labrador"
            ,"Nova Scotia"       ,"Ontario"   ,"Prince Edward Island" ,"Quebec" ,"Saskatchewan"            
  )
  
  ,  SAS.Est  = an(SAS.data[c( 2*(1:6)-1, 17, 13, 15, 2*(10:18)-1, 2*(1:6), 18, 14, 16, 2*(10:18))+36,5])
  ,  SAS.SE   = an(SAS.data[c( 2*(1:6)-1, 17, 13, 15, 2*(10:18)-1, 2*(1:6), 18, 14, 16, 2*(10:18))+36,6])
  , SPSS.Est  = an(SPSS.data[c( (73:93)[-c(3,8,12)],(73:93)[-c(3,8,12)]+22)+9,3] )
  , SPSS.SE   = an(SPSS.data[c( (73:93)[-c(3,8,12)],(73:93)[-c(3,8,12)]+22)+9,4] )
  , Stata.Est = an(Stata.data[c(c(21,1,3:6,10,7,8,12:20),c(21,1,3:6,10,7,8,12:20)+22),1])
  , Stata.SE  = sqrt( an(Stata.data[c(c(21,1,3:6,10,7,8,12:20),c(21,1,3:6,10,7,8,12:20)+22),2]))
)  


kable(Table.list[[Tab.num]], digits = 4, format = "latex",
      linesep = c( sep.vec,"	\\midrule	\\multicolumn{7}{l}{\\textbf{3:Others}}\\\\",sep.vec),
      row.names = FALSE, escape = FALSE )


## Ordinal logistic regression in domain analysis %19
Tab.num <- 19
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]],".sas7bdat",sep=""),  NULL)
SPSS.data  <- read_excel(paste("SPSS/",SPSS.namelist2[[Tab.num]],".xls",sep=""), col_names = FALSE)
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)
View(SPSS.data)
View(Stata.data)




sep.vec<- c("\\multicolumn{9}{l}{\\underline{\\textit{Age Groups}}}\\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Age\\_Gpr0: Age 45-48)}}\\\\", rep("",3)
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Education Levels}}} \\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Lower Education)}}\\\\", rep("",2)
            ,"\\multicolumn{9}{l}{\\underline{\\textit{Provinces}}} \\\\ 
            \\multicolumn{9}{l}{\\textit{(relative to Alberta)}} \\\\", rep("",8)
            ,"\\multicolumn{9}{l}{\\underline{\\textit{(Intercepts)}}} \\\\", rep("",2)
)

Table.list[[Tab.num]]<-data.frame( 
  Stat = c(  "\\texttt{SEX\\_ASK\\_TRM=\"M\"}"
             ,"Age\\_Gpr1:Age 49-54", "Age\\_Gpr2:Age 55-64","Age\\_Gpr3:Age 65-74","Age\\_Gpr4:Age 75+"
             ,"Medium Education " ,"Higher Education lower" ,"Higher Education upper"
             ,"British Columbia"  ,"Manitoba"  ,"New Brunswick"  ,"Newfoundland \\& Labrador"
             ,"Nova Scotia"       ,"Ontario"   ,"Prince Edward Island" ,"Quebec" ,"Saskatchewan" 
             ,"Strongly Agree|Agree","Agree|Disagree", "Disagree|Strongly Disagree"
  )
  ,  R.Est    =  R.data[,1]      ,   R.SE =  R.data[,2]
  ,  SAS.Est  = an(SAS.data[c(4:20,1:3)+20,4])*c(rep(-1,17),rep(1,3))
  ,  SAS.SE   = an(SAS.data[c(4:20,1:3)+20,5])
  , SPSS.Est  = an(SPSS.data[c((74:93)[-c(2,7,11)],71:73)+4,3])
  , SPSS.SE   = an(SPSS.data[c((74:93)[-c(2,7,11)],71:73)+4,4])
  , Stata.Est = an(Stata.data[c(1,3:6,10,7,8,12:23),1])
  , Stata.SE  = sqrt(an(Stata.data[c(1,3:6,10,7,8,12:23),2]))
)  

kable(Table.list[[Tab.num]], digits = 4, format = "latex",linesep = sep.vec, row.names = FALSE, escape = FALSE )


##Adjusted odds ratios, relative risks and risk ratios in domain analysis  %20
Tab.num <- 20
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))
SAS.data1   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]][1],".sas7bdat",sep=""),  NULL) 
SAS.data2   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]][2],".sas7bdat",sep=""),  NULL) 
SAS.data3   <- read_sas(paste("SAS/",SAS.namelist2[[Tab.num]][3],".sas7bdat",sep=""),  NULL) 
Stata.data <- read_excel(paste("Stata/",Stata.namelist2[[Tab.num]],".xlsx",sep=""), col_names = FALSE)

View(SAS.data1)
View(SAS.data2)
View(SAS.data3)

View(SPSS.data)
View(Stata.data)

## Calculation for Stata
Stata.data <- array( an(Stata.data[1,]), c(1,6))
expit <- function( x) { 1/(1+exp(-x))}
Stata.temp<- c(   exp(Stata.data[1,1])
                  , exp( Stata.data[1,1] - qnorm(0.975)*sqrt(Stata.data[1,2]) )
                  , exp( Stata.data[1,1] + qnorm(0.975)*sqrt(Stata.data[1,2]) )
                  , exp(Stata.data[1,3])
                  , exp( Stata.data[1,3] - qnorm(0.975)*sqrt(Stata.data[1,4]) )
                  , exp( Stata.data[1,3] + qnorm(0.975)*sqrt(Stata.data[1,4]) )
                  ,    (Stata.data[1,5])
                  ,    ( Stata.data[1,5] - qnorm(0.975)*sqrt(Stata.data[1,6]) )
                  ,    ( Stata.data[1,5] + qnorm(0.975)*sqrt(Stata.data[1,6]) )                
)

Table.list[[Tab.num]]<-data.frame( 
  Stat = c( "Adjusted Odds ratio (M vs F)"     , "95\\% lower confidence limit", "95\\% upper confidence limit"
            ,"Adjusted Relative risk (M vs F)"  , "95\\% lower confidence limit", "95\\% upper confidence limit"
            ,"Adjusted Risk difference (M vs F)", "95\\% lower confidence limit", "95\\% upper confidence limit")
  ,    R.Total = as.numeric(c(R.data))  
  ,  SAS.Total = an(c( SAS.data1[18,3:5], SAS.data2[1,10:12], SAS.data3[2,c(4,6,7)]))  
  ,Stata.Total = Stata.temp ) 

kable(Table.list[[Tab.num]], digits = 4, format = "latex",
      linesep = c("","","\\midrule","","","\\midrule","",""), row.names = FALSE, escape = FALSE )



## What is Ignore  %21
Tab.num <- 21
R.data     <- eval(parse(text=R.namelist2[[Tab.num]]))

Table.list[[Tab.num]]<-data.frame( 
  Stat = rep(c("Strongly Agree","Agree","Disagree","Strongly Disagree" ),2) 
  ,    R.Total = R.data  
) 
sep.vec<- c(rep("",3),"	\\multicolumn{5}{l}{Group:\\textit{BMI $<19$}} \\\\",rep("",3) )


kable(Table.list[[Tab.num]], digits = 4, format = "latex",
      linesep = sep.vec, row.names = FALSE, escape = FALSE )


## 864



