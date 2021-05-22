

##creating empty objects to fill in from loops: coefficient, standard error, p-value, and coverage
library(simstudy)
library(car)
library(mice)
library(Amelia)
library(WriteXLS)
library(nlme)
library(OpenMx)



#this is for full analysis set
Undeleted_coef = NULL
Undeleted_SE = NULL
Undeleted_p = NULL
UnDel_coverage = matrix(nrow=1000, ncol=2)


#linear mixed model
LMM_coef = NULL
LMM_se = NULL
LMM_p = NULL
LMM_coverage = matrix(nrow=1000, ncol=2)

#linear mixed model--Aux Level 2
LMM_coef2 = NULL
LMM_se2 = NULL
LMM_p2 = NULL
LMM_coverage2 = matrix(nrow=1000, ncol=2)

#linear mixed model--Aux Level 1
LMM_coef3 = NULL
LMM_se3 = NULL
LMM_p3 = NULL
LMM_coverage3 = matrix(nrow=1000, ncol=2)


#multiple imputation objects, multi level--Aux Level 2
implist_coef = NULL
implist_se = NULL
implist_coef2 = NULL
implist_se2 = NULL
imp_coverage = matrix(nrow=1000, ncol=2)
imp_pvalue = NULL

#multiple imputation objects, multi level--Aux Level 2
implist_coef3 = NULL
implist_se3 = NULL
implist_coef4 = NULL
implist_se4 = NULL
imp_coverage2 = matrix(nrow=1000, ncol=2)
imp_pvalue2 = NULL


#multiple imputation objects, multi level--Aux Level 2
implist_coef5=NULL
implist_se5 = NULL
implist_coef6=NULL
implist_se6 = NULL
imp_coverage3 = matrix(nrow=1000, ncol=2)
imp_pvalue3 = NULL


#multiple imputation objects, multi level--Aux Level 1
implist_coef7=NULL
implist_se7 = NULL
implist_coef8=NULL
implist_se8 = NULL
imp_coverage4 = matrix(nrow=1000, ncol=2)
imp_pvalue4 = NULL


#structural equation model (FIML)
OpenMx_coef = NULL
OpenMx_SE = NULL
#p value for SEM 
mxp = data.frame(pvalue1 = rep(2, length=1000), pvalue2 = rep(2, length=1000))
OpenMx_coverage = matrix(nrow=1000, ncol=2)

#structural equation model (FIML)--Aux level 2
OpenMx_coef2 = NULL
OpenMx_SE2 = NULL
#p value for SEM 
mxp2 = data.frame(pvalue1 = rep(2, length=1000), pvalue2 = rep(2, length=1000))
OpenMx_coverage2 = matrix(nrow=1000, ncol=2)

#structural equation model (FIML)--Aux Level 3
OpenMx_coef3 = NULL
OpenMx_SE3 = NULL
#p value for SEM 
mxp3 = data.frame(pvalue1 = rep(2, length=1000), pvalue2 = rep(2, length=1000))
OpenMx_coverage3 = matrix(nrow=1000, ncol=2)



###### LOOPS 1000 X, Missing completely at Random, 10 clusters #########
for (i in 1:1) {
  
  #creation of school-level data: 10 schools, with small mean shift for ICC to be around 5%
  #Also created each school to have around 60 kids, 600 total students 
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  #specified number of students (60/40/30/24) with a Poisson distribution
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  
  #this generates a sort of descriptive dataset at the cluster level
  #with number of clusters (10/15/20/25), nStudents per cluster, 
  #and s0 is same mean shift for each student within each school
  dtSchool <- genData(10, gen.school)
  

 
  
  #Adding treatment at the level of the cluster, 2 groups, randomly assigned to half the schools
  dtSchool <- trtAssign(dtSchool, n=2)
  #Creating DEIS variable at level of cluster, DEIS or NonDEIS, randomly assigned to half the schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  #generated full dataset, with number of students
  #clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  
  
  #Created second dataset for the outcome variable with 3 repeated measures
  #created random variable for Treatment*Time effect of 1 (Tr1)
  #for second time-point, created effect of 2 (Tr2)
  gen.student = defDataAdd(varname = "Tr1", dist = "normal", formula = 1, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Tr2", dist = "normal", formula = 1, variance = .25)
  
  
  #combined student/school dataset with above Treatment*Time effects
  dtStudent = addColumns(gen.student, dtStudent)

  
  #Specified correlation matrix for the repeated measures, based on CopeSmart data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  
  #specified means and standard deviations of outcomes, based on the baseline measure in CopeSmart
  #created dataset of 3 repeated measures, with no treatment or time effect, with length based on student information
  baselinedt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  

  #Adding the repeated measures to the student dataset, renaming "ESA" from CopeSmart data
  dtStudent <- cbind(dtStudent, ESA_T1 = baselinedt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = baselinedt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = baselinedt$V3)
  
  
  #Added the school mean shift to baseline
  #Added the Treatment*Time effect for those in the treatment group
  
  #ESA1 (baseline)
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 
  #ESA2 (first follow up)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$Tr1*dtStudent$trtGrp) 
  #ESA3 (second follow up)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr1*dtStudent$trtGrp) + (dtStudent$Tr2*dtStudent$trtGrp)
  
  
  #Reducing variables to just those that are needed
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS")]
  
  #Changing to long format
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  #recoding into numbers, which stay as as.factor so that linear mixed model analyses two slopes
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  #running linear mixed model to determine coefficients of full analysis set
  #specified a random intercept by school
  #specified compound symmetry covariance structure
  lmm_fas <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "ML")
 

  #recording results for this loop
  #coefficient
  TestSum = summary(lmm_fas)$tTable[5:6,1]
  #standard error
  TestSum2 = summary(lmm_fas)$tTable[5:6,2]
  #p-value
  TestSum3 = summary(lmm_fas)$tTable[5:6,5]
  
  #adding results back into the objects
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  Undeleted_SE = rbind(Undeleted_SE, TestSum2)
  Undeleted_p = rbind(Undeleted_p, TestSum3)
  


## getting coverage
#coverage here should be 1 for first slope and 2 for second
Coverage = c(1, 2)

#getting upper and lower bounds (95% confidence interval of slope)
Upper = TestSum + (TestSum2*1.96)
Lower = TestSum - (TestSum2 * 1.96)

#if 1 is contained in the confidence interval, True, else False
UnDel_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)



#################################################
#################################################
#Creating missingness
#################################################
#################################################


#No missingness in baseline for MNAR 
dtStudent1$ESA_1_20MNAR = dtStudent1$ESA_T1


#order small to large outcome values for time 2 
dtStudent1 = dtStudent1[order(ESA_T2, decreasing=F), ]
dtStudent1 = dtStudent1[order(trtGrp), ]


#create object length of control group, all 1s
Del = rep(1, length(dtStudent1$ESA_T1[dtStudent1$trtGrp==0]))

#create object length of intervention group, blank for now
Del1 = rep(NA, length(dtStudent1$ESA_T1[dtStudent1$trtGrp==1]))

#subsetting those in treatment group
x = subset(dtStudent1,trtGrp==1)


#creating probabilities ranging from .10 to .30, average is 0.20 or 20%
test2 = defData(varname = "w", dist = "uniform", formula = "10;30")
test1= genData(length(x$ESA_T2), test2)
test1 = test1[, "w"]
#dividing by 100 to get probabilities
test1=test1/100
#sorting small to large to make it that larger outcome values more likely to be noncompliant
test1 = sort(test1$w, decreasing=FALSE)
#v is the opposite probability, for the sample() code
v = 1-test1

dtStudent1$test1 = c(rep(0, length(Del)), test1)

#creating individual-level auxiliary variable, to be correlated with the missingness mechanism 
#(and therefore the outcome values)
#auxiliary variable only correlated with the treatment group, not the control group because no missingness
library(dplyr)
library(faux)
Aux = c(rnorm(length(Del), 0, 1), rnorm_pre(test1, mu=0, sd=1, r=0.9))
dtStudent1$Aux = Aux



#creating the cluster-level auxiliary variable
dtStudent1 = dtStudent1[order(dtStudent1$School),]
dtStudent1$Aux_Clust = rep(NA, length(dtStudent1$idChild))

#creating a matrix of the missingness probability per cluster, school number, and treatment group
MeanClust = matrix(nrow=10, ncol=3)
for(k in 1:10){
  MeanClust[k,1] = mean(dtStudent1$test1[dtStudent1$School==k], na.rm=T)
  MeanClust[k,2] = mean(dtStudent1$trtGrp[dtStudent1$School==k], na.rm=T)
  MeanClust[k,3] = mean(dtStudent1$School[dtStudent1$School==k], na.rm=T)
}

MeanClust = as.data.frame(MeanClust)

colnames(MeanClust) = c("Mean", "trtGrp", "School")
MeanClust = MeanClust[order(MeanClust$trtGrp),]


#Creating a vector of the cluster-level auxiliary variable
#First half of vector is control group, so just a normally distributed variable with mean of 0
#Second half is the treatment group, so the auxiliary variable is correlated with the mean probability
#for missingness for that cluster
AuxClust = c(rnorm(length(MeanClust$trtGrp[MeanClust$trtGrp==0]), 0, 1), rnorm_pre(MeanClust$Mean[MeanClust$trtGrp==1], mu=0, sd=1, r=0.9))

#setting this up as a dataset in order to be able to link back to the dtStudent1 data
AuxClust = as.data.frame(AuxClust)
AuxClust$School = MeanClust$School
AuxClust$trtGrp = MeanClust$trtGrp
AuxClust = AuxClust[order(AuxClust$School),]
AuxClust = AuxClust[order(AuxClust$trtGrp),]

dtStudent1 = dtStudent1[order(dtStudent1$School),]
dtStudent1 = dtStudent1[order(dtStudent1$trtGrp),]

#matching the cluster-level auxiliary variable back to the dtstudent1 data, by cluster
for(k in 1:10){
  dtStudent1$Aux_Clust[dtStudent1$School==k] = AuxClust[k,1]
}


detach(package:faux)
detach(package:dplyr)

#now back to introducing the missingness in Time 2 and 3 for the treatment group
dtStudent1 = dtStudent1[order(ESA_T2, decreasing=F), ]
dtStudent1 = dtStudent1[order(trtGrp), ]


#creating loop for each individual in the dataset in treatment group
#each person has their own probability of being noncompliant or not
for (j in 1:length(x$ESA_T2))
{
  Del1[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
}

#combining the Del object (all 1s) with the new Del1 object
dtStudent1$Del2 = c(Del, Del1)
#Creating new variable with missingness where Del=0
dtStudent1$ESA_2_20MNAR = ifelse(dtStudent1$Del2==0, NA, dtStudent1$ESA_T2) 


#order small to large outcome values for time 3 
dtStudent1 = dtStudent1[order(ESA_T3, decreasing=F), ]
dtStudent1 = dtStudent1[order(trtGrp), ]


#create object length of control group, all 1 (observed)
Del = rep(1, length(dtStudent1$ESA_T1[dtStudent1$trtGrp==0]))

#create object length of intervention group, blank for now
Del1 = rep(NA, length(dtStudent1$ESA_T1[dtStudent1$trtGrp==1]))

#subsetting those in treatment group
x = subset(dtStudent1,trtGrp==1)


#creating probabilities ranging from .10 to .30, average is 0.20 or 20%
test2 = defData(varname = "w", dist = "uniform", formula = "10;30")
test1= genData(length(x$ESA_T2), test2)
test1 = test1[, "w"]
#dividing by 100 to get probabilities
test1=test1/100
#sorting small to large to make it that larger outcome values more likely to be noncompliant
test1 = sort(test1$w, decreasing=FALSE)
#v is the opposite probability, for the sample() code
v = 1-test1



#creating loop for each individual in the dataset in treatment group
#each person has their own probability of being noncompliant or not
for (j in 1:length(x$ESA_T2))
{
  Del1[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
}

dtStudent1 = dtStudent1[order(ESA_T3, decreasing=F), ]
dtStudent1 = dtStudent1[order(trtGrp), ]
#combining the Del object (all 1s) with the new Del1 object
dtStudent1$Del2 = c(Del, Del1)
#Creating new variable with missingness where Del=0
dtStudent1$ESA_3_20MNAR = ifelse(dtStudent1$Del2==0, NA, dtStudent1$ESA_T3) 

dtStudent1 = dtStudent1[order(idChild, decreasing=F), ]





#creating new dataset of just new deleted variables, plus the two auxiliary variables
Data1 = dtStudent1[, c("School", "trtGrp", "idChild", "ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR", "DEIS", "Aux", "Aux_Clust")]






#creating a variable of "1"s for the imputation random intercept
Data1$constant=Data1$School/Data1$School

#creating an interaction term before imputation or SEM
Data1$trt_Aux = Data1$trtGrp* Data1$Aux


#changing to long format
Data1_long <- melt(Data1,
                   # ID variables - all the variables to keep but not split apart on
                   id.vars=c("School", "trtGrp", "idChild", "DEIS", "Aux", "Aux_Clust", "constant", "trt_Aux"),
                   # The source columns
                   measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR" ),
                   # Name of the destination column that will identify the original
                   # column that the measurement came from
                   variable.name="Time",
                   value.name="ESA_recid")

Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_20MNAR'=1")
Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_20MNAR'=2")
Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_20MNAR'=3")


#performing linear mixed on data with missingness, same model as Full analysis set
lmm_lmm <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
             random = list(School = ~1, idChild = ~1), 
             weights =varIdent(School),
             corr = corCompSymm(form= ~Time),
             data=Data1_long, na.action="na.omit",
             method = "ML",
             control=(msMaxIter=100000))

#getting coefficients
TestSum10L = summary(lmm_lmm)$tTable[5:6,1]
#getting standard errors
TestSum21L = summary(lmm_lmm)$tTable[5:6,2]
#getting p-values
TestSum31L = summary(lmm_lmm)$tTable[5:6,5]



#recording in objects
LMM_coef = rbind(LMM_coef, TestSum10L)
LMM_se = rbind(LMM_se, TestSum21L)
LMM_p = rbind(LMM_p, TestSum31L)


#getting 95% confidence intervals for coverage
Upper = TestSum10L + (TestSum21L*1.96)
Lower = TestSum10L - (TestSum21L * 1.96)

LMM_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)





#performing linear mixed on data with missingness, with Level 2 auxiliary variable
#adding it here (and all other analyses) as an interaction effect with treatment group and time
lmm_lmm2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time + Aux*trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "ML",
               control=(msMaxIter=100000))




#getting coefficients
TestSum10L = summary(lmm_lmm2)$tTable[6:7,1]
#getting standard errors
TestSum21L = summary(lmm_lmm2)$tTable[6:7,2]
#getting p-values
TestSum31L = summary(lmm_lmm2)$tTable[6:7,5]


#recording in objects
LMM_coef2 = rbind(LMM_coef2, TestSum10L)
LMM_se2 = rbind(LMM_se2, TestSum21L)
LMM_p2 = rbind(LMM_p2, TestSum31L)


#getting 95% confidence intervals for coverage
Upper = TestSum10L + (TestSum21L*1.96)
Lower = TestSum10L - (TestSum21L * 1.96)

LMM_coverage2[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)


#performing linear mixed on data with missingness, with Level 1 auxiliary variable
lmm_lmm3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time + Aux_Clust*trtGrp*Time, 
                random = list(School = ~1, idChild = ~1), 
                weights =varIdent(School),
                corr = corCompSymm(form= ~Time),
                data=Data1_long, na.action="na.omit",
                method = "ML",
                control=(msMaxIter=100000))



#getting coefficients
TestSum10L = summary(lmm_lmm3)$tTable[6:7,1]
#getting standard errors
TestSum21L = summary(lmm_lmm3)$tTable[6:7,2]
#getting p-values
TestSum31L = summary(lmm_lmm3)$tTable[6:7,5]


#recording in objects
LMM_coef3 = rbind(LMM_coef3, TestSum10L)
LMM_se3 = rbind(LMM_se3, TestSum21L)
LMM_p3 = rbind(LMM_p3, TestSum31L)


#getting 95% confidence intervals for coverage
Upper = TestSum10L + (TestSum21L*1.96)
Lower = TestSum10L - (TestSum21L * 1.96)

LMM_coverage3[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)






############################################
##### Multilevel multiple imputation ######
############################################

#creating blank imputation to be able to fill in predictor matrix (pred)
ini1 = mice(Data1, maxit=0)
pred = ini1$pred

#getting "method of imputation"
meth = ini1$meth

#filling in the method of imputation as "", which means do not impute these variables
meth[c("School", "trtGrp", "idChild", "DEIS", "constant", "Aux", "Aux_Clust", "ESA_1_20MNAR")] = ""

#filling in predictor matrix. -2 is the random intercept, 2 the column of 1s necessary for this
#all other predictors are the treatment group, Aux, Aux*TrtGrp, and the other 2 ESA variables

pred["ESA_2_20MNAR", ] = c(-2, 1, 0, 1, 0, 1, 0, 0, 0, 2, 0)
pred["ESA_3_20MNAR", ] = c(-2, 1, 0, 1, 1, 0, 0, 0, 0, 2, 0)
pred

#method to impute these variables is the multilevel MI for continuous variables
meth[c("ESA_2_20MNAR", "ESA_3_20MNAR")] = "2l.lmer"
meth

#running imputation model with 40 imputed datasets
imp = mice(Data1, meth=meth, pred=pred, maxit=100, m = 40, pri=F)

#selecting out the appropriate imputed datasets from the output of imp
com_MNAR20 = complete(imp, "long")

#Next, changing to further long format via "Time"
impMNAR20_long_01 <- melt(com_MNAR20,
                          # ID variables - all the variables to keep but not split apart on
                          id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Aux", "Aux_Clust", "constant"),
                          # The source columns
                          measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR" ),
                          # Name of the destination column that will identify the original
                          # column that the measurement came from
                          variable.name="Time",
                          value.name="ESA_Recid")

#Renaming values as 1, 2, and 3 in Time variable
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_1_20MNAR'=1")
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_2_20MNAR'=2")
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_3_20MNAR'=3")


#creating blank objects for next loop
implist_p = NULL
imp_trt1C = NULL
imp_trt1S = NULL
imp_trt2C = NULL
imp_trt2S = NULL


#creating loop to analyse each of the 40 imputed datasets and getting results from this
for (j in 1:40){
  impMNAR20_long_01$Time=as.factor(impMNAR20_long_01$Time)
  
  #linear mixed model for each imputed dataset
  lmm_mlmi <- lme(ESA_Recid ~ Time + trtGrp + trtGrp*Time, 
                  random = list(School = ~1, idChild = ~1), 
                  weights =varIdent(School),
                  corr = corCompSymm(form= ~Time),
               data=impMNAR20_long_01, subset=c(impMNAR20_long_01$.imp==j), na.action="na.omit",
               method = "ML",
               control=(msMaxIter=100000))  

  
  #getting p values
  implist_p = rbind(implist_p, summary(lmm_mlmi)$tTable[5:6,5])
  
  #getting coefficient and standard error
  imp_trt1C = rbind(imp_trt1C, summary(lmm_mlmi)$tTable[5,1])
  imp_trt1S = rbind(imp_trt1S, summary(lmm_mlmi)$tTable[5,2])
  
  imp_trt2C = rbind(imp_trt2C, summary(lmm_mlmi)$tTable[6,1])
  imp_trt2S = rbind(imp_trt2S, summary(lmm_mlmi)$tTable[6,2])
  
}



#using Rubins rules, pools results
combined.results2 <- mi.meld(q = imp_trt1C, se = imp_trt1S)
combined.results3 <- mi.meld(q = imp_trt2C, se = imp_trt2S)

combined.results2

#getting p-values
imp_p1 = NULL
imp_p2 = NULL

imp_p1=mean(implist_p[,1])
imp_p2=mean(implist_p[,2])
imp_p_final = cbind(imp_p1, imp_p2)



#combining the resuls with results of other simulates from the 1000 loop
#coef2 and se2 = first slope, coef3 and se3 = second slope
implist_coef=rbind(implist_coef, combined.results2$q.mi)
implist_se=rbind(implist_se, combined.results2$se.mi)
implist_coef2=rbind(implist_coef2, combined.results3$q.mi)
implist_se2=rbind(implist_se2, combined.results3$se.mi)
imp_pvalue=rbind(imp_pvalue, imp_p_final)

#getting coefficient and SEs to determine 95% confidence interval, for coverage
estimates = cbind(combined.results2$q.mi, combined.results3$q.mi)
ses = cbind(combined.results2$se.mi, combined.results3$se.mi)

Upper = estimates + (ses*1.96)
Lower = estimates - (ses * 1.96)

imp_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)







#creating blank imputation to be able to fill in predictor matrix (pred)
ini1 = mice(Data1, maxit=0)
pred = ini1$pred

#getting "method of imputation"
meth = ini1$meth

#filling in the method of imputation as "", which means do not impute these variables
meth[c("School", "trtGrp", "idChild", "DEIS", "constant", "Aux", "Aux_Clust", "ESA_1_20MNAR")] = ""

#filling in predictor matrix. -2 is the random intercept, 2 the column of 1s necessary for this
#all other predictors are the treatment group, Aux, Aux*TrtGrp, and the other 2 ESA variables

pred["ESA_2_20MNAR", ] = c(-2, 1, 0, 1, 0, 1, 0, 1, 0, 2, 1)
pred["ESA_3_20MNAR", ] = c(-2, 1, 0, 1, 1, 0, 0, 1, 0, 2, 1)

#method to impute these variables is the multilevel MI for continuous variables
meth[c("ESA_2_20MNAR", "ESA_3_20MNAR")] = "2l.lmer"
meth

#running imputation model with 40 imputed datasets
imp = mice(Data1, meth=meth, pred=pred, maxit=100, m = 40, pri=F)

#selecting out the appropriate imputed datasets from the output of imp
com_MNAR20 = complete(imp, "long")

#Next, changing to further long format via "Time"
impMNAR20_long_01 <- melt(com_MNAR20,
                          # ID variables - all the variables to keep but not split apart on
                          id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Aux", "Aux_Clust", "constant"),
                          # The source columns
                          measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR" ),
                          # Name of the destination column that will identify the original
                          # column that the measurement came from
                          variable.name="Time",
                          value.name="ESA_Recid")

#Renaming values as 1, 2, and 3 in Time variable
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_1_20MNAR'=1")
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_2_20MNAR'=2")
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_3_20MNAR'=3")


#creating blank objects for next loop
implist_p = NULL
imp_trt1C = NULL
imp_trt1S = NULL
imp_trt2C = NULL
imp_trt2S = NULL


#creating loop to analyse each of the 40 imputed datasets and getting results from this
for (j in 1:40){
  impMNAR20_long_01$Time=as.factor(impMNAR20_long_01$Time)
  
  #linear mixed model for each imputed dataset
  lmm_mlmi <- lme(ESA_Recid ~ Time + trtGrp + trtGrp*Time, 
                  random = list(School = ~1, idChild = ~1), 
                  weights =varIdent(School),
                  corr = corCompSymm(form= ~Time),
                  data=impMNAR20_long_01, subset=c(impMNAR20_long_01$.imp==j), na.action="na.omit",
                  method = "ML",
                  control=(msMaxIter=100000))  
  
  
  #getting p values
  implist_p = rbind(implist_p, summary(lmm_mlmi)$tTable[5:6,5])
  
  #getting coefficient and standard error
  imp_trt1C = rbind(imp_trt1C, summary(lmm_mlmi)$tTable[5,1])
  imp_trt1S = rbind(imp_trt1S, summary(lmm_mlmi)$tTable[5,2])
  
  imp_trt2C = rbind(imp_trt2C, summary(lmm_mlmi)$tTable[6,1])
  imp_trt2S = rbind(imp_trt2S, summary(lmm_mlmi)$tTable[6,2])
  
}



#using Rubins rules, pools results
combined.results2 <- mi.meld(q = imp_trt1C, se = imp_trt1S)
combined.results3 <- mi.meld(q = imp_trt2C, se = imp_trt2S)

combined.results2

#getting p-values
imp_p1 = NULL
imp_p2 = NULL

imp_p1=mean(implist_p[,1])
imp_p2=mean(implist_p[,2])
imp_p_final = cbind(imp_p1, imp_p2)



#combining the resuls with results of other simulates from the 1000 loop
#coef2 and se2 = first slope, coef3 and se3 = second slope
implist_coef5=rbind(implist_coef5, combined.results2$q.mi)
implist_se5=rbind(implist_se5, combined.results2$se.mi)
implist_coef6=rbind(implist_coef6, combined.results3$q.mi)
implist_se6=rbind(implist_se6, combined.results3$se.mi)
imp_pvalue2=rbind(imp_pvalue2, imp_p_final)

#getting coefficient and SEs to determine 95% confidence interval, for coverage
estimates = cbind(combined.results2$q.mi, combined.results3$q.mi)
ses = cbind(combined.results2$se.mi, combined.results3$se.mi)

Upper = estimates + (ses*1.96)
Lower = estimates - (ses * 1.96)

imp_coverage2[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)









Data5 = Data1
Data5$AuxCl_trt = Data5$trtGrp*Data5$Aux_Clust

#creating blank imputation to be able to fill in predictor matrix (pred)
ini1 = mice(Data5, maxit=0)
pred = ini1$pred

#getting "method of imputation"
meth = ini1$meth

#filling in the method of imputation as "", which means do not impute these variables
meth[c("School", "trtGrp", "idChild", "DEIS", "constant", "Aux", "Aux_Clust", "ESA_1_20MNAR")] = ""

#filling in predictor matrix. -2 is the random intercept, 2 the column of 1s necessary for this
#all other predictors are the treatment group, Cluster Aux, Cluster Aux by Treatment, and the other 2 ESA variables
pred["ESA_2_20MNAR", ] = c(-2, 1, 0, 1, 0, 1, 0, 0, 1, 2, 0, 1)
pred["ESA_3_20MNAR", ] = c(-2, 1, 0, 1, 1, 0, 0, 0, 1, 2, 0, 1)
pred

#method to impute these variables is the multilevel MI for continuous variables
meth[c("ESA_2_20MNAR", "ESA_3_20MNAR")] = "2l.lmer"

#running imputation model with 40 imputed datasets
imp = mice(Data5, meth=meth, pred=pred, maxit=100, m = 40, pri=F)

#selecting out the appropriate imputed datasets from the output of imp
com_MNAR20 = complete(imp, "long")

#Next, changing to further long format via "Time"
impMNAR20_long_01 <- melt(com_MNAR20,
                          # ID variables - all the variables to keep but not split apart on
                          id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Aux", "Aux_Clust", "constant"),
                          # The source columns
                          measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR" ),
                          # Name of the destination column that will identify the original
                          # column that the measurement came from
                          variable.name="Time",
                          value.name="ESA_Recid")

#Renaming values as 1, 2, and 3 in Time variable
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_1_20MNAR'=1")
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_2_20MNAR'=2")
impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_3_20MNAR'=3")


#creating blank objects for next loop
implist_p = NULL
imp_trt1C = NULL
imp_trt1S = NULL
imp_trt2C = NULL
imp_trt2S = NULL


#creating loop to analyse each of the 40 imputed datasets and getting results from this
for (j in 1:40){
  impMNAR20_long_01$Time=as.factor(impMNAR20_long_01$Time)
  
  #linear mixed model for each imputed dataset
  lmm_mlmi <- lme(ESA_Recid ~ Time + trtGrp + trtGrp*Time, 
                  random = list(School = ~1, idChild = ~1), 
                  weights =varIdent(School),
                  corr = corCompSymm(form= ~Time),
                  data=impMNAR20_long_01, subset=c(impMNAR20_long_01$.imp==j), na.action="na.omit",
                  method = "ML",
                  control=(msMaxIter=100000))  
  
  #getting p values
  implist_p = rbind(implist_p, summary(lmm_mlmi)$tTable[5:6,5])
  
  #getting coefficient and standard error
  imp_trt1C = rbind(imp_trt1C, summary(lmm_mlmi)$tTable[5,1])
  imp_trt1S = rbind(imp_trt1S, summary(lmm_mlmi)$tTable[5,2])
  
  imp_trt2C = rbind(imp_trt2C, summary(lmm_mlmi)$tTable[6,1])
  imp_trt2S = rbind(imp_trt2S, summary(lmm_mlmi)$tTable[6,2])
  
}


#using Rubins rules, pools results
combined.results2 <- mi.meld(q = imp_trt1C, se = imp_trt1S)
combined.results3 <- mi.meld(q = imp_trt2C, se = imp_trt2S)

#getting p-values
imp_p1 = NULL
imp_p2 = NULL

imp_p1=mean(implist_p[,1])
imp_p2=mean(implist_p[,2])
imp_p_final = cbind(imp_p1, imp_p2)



#combining the resuls with results of other simulates from the 1000 loop
#coef2 and se2 = first slope, coef3 and se3 = second slope
implist_coef7=rbind(implist_coef7, combined.results2$q.mi)
implist_se7=rbind(implist_se7, combined.results2$se.mi)
implist_coef8=rbind(implist_coef8, combined.results3$q.mi)
implist_se8=rbind(implist_se8, combined.results3$se.mi)
imp_pvalue4=rbind(imp_pvalue4, imp_p_final)

#getting coefficient and SEs to determine 95% confidence interval, for coverage
estimates = cbind(combined.results2$q.mi, combined.results3$q.mi)
ses = cbind(combined.results2$se.mi, combined.results3$se.mi)

Upper = estimates + (ses*1.96)
Lower = estimates - (ses * 1.96)

imp_coverage4[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)







##################################################
######## Structural Equation Model ##########
##################################################

#setting up the school level, latent intercept per school
perSchool <- mxModel(
  "perSchool", type="RAM", latentVars=c("school"),
  mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
  mxPath("school", arrows=2, values=1) #arrows=2 means variance
)


#setting up remainder of model, using raw data
#manifestVars = observed variables
#latentVars = new latent variables

SingleLevelModel1= mxModel(
  "One Level Model", type="RAM", manifestVars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR", 
                                                "trtGrp"), perSchool,
  latentVars=c("I", "S", "S2"), #I = intercept, S= slope 1, S2 = slope 2
  mxData(observed=Data1, type="raw"),
  #compound symmetry covariance structure, free=T means to be estimated
  #since called them the same thing (variance and covar), will estimate one variance and one covariance
  mxPath(from=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(1,1,1),
         labels=c("variance", "variance", "variance")),
  mxPath(from="ESA_1_20MNAR", to=c("ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(.5, .5),
         labels=c("covar", "covar")),
  mxPath(from="ESA_2_20MNAR", to="ESA_3_20MNAR", arrows=2, free=T, values=.5, labels="covar"),
  #two slopes (S, S2) for T1 to T2, and T1 to T3
  #Free=F meaning not estimated, covariance between them set to 0
  mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
  
  #for a latent growth curve, to estimate the latent intercept, you have to set all the values to 1 
  #from each of the manifest variables
  mxPath(from="I", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
         values=c(1, 1, 1)),
  
  #and for a latent growth curve, to estimate the slope, observed baseline has to be set to 0 and next observed measurement 
  #has to be set to 1
  mxPath(from="S", to=c("ESA_1_20MNAR", "ESA_2_20MNAR"), arrows=1, free=F, 
         values=c(0, 1)),
  mxPath(from="S2", to=c("ESA_1_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
         values=c(0, 1)),
  
  #have to specify a "one" to the observed (manifest) variables to represent manifest means
  #manifest means not estimated here, so values set to 0
  mxPath(from="one", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F,
         values=c(0, 0, 0)),
  
  # Account for per-School intercepts
  mxPath('perSchool.school', to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"),
         values=1, free=FALSE, joinKey='School'),
  #latent means estimated
  mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
         labels=c("meanI", "meanS", "meanS2")),
  #path from treatment group to the latent variables
  mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
         labels=c("ireg", "Sreg", "S2reg")),
  #mean not estimated for treatment group
  mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
  #variance not estimated for trtGrp
  mxPath(from="trtGrp", arrows=2, free=F, values=1))


#now that specified the model, have to run it using mxRun
SingleLevelModel1 = mxRun(SingleLevelModel1)
SingleLevelModel1$output$estimate



#getting the coefficients from the output
OpenMx_stats2 = SingleLevelModel1$output$estimate[c(2,3)]
OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats2)

#getting the standard errors from the output
OpenMx_stats3 = SingleLevelModel1$output$standardErrors[c(2,3)]
OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats3)



#getting p values for each coefficient
m1 = mxModel(SingleLevelModel1, mxCI(c("Sreg", "S2reg"))) # list the things you want CIs for.
m1 = mxRun(m1, intervals= T)
x = m1$output$confidenceIntervals

#if confidence interval does not contain zero, is significant
#for this, 1 = significant p value, 0 = nonsignificant p value
mxp[i,1] = ifelse(x[1,1] > 0, 1, 
                   ifelse(x[1,3] < 0, 1, 0))

mxp[i,2] = ifelse(x[2,1] > 0, 1, 
                   ifelse(x[2,3] < 0, 1, 0))

#getting 95% CI for coverage
Upper = OpenMx_stats2 + (OpenMx_stats3*1.96)
Lower = OpenMx_stats2 - (OpenMx_stats3*1.96)

OpenMx_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)












##################################################
######## Structural Equation Model ##########
##################################################

#setting up the school level, latent intercept per school
perSchool <- mxModel(
  "perSchool", type="RAM", latentVars=c("school"),
  mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
  mxPath("school", arrows=2, values=1) #arrows=2 means variance
)


#setting up remainder of model, using raw data
#manifestVars = observed variables
#latentVars = new latent variables

#this model includes the individual-level auxiliary variable
#added as a manifest treatment*Aux variable, with paths onto the latent variables

SingleLevelModel2= mxModel(
  "One Level Model", type="RAM", manifestVars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR", 
                                                "trtGrp", "trt_Aux"), perSchool,
  latentVars=c("I", "S", "S2"), #I = intercept, S= slope 1, S2 = slope 2
  mxData(observed=Data1, type="raw"),
  #compound symmetry covariance structure, free=T means to be estimated
  #since called them the same thing (variance and covar), will estimate one variance and one covariance
  mxPath(from=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(1,1,1),
         labels=c("variance", "variance", "variance")),
  mxPath(from="ESA_1_20MNAR", to=c("ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(.5, .5),
         labels=c("covar", "covar")),
  mxPath(from="ESA_2_20MNAR", to="ESA_3_20MNAR", arrows=2, free=T, values=.5, labels="covar"),
  #two slopes (S, S2) for T1 to T2, and T1 to T3
  #Free=F meaning not estimated, covariance between them set to 0
  mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
  
  #for a latent growth curve, to estimate the latent intercept, you have to set all the values to 1 
  #from each of the manifest variables
  mxPath(from="I", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
         values=c(1, 1, 1)),
  
  #and for a latent growth curve, to estimate the slope, observed baseline has to be set to 0 and next observed measurement 
  #has to be set to 1
  mxPath(from="S", to=c("ESA_1_20MNAR", "ESA_2_20MNAR"), arrows=1, free=F, 
         values=c(0, 1)),
  mxPath(from="S2", to=c("ESA_1_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
         values=c(0, 1)),
  
  #have to specify a "one" to the observed (manifest) variables to represent manifest means
  #manifest means not estimated here, so values set to 0
  mxPath(from="one", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F,
         values=c(0, 0, 0)),
  
  # Account for per-School intercepts
  mxPath('perSchool.school', to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"),
         values=1, free=FALSE, joinKey='School'),
  #latent means estimated
  mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
         labels=c("meanI", "meanS", "meanS2")),
  #path from treatment group to the latent variables
  mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
         labels=c("ireg", "Sreg", "S2reg")),
  #mean not estimated for treatment group
  mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
  #variance not estimated for trtGrp
  mxPath(from="trtGrp", arrows=2, free=F, values=1),
  #Level 2 auxiliary variale
  mxPath(from="trt_Aux", to=c("I", "S"), arrows=1, free=TRUE, values=c(0.5, 0.5)),
  #mean not estimated for treatment group
  mxPath(from="one", to="trt_Aux", arrows=1, free=F, values=0),
  #variance not estimated for trtGrp
  mxPath(from="trt_Aux", arrows=2, free=F, values=1))


#now that specified the model, have to run it using mxRun
SingleLevelModel2 = mxRun(SingleLevelModel2)


#getting the coefficients from the output
OpenMx_stats2 = SingleLevelModel2$output$estimate[c(2,3)]
OpenMx_coef2 = rbind(OpenMx_coef2, OpenMx_stats2)

#getting the standard errors from the output
OpenMx_stats3 = SingleLevelModel2$output$standardErrors[c(2,3)]
OpenMx_SE2 = rbind(OpenMx_SE2, OpenMx_stats3)


#getting p values for each coefficient
m1 = mxModel(SingleLevelModel2, mxCI(c("Sreg", "S2reg"))) # list the things you want CIs for.
m1 = mxRun(m1, intervals= T)
x = m1$output$confidenceIntervals

#if confidence interval does not contain zero, is significant
#for this, 1 = significant p value, 0 = nonsignificant p value
mxp2[i,1] = ifelse(x[1,1] > 0, 1, 
                   ifelse(x[1,3] < 0, 1, 0))

mxp2[i,2] = ifelse(x[2,1] > 0, 1, 
                   ifelse(x[2,3] < 0, 1, 0))
#getting 95% CI for coverage
Upper = OpenMx_stats2 + (OpenMx_stats3*1.96)
Lower = OpenMx_stats2 - (OpenMx_stats3*1.96)

OpenMx_coverage2[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)








#creating interaction variable so that a treatment*time*auxiliary effect can be included
Data1$trt_AuxClust = Data1$Aux_Clust*Data1$trtGrp



##################################################
######## Structural Equation Model ##########
##################################################

#setting up the school level, latent intercept per school
perSchool <- mxModel(
  "perSchool", type="RAM", latentVars=c("school"),
  mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
  mxPath("school", arrows=2, values=1) #arrows=2 means variance
)


#setting up remainder of model, using raw data
#manifestVars = observed variables
#latentVars = new latent variables

#this model includes the cluster-level auxiliary variable
#added as a manifest treatment*clusterAux variable, with paths onto the latent variables

SingleLevelModel3= mxModel(
  "One Level Model", type="RAM", manifestVars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR", 
                                                "trtGrp", "trt_AuxClust"), perSchool,
  latentVars=c("I", "S", "S2"), #I = intercept, S= slope 1, S2 = slope 2
  mxData(observed=Data1, type="raw"),
  #compound symmetry covariance structure, free=T means to be estimated
  #since called them the same thing (variance and covar), will estimate one variance and one covariance
  mxPath(from=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(1,1,1),
         labels=c("variance", "variance", "variance")),
  mxPath(from="ESA_1_20MNAR", to=c("ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(.5, .5),
         labels=c("covar", "covar")),
  mxPath(from="ESA_2_20MNAR", to="ESA_3_20MNAR", arrows=2, free=T, values=.5, labels="covar"),
  #two slopes (S, S2) for T1 to T2, and T1 to T3
  #Free=F meaning not estimated, covariance between them set to 0
  mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
  
  #for a latent growth curve, to estimate the latent intercept, you have to set all the values to 1 
  #from each of the manifest variables
  mxPath(from="I", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
         values=c(1, 1, 1)),
  
  #and for a latent growth curve, to estimate the slope, observed baseline has to be set to 0 and next observed measurement 
  #has to be set to 1
  mxPath(from="S", to=c("ESA_1_20MNAR", "ESA_2_20MNAR"), arrows=1, free=F, 
         values=c(0, 1)),
  mxPath(from="S2", to=c("ESA_1_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
         values=c(0, 1)),
  
  #have to specify a "one" to the observed (manifest) variables to represent manifest means
  #manifest means not estimated here, so values set to 0
  mxPath(from="one", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F,
         values=c(0, 0, 0)),
  
  # Account for per-School intercepts
  mxPath('perSchool.school', to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"),
         values=1, free=FALSE, joinKey='School'),
  #latent means estimated
  mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
         labels=c("meanI", "meanS", "meanS2")),
  #path from treatment group to the latent variables
  mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
         labels=c("ireg", "Sreg", "S2reg")),
  #mean not estimated for treatment group
  mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
  #variance not estimated for trtGrp
  mxPath(from="trtGrp", arrows=2, free=F, values=1),
  #Level 2 auxiliary variale
  mxPath(from="trt_AuxClust", to=c("S", "I"), arrows=1, free=TRUE, values=c(0.5, 0.5)),
  #mean not estimated for treatment group
  mxPath(from="one", to="trt_AuxClust", arrows=1, free=F, values=0),
  #variance not estimated for trtGrp
  mxPath(from="trt_AuxClust", arrows=2, free=F, values=1))


#now that specified the model, have to run it using mxRun
SingleLevelModel3 = mxRun(SingleLevelModel3)


#getting the coefficients from the output
OpenMx_stats2 = SingleLevelModel3$output$estimate[c(2,3)]
OpenMx_coef3 = rbind(OpenMx_coef3, OpenMx_stats2)

#getting the standard errors from the output
OpenMx_stats3 = SingleLevelModel3$output$standardErrors[c(2,3)]
OpenMx_SE3 = rbind(OpenMx_SE3, OpenMx_stats3)


#getting p values for each coefficient
m1 = mxModel(SingleLevelModel3, mxCI(c("Sreg", "S2reg"))) # list the things you want CIs for.
m1 = mxRun(m1, intervals= T)
x = m1$output$confidenceIntervals

#if confidence interval does not contain zero, is significant
#for this, 1 = significant p value, 0 = nonsignificant p value
mxp3[i,1] = ifelse(x[1,1] > 0, 1, 
                  ifelse(x[1,3] < 0, 1, 0))

mxp3[i,2] = ifelse(x[2,1] > 0, 1, 
                  ifelse(x[2,3] < 0, 1, 0))

#getting 95% CI for coverage
Upper = OpenMx_stats2 + (OpenMx_stats3*1.96)
Lower = OpenMx_stats2 - (OpenMx_stats3*1.96)

OpenMx_coverage3[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)




print(i)

}






#######################################################
########### FINISHED!!! ##############################
#######################################################

#getting results from full analysis set
#Mean of coefficient and standard error over the 1000 simulates
#and number of simulates with p-values nonsignificant (Type II error)

#SLOPE 1
Trt1_effect=c(mean(Undeleted_coef[,1]), mean(Undeleted_SE[,1]), length(which(Undeleted_p[,1] > 0.05)))
Trt1_effect

## SLOPE 2
Trt2_effect=c(mean(Undeleted_coef[,2]), mean(Undeleted_SE[,2]), length(which(Undeleted_p[,2] > 0.05)))
Trt2_effect


#getting same results for all analyses
## First slope/effect bias


#SEM results
OpenMx_bias1 = c((Trt1_effect[1]-mean(OpenMx_coef[,1])), (Trt1_effect[2]-mean(OpenMx_SE[,1])), (length(which(mxp[,1]==0))))
OpenMx_bias1

OpenMx_bias2 = c((Trt1_effect[1]-mean(OpenMx_coef2[,1])), (Trt1_effect[2]-mean(OpenMx_SE2[,1])), (length(which(mxp2[,1]==0))))
OpenMx_bias2

OpenMx_bias3 = c((Trt1_effect[1]-mean(OpenMx_coef3[,1])), (Trt1_effect[2]-mean(OpenMx_SE3[,1])), (length(which(mxp3[,1]==0))))
OpenMx_bias3

#multiple imputation results, single level
imp_bias1 = c((Trt1_effect[1]-mean(implist_coef)), (Trt1_effect[2]-mean(implist_se)), length(which(imp_pvalue[,1] > 0.05)))
imp_bias1

#multiple imputation results, multilevel level
imp_bias3 = c((Trt1_effect[1]-mean(implist_coef5)), (Trt1_effect[2]-mean(implist_se5)), length(which(imp_pvalue3[,1] > 0.05)))
imp_bias3

#multiple imputation results, multilevel level
imp_bias4 = c((Trt1_effect[1]-mean(implist_coef7)), (Trt1_effect[2]-mean(implist_se7)), length(which(imp_pvalue4[,1] > 0.05)))
imp_bias4

#linear mixed model results
LMM_bias1 = c((Trt1_effect[1]-mean(LMM_coef[,1])), (Trt1_effect[2]-mean(LMM_se[,1])), length(which(LMM_p[,1] > 0.05)))
LMM_bias1

#linear mixed model results
LMM_bias2 = c((Trt1_effect[1]-mean(LMM_coef2[,1])), (Trt1_effect[2]-mean(LMM_se2[,1])), length(which(LMM_p2[,1] > 0.05)))
LMM_bias2

#linear mixed model results
LMM_bias3 = c((Trt1_effect[1]-mean(LMM_coef3[,1])), (Trt1_effect[2]-mean(LMM_se3[,1])), length(which(LMM_p3[,1] > 0.05)))
LMM_bias3






#combining all results
Slope1Bias = rbind(Trt1_effect, LMM_bias1, LMM_bias2, LMM_bias3, OpenMx_bias1, OpenMx_bias2,
                   OpenMx_bias3, imp_bias1, imp_bias3, imp_bias4)

Total = Slope1Bias
Total = as.data.frame(Total)



### Getting all the monte carlo standard errors

## OpenMx Coefficient MCSEs
z = rep(mean(OpenMx_coef[,1]), times=1000)
MCSE_MxCoef_1 = sqrt(sum((OpenMx_coef[,1] - z)^2)/999000)
z = rep(mean(OpenMx_coef[,2]), times=1000)
MCSE_MxCoef_2 = sqrt(sum((OpenMx_coef[,2] - z)^2)/999000)

## SEs SEs
z = rep(mean(OpenMx_SE[,1]), times=1000)
MCSE_MxSE_1 = sqrt(sum((OpenMx_SE[,1] - z)^2)/999000)
z = rep(mean(OpenMx_SE[,2]), times=1000)
MCSE_MxSE_2 = sqrt(sum((OpenMx_SE[,2] - z)^2)/999000)


## OpenMx Coefficient MCSEs
z = rep(mean(OpenMx_coef2[,1]), times=1000)
MCSE_MxCoef_2 = sqrt(sum((OpenMx_coef2[,1] - z)^2)/999000)

## SEs SEs
z = rep(mean(OpenMx_SE2[,1]), times=1000)
MCSE_MxSE_2 = sqrt(sum((OpenMx_SE2[,1] - z)^2)/999000)


## OpenMx Coefficient MCSEs
z = rep(mean(OpenMx_coef3[,1]), times=1000)
MCSE_MxCoef_3 = sqrt(sum((OpenMx_coef3[,1] - z)^2)/999000)

## SEs SEs
z = rep(mean(OpenMx_SE3[,1]), times=1000)
MCSE_MxSE_3 = sqrt(sum((OpenMx_SE3[,1] - z)^2)/999000)




## LMM Coefficient MCSEs
z = rep(mean(LMM_coef[,1]), times=1000)
MCSE_LMMCoef_1 = sqrt(sum((LMM_coef[,1] - z)^2)/999000)
z = rep(mean(LMM_coef[,2]), times=1000)
MCSE_LMMCoef_2 = sqrt(sum((LMM_coef[,2] - z)^2)/999000)

## SEs SEs
z = rep(mean(LMM_se[,1]), times=1000)
MCSE_LMMSE_1 = sqrt(sum((LMM_se[,1] - z)^2)/999000)
z = rep(mean(LMM_se[,2]), times=1000)
MCSE_LMMSE_2 = sqrt(sum((LMM_se[,2] - z)^2)/999000)


## LMM2 Coefficient MCSEs
z = rep(mean(LMM_coef2[,1]), times=1000)
MCSE_LMMCoef_3 = sqrt(sum((LMM_coef2[,1] - z)^2)/999000)
z = rep(mean(LMM_coef2[,2]), times=1000)
MCSE_LMMCoef_4 = sqrt(sum((LMM_coef2[,2] - z)^2)/999000)

## SEs2 SEs
z = rep(mean(LMM_se2[,1]), times=1000)
MCSE_LMMSE_3 = sqrt(sum((LMM_se2[,1] - z)^2)/999000)
z = rep(mean(LMM_se2[,2]), times=1000)
MCSE_LMMSE_4 = sqrt(sum((LMM_se2[,2] - z)^2)/999000)




## LMM3 Coefficient MCSEs
z = rep(mean(LMM_coef3[,1]), times=1000)
MCSE_LMMCoef_5 = sqrt(sum((LMM_coef3[,1] - z)^2)/999000)
z = rep(mean(LMM_coef3[,2]), times=1000)
MCSE_LMMCoef_6 = sqrt(sum((LMM_coef3[,2] - z)^2)/999000)

## SEs3 SEs
z = rep(mean(LMM_se3[,1]), times=1000)
MCSE_LMMSE_5 = sqrt(sum((LMM_se3[,1] - z)^2)/999000)
z = rep(mean(LMM_se3[,2]), times=1000)
MCSE_LMMSE_6 = sqrt(sum((LMM_se3[,2] - z)^2)/999000)



## imputation Coefficient MCSEs
z = rep(mean(implist_coef), times=1000)
MCSE_impCoefML_1 = sqrt(sum((implist_coef - z)^2)/999000)
z = rep(mean(implist_coef2), times=1000)
MCSE_impCoefML_2 = sqrt(sum((implist_coef2 - z)^2)/999000)

## SEs SEs
z = rep(mean(implist_se), times=1000)
MCSE_impSEML_1 = sqrt(sum((implist_se - z)^2)/999000)
z = rep(mean(implist_se2), times=1000)
MCSE_impSEML_2 = sqrt(sum((implist_se2 - z)^2)/999000)



## imputation Coefficient MCSEs
z = rep(mean(implist_coef5), times=1000)
MCSE_impCoefML_3 = sqrt(sum((implist_coef5 - z)^2)/999000)
z = rep(mean(implist_coef6), times=1000)
MCSE_impCoefML_4 = sqrt(sum((implist_coef6 - z)^2)/999000)

## SEs SEs
z = rep(mean(implist_se5), times=1000)
MCSE_impSEML_3 = sqrt(sum((implist_se5 - z)^2)/999000)
z = rep(mean(implist_se6), times=1000)
MCSE_impSEML_4 = sqrt(sum((implist_se6 - z)^2)/999000)


## imputation2 Coefficient MCSEs
z = rep(mean(implist_coef7), times=1000)
MCSE_impCoefML_5 = sqrt(sum((implist_coef7 - z)^2)/999000)
z = rep(mean(implist_coef8), times=1000)
MCSE_impCoefML_6 = sqrt(sum((implist_coef8 - z)^2)/999000)

## SEs2 SEs
z = rep(mean(implist_se7), times=1000)
MCSE_impSEML_5 = sqrt(sum((implist_se7 - z)^2)/999000)
z = rep(mean(implist_se8), times=1000)
MCSE_impSEML_6 = sqrt(sum((implist_se8 - z)^2)/999000)



#adding column of monte carlo standard errors for the coefficient and SE estimates
Total$MC_SE_Coef = c(NA, MCSE_LMMCoef_1, MCSE_LMMCoef_3, MCSE_LMMCoef_5, MCSE_MxCoef_1, MCSE_MxCoef_2, MCSE_MxCoef_3, MCSE_impCoefML_1, MCSE_impCoefML_3, MCSE_impCoefML_5)
Total$MC_SE_SEs = c(NA, MCSE_LMMSE_1, MCSE_LMMSE_3, MCSE_LMMSE_5, MCSE_MxSE_1, MCSE_MxSE_2, MCSE_MxSE_3, MCSE_impSEML_1, MCSE_impSEML_3, MCSE_impSEML_5)





#adding coverage for each method
Total$Coverage = c(
  prop.table(table(UnDel_coverage[1:1000,1]))[2],
  prop.table(table(LMM_coverage[1:1000,1]))[2],
  prop.table(table(LMM_coverage2[1:1000,1]))[2],
  prop.table(table(LMM_coverage3[1:1000,1]))[2],
  prop.table(table(OpenMx_coverage[1:1000,1]))[2],
  prop.table(table(OpenMx_coverage2[1:1000,1]))[2],
  prop.table(table(OpenMx_coverage3[1:1000,1]))[2],
  prop.table(table(imp_coverage1[1:1000,1]))[2],
  prop.table(table(imp_coverage3[1:1000,1]))[2],
  prop.table(table(imp_coverage4[1:1000,1]))[2],
)

Total


#export as excel. Named with number of clusters, mechanism, and percent missing
WriteXLS(Total, ExcelFileName = "CopeSmart_10_MNAR1000_20_Aux.xlsx", col.names = TRUE, row.names=TRUE)





