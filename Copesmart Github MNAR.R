

##creating empty objects to fill in from loops: coefficient, standard error, p-value, and coverage

#this is for full analysis set
Undeleted_coef = NULL
Undeleted_SE = NULL
Undeleted_p = NULL
UnDel_coverage = matrix(nrow=10000, ncol=2)


#linear mixed model
LMM_coef=NULL
LMM_se = NULL
LMM_p = NULL
LMM_coverage = matrix(nrow=10000, ncol=2)


#complete case analysis
CCA_coef=NULL
CCA_se = NULL
CCA_p = NULL
CCA_coverage = matrix(nrow=10000, ncol=2)


#multiple imputation objects, single level
implist_coef1=NULL
implist_se1 = NULL
implist_coef2=NULL
implist_se2 = NULL
imp_pvalue1=NULL
imp_coverage1 = matrix(nrow=10000, ncol=2)


#multiple imputation objects, multi level
implist_coef3=NULL
implist_se3 = NULL
implist_coef4=NULL
implist_se4 = NULL
imp_coverage2 = matrix(nrow=10000, ncol=2)
imp_pvalue2=NULL


#structural equation model (FIML)
OpenMx_coef = NULL
OpenMx_SE = NULL
#p value for SEM 
mxp = data.frame(pvalue1 = rep(2, length=1000), pvalue2 = rep(2, length=1000))
OpenMx_coverage = matrix(nrow=10000, ncol=2)


#last observation carried forward
LOCF_coef=NULL
LOCF_se = NULL
LOCF_p = NULL
LOCF_coverage = matrix(nrow=10000, ncol=2)


#mean imputation
Mean_coef=NULL
Mean_se = NULL
Mean_p = NULL
Mean_coverage = matrix(nrow=10000, ncol=2)


#Clustered mean imputation
MeanClus_coef=NULL
MeanClus_se = NULL
MeanClus_p = NULL
MeanClus_coverage = matrix(nrow=10000, ncol=2)




###### LOOPS 1000 X, Missing completely at Random, 10 clusters #########
for (i in 1:1000) {
  
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
  gen.student = defDataAdd(gen.student, varname = "Tr2", dist = "normal", formula = 2, variance = .25)
  
  
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
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr2*dtStudent$trtGrp) 
  
  
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
              method = "REML")

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

#order small to large outcome values for time 2 
dtStudent1 = dtStudent1[order(ESA_T1), ]
nlength = length(dtStudent1$ESA_T1)

#create blank "Del" object, length of dataset
Del = rep(NA, nlength)

#creating uniform variable that is length of dataset, 0.0 to 0.40 for average of .2
#this represents the probability of missingness, or 20% missingness on average, ranging from 0.10 to 0.30
#for 10% missingness, would be 0.05 to 0.15; for 40% missing would be 0.20 to 0.60
test2 = defData(varname = "w", dist = "uniform", formula = "10;30")
test1= genData(nlength, test2)
test1 = test1[, "w"]


#dividing by 100 to get to probabilities
test1=test1/100
#sorting small to low, to match up with ESA2 above
test1 = sort(test1$w, decreasing=FALSE)
#creating opposite probability
v = 1-test1

#this is created so that it is MNAR, with larger ESA_T2 values corresponds to 
#larger probability of missingness
for (j in 1:nlength)
{
  Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
}


#Creating new variable with missingness where Del=0
dtStudent1$ESA_1_20MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T1) 


#performing same for ESA_T2
dtStudent1 = dtStudent1[order(ESA_T2), ]


for (j in 1:nlength)
{
  Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
}

dtStudent1$ESA_2_20MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T2)  



#performing same for ESA_T3
dtStudent1 = dtStudent1[order(ESA_T3), ]

for (j in 1:nlength)
{
  Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
}

dtStudent1$ESA_3_20MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T3)  # doesn't show up when larger value

#re-ordering by id number
dtStudent1 = dtStudent1[order(idChild), ]



#creating new dataset of just new deleted variables
Data1 = dtStudent1[, c("School", "trtGrp", "idChild", "ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR", "DEIS")]

#creating a variable of "1"s for the imputation random intercept
Data1$constant=Data1$School/Data1$School


#changing to long format
Data1_long <- melt(Data1,
                   # ID variables - all the variables to keep but not split apart on
                   id.vars=c("School", "trtGrp", "idChild", "DEIS", "constant"),
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
             method = "REML",
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



###################################
##### complete case analysis #####
###################################

#forcing observations out with any missingness
Data2 = na.omit(Data1)

#changing to long format
Data2_long <- melt(Data2,
                   # ID variables - all the variables to keep but not split apart on
                   id.vars=c("School", "trtGrp", "idChild", "DEIS", "constant"),
                   # The source columns
                   measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR" ),
                   # Name of the destination column that will identify the original
                   # column that the measurement came from
                   variable.name="Time",
                   value.name="ESA_recid")

Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_20MNAR'=1")
Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_20MNAR'=2")
Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_20MNAR'=3")


#linear mixed model on complete case analysis data
lmm_cca <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
             random = list(School = ~1, idChild = ~1), 
             weights =varIdent(School),
             corr = corCompSymm(form= ~Time),
             data=Data2_long, na.action="na.omit",
             method = "REML",
             control=(msMaxIter=100000))

#recording coefficient, standard error, p value
TestSum1 = summary(lmm_cca)$tTable[5:6,1]
TestSum2 = summary(lmm_cca)$tTable[5:6,2]
TestSum3 = summary(lmm_cca)$tTable[5:6,5]

CCA_coef = rbind(CCA_coef, TestSum1)
CCA_se = rbind(CCA_se, TestSum2)
CCA_p = rbind(CCA_p, TestSum3)

#getting 95% confidence interval for coverage
Upper = TestSum1 + (TestSum2*1.96)
Lower = TestSum1 - (TestSum2 * 1.96)


CCA_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)











############################################
##### Single level multiple imputation #####
############################################

#creating blank imputation to be able to fill in predictor matrix (pred)
ini1 = mice(Data1, maxit=0)
pred = ini1$pred

#getting "method of imputation"
meth = ini1$meth

#filling in the method of imputation as "", which means do not impute these variables
meth[c("School", "trtGrp", "idChild", "DEIS", "constant")] = ""

#filling in predictor matrix. 1 = predictor, 0 = not a predictor
#The predictors are the treatment group, DEIS status, and the other 2 ESA variables
pred["ESA_1_20MNAR", ] = c(0, 1, 0, 0, 1, 1, 1, 0)
pred["ESA_2_20MNAR", ] = c(0, 1, 0, 1, 0, 1, 1, 0)
pred["ESA_3_20MNAR", ] = c(0, 1, 0, 1, 1, 0, 1, 0)
pred

#method to impute these variables is for continuous variables. Doesnt take clustering into account
meth[c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR")] = "norm"

#running imputation model with 40 imputed datasets
imp = mice(Data1, meth=meth, pred=pred, maxit=10, m = 40, pri=F)

#selecting out the appropriate imputed datasets from the output of imp
com_MNAR20 = complete(imp, "long")

#Next, changing to further long format via "Time"
impMNAR20_long_01 <- melt(com_MNAR20,
                          # ID variables - all the variables to keep but not split apart on
                          id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "constant"),
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
  lmm_mi <- lme(ESA_Recid ~ Time + trtGrp + trtGrp*Time, 
                random = list(idChild = ~1), 
                corr = corCompSymm(form= ~Time),
                data=impMNAR20_long_01, subset=c(impMNAR20_long_01$.imp==1), na.action="na.omit",
                method = "REML",
                control=(msMaxIter=100000))  
  
  #getting p values
  implist_p = rbind(implist_p, summary(lmm_mi)$tTable[5:6,5])
  
  #getting coefficient and standard error
  imp_trt1C = rbind(imp_trt1C, summary(lmm_mi)$tTable[5,1])
  imp_trt1S = rbind(imp_trt1S, summary(lmm_mi)$tTable[5,2])
  
  imp_trt2C = rbind(imp_trt2C, summary(lmm_mi)$tTable[6,1])
  imp_trt2S = rbind(imp_trt2S, summary(lmm_mi)$tTable[6,2])
  
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
implist_coef1=rbind(implist_coef1, combined.results2$q.mi)
implist_se1=rbind(implist_se1, combined.results2$se.mi)
implist_coef2=rbind(implist_coef2, combined.results3$q.mi)
implist_se2=rbind(implist_se2, combined.results3$se.mi)
imp_pvalue1=rbind(imp_pvalue1, imp_p_final)

#getting coefficient and SEs to determine 95% confidence interval, for coverage
estimates = cbind(combined.results2$q.mi, combined.results3$q.mi)
ses = cbind(combined.results2$se.mi, combined.results3$se.mi)

Upper = estimates + (ses*1.96)
Lower = estimates - (ses * 1.96)

imp_coverage1[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)













############################################
##### Multilevel multiple imputation ######
############################################

#creating blank imputation to be able to fill in predictor matrix (pred)
ini1 = mice(Data1, maxit=0)
pred = ini1$pred

#getting "method of imputation"
meth = ini1$meth

#filling in the method of imputation as "", which means do not impute these variables
meth[c("School", "trtGrp", "idChild", "DEIS", "constant")] = ""

#filling in predictor matrix. -2 is the random intercept, 2 the column of 1s necessary for this
#all other predictors are the treatment group, DEIS status, and the other 2 ESA variables
pred["ESA_1_20MNAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 2)
pred["ESA_2_20MNAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 2)
pred["ESA_3_20MNAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 2)
pred

#method to impute these variables is the multilevel MI for continuous variables
meth[c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR")] = "2l.norm"

#running imputation model with 40 imputed datasets
imp = mice(Data1, meth=meth, pred=pred, maxit=10, m = 40, pri=F)

#selecting out the appropriate imputed datasets from the output of imp
com_MNAR20 = complete(imp, "long")

#Next, changing to further long format via "Time"
impMNAR20_long_01 <- melt(com_MNAR20,
                          # ID variables - all the variables to keep but not split apart on
                          id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "constant"),
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
               random = list(idChild = ~1), 
               corr = corCompSymm(form= ~Time),
               data=impMNAR20_long_01, subset=c(impMNAR20_long_01$.imp==1), na.action="na.omit",
               method = "REML",
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
implist_coef3=rbind(implist_coef3, combined.results2$q.mi)
implist_se3=rbind(implist_se3, combined.results2$se.mi)
implist_coef4=rbind(implist_coef4, combined.results3$q.mi)
implist_se4=rbind(implist_se4, combined.results3$se.mi)
imp_pvalue2=rbind(imp_pvalue2, imp_p_final)

#getting coefficient and SEs to determine 95% confidence interval, for coverage
estimates = cbind(combined.results2$q.mi, combined.results3$q.mi)
ses = cbind(combined.results2$se.mi, combined.results3$se.mi)

Upper = estimates + (ses*1.96)
Lower = estimates - (ses * 1.96)

imp_coverage2[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)










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
mxp[i,1] = ifelse(x[1,1] > 0, 1, 0)
mxp[i,2] = ifelse(x[2,1] > 0, 1, 0)

#getting 95% CI for coverage
Upper = OpenMx_stats2 + (OpenMx_stats3*1.96)
Lower = OpenMx_stats2 - (OpenMx_stats3*1.96)

OpenMx_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)




###############################
############ LOCF #############
###############################


Data3=Data1

#if missing in ESA_2, replace with value from ESA1
Data3$ESA_2_20MNAR = ifelse(is.na(Data3$ESA_2_20MNAR), Data3$ESA_1_20MNAR, Data3$ESA_2_20MNAR)
#if missing in ESA_3, replace with value from ESA2
Data3$ESA_3_20MNAR = ifelse(is.na(Data3$ESA_3_20MNAR), Data3$ESA_2_20MNAR, Data3$ESA_3_20MNAR)

#transform to long format
Data3_long <- melt(Data3,
                   # ID variables - all the variables to keep but not split apart on
                   id.vars=c("idChild", "School", "trtGrp", "DEIS"),
                   # The source columns
                   measure.vars=c("ESA_1_20MNAR","ESA_2_20MNAR", "ESA_3_20MNAR"),
                   # Name of the destination column that will identify the original
                   # column that the measurement came from
                   variable.name="Time",
                   value.name="ESA_Recid")

Data3_long$Time = recode(Data3_long$Time, " 'ESA_1_20MNAR'=1")
Data3_long$Time = recode(Data3_long$Time, " 'ESA_2_20MNAR'=2")
Data3_long$Time = recode(Data3_long$Time, " 'ESA_3_20MNAR'=3")


#running linear mixed model of locf data
lmm_locf <-  lme(ESA_Recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=Data3_long, na.action="na.omit",
              method = "REML",
              control=(msMaxIter=100000))


#getting coefficients, standard errors, p values
TestSum1 = summary(lmm_locf)$tTable[5:6,1]
TestSum2 = summary(lmm_locf)$tTable[5:6,2]
TestSum3 = summary(lmm_locf)$tTable[5:6,5]

LOCF_coef = rbind(LOCF_coef, TestSum1)
LOCF_se = rbind(LOCF_se, TestSum2)
LOCF_p = rbind(LOCF_p, TestSum3)

#getting 95% confidence interval for coverage
Upper = TestSum1 + (TestSum2*1.96)
Lower = TestSum1 - (TestSum2 * 1.96)


LOCF_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)






########################################
######### Mean Imputation ############
########################################
Data4=Data1

#order by treatment group
Data4 = Data4[order(Data4$trtGrp),]

mean(Data4$ESA_3_20MNAR[Data4$trtGrp==0], na.rm=T)

#replace each missing value as the mean of that treatment group at that time-point
Data4$ESA_1_20MNAR[Data4$trtGrp==0] = ifelse(is.na(Data4$ESA_1_20MNAR[Data4$trtGrp==0]),
                                             mean(Data4$ESA_1_20MNAR[Data4$trtGrp==0], na.rm=T), 
                                             Data4$ESA_1_20MNAR[Data4$trtGrp==0])

Data4$ESA_2_20MNAR[Data4$trtGrp==0] = ifelse(is.na(Data4$ESA_2_20MNAR[Data4$trtGrp==0]),
                                             mean(Data4$ESA_2_20MNAR[Data4$trtGrp==0], na.rm=T), 
                                             Data4$ESA_2_20MNAR[Data4$trtGrp==0])

Data4$ESA_3_20MNAR[Data4$trtGrp==0] = ifelse(is.na(Data4$ESA_3_20MNAR[Data4$trtGrp==0]),
                                             mean(Data4$ESA_3_20MNAR[Data4$trtGrp==0], na.rm=T), 
                                             Data4$ESA_3_20MNAR[Data4$trtGrp==0])

#repeat for trtgrp==1
Data4$ESA_1_20MNAR[Data4$trtGrp==1] = ifelse(is.na(Data4$ESA_1_20MNAR[Data4$trtGrp==1]),
                                             mean(Data4$ESA_1_20MNAR[Data4$trtGrp==1], na.rm=T), 
                                             Data4$ESA_1_20MNAR[Data4$trtGrp==1])

Data4$ESA_2_20MNAR[Data4$trtGrp==1] = ifelse(is.na(Data4$ESA_2_20MNAR[Data4$trtGrp==1]),
                                             mean(Data4$ESA_2_20MNAR[Data4$trtGrp==1], na.rm=T), 
                                             Data4$ESA_2_20MNAR[Data4$trtGrp==1])

Data4$ESA_3_20MNAR[Data4$trtGrp==1] = ifelse(is.na(Data4$ESA_3_20MNAR[Data4$trtGrp==1]),
                                             mean(Data4$ESA_3_20MNAR[Data4$trtGrp==1], na.rm=T), 
                                             Data4$ESA_3_20MNAR[Data4$trtGrp==1])


#transforming to long format
Data4_long <- melt(Data4,
                   # ID variables - all the variables to keep but not split apart on
                   id.vars=c("idChild", "School", "trtGrp", "DEIS"),
                   # The source columns
                   measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"),
                   # Name of the destination column that will identify the original
                   # column that the measurement came from
                   variable.name="Time",
                   value.name="ESA_Recid")

Data4_long$Time = recode(Data4_long$Time, " 'ESA_1_20MNAR'=1")
Data4_long$Time = recode(Data4_long$Time, " 'ESA_2_20MNAR'=2")
Data4_long$Time = recode(Data4_long$Time, " 'ESA_3_20MNAR'=3")


#linear mixed model of the  mean imputation
lmm_MeI <-  lme(ESA_Recid ~ Time + trtGrp + trtGrp*Time, 
                     random = list(School = ~1, idChild = ~1), 
                     weights =varIdent(School),
                     corr = corCompSymm(form= ~Time),
                     data=Data4_long, na.action="na.omit",
                     method = "REML",
                     control=(msMaxIter=100000))


#getting coefficients, standard errors, p-values
TestSum1 = summary(lmm_MeI)$tTable[5:6,1]
TestSum2 = summary(lmm_MeI)$tTable[5:6,2]
TestSum3 = summary(lmm_MeI)$tTable[5:6,5]


Mean_coef = rbind(Mean_coef, TestSum1)
Mean_se = rbind(Mean_se, TestSum2)
Mean_p = rbind(Mean_p, TestSum3)

#getting 95% confidence intervals for coverage
Upper = TestSum1 + (TestSum2*1.96)
Lower = TestSum1 - (TestSum2 * 1.96)

Mean_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)



##############################################
######### Cluster Mean Imputation ############
##############################################

Data4=Data1

#order by School number
Data4 = Data4[order(Data4$School),]

#creating loop for each school. If any missing values, replace with mean of the students
#within that school, at that time-point
for(i in 1:15){
  Data4$ESA_1_20MNAR[Data4$School==i] = ifelse(is.na(Data4$ESA_1_20MNAR[Data4$School==i]),
                                               mean(Data4$ESA_1_20MNAR[Data4$School==i], na.rm=T), 
                                               Data4$ESA_1_20MNAR[Data4$School==i])
}

for(i in 1:15){
  Data4$ESA_2_20MNAR[Data4$School==i] = ifelse(is.na(Data4$ESA_2_20MNAR[Data4$School==i]),
                                               mean(Data4$ESA_2_20MNAR[Data4$School==i], na.rm=T), 
                                               Data4$ESA_2_20MNAR[Data4$School==i])
}

for(i in 1:15){
  Data4$ESA_3_20MNAR[Data4$School==i] = ifelse(is.na(Data4$ESA_3_20MNAR[Data4$School==i]),
                                               mean(Data4$ESA_3_20MNAR[Data4$School==i], na.rm=T), 
                                               Data4$ESA_3_20MNAR[Data4$School==i])
}


#transforming to long format
Data4_long <- melt(Data4,
                   # ID variables - all the variables to keep but not split apart on
                   id.vars=c("idChild", "School", "trtGrp", "DEIS"),
                   # The source columns
                   measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"),
                   # Name of the destination column that will identify the original
                   # column that the measurement came from
                   variable.name="Time",
                   value.name="ESA_Recid")

Data4_long$Time = recode(Data4_long$Time, " 'ESA_1_20MNAR'=1")
Data4_long$Time = recode(Data4_long$Time, " 'ESA_2_20MNAR'=2")
Data4_long$Time = recode(Data4_long$Time, " 'ESA_3_20MNAR'=3")


#linear mixed model of the clustered mean imputation
lmm_clustMeI <-  lme(ESA_Recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=Data4_long, na.action="na.omit",
              method = "REML",
              control=(msMaxIter=100000))


#getting coefficients, standard errors, p-values
TestSum1 = summary(lmm_clustMeI)$tTable[5:6,1]
TestSum2 = summary(lmm_clustMeI)$tTable[5:6,2]
TestSum3 = summary(lmm_clustMeI)$tTable[5:6,5]


MeanClus_coef = rbind(Mean_coef, TestSum1)
MeanClus_se = rbind(Mean_se, TestSum2)
MeanClus_p = rbind(Mean_p, TestSum3)

#getting 95% confidence intervals for coverage
Upper = TestSum1 + (TestSum2*1.96)
Lower = TestSum1 - (TestSum2 * 1.96)

MeanClus_coverage[i, 1:2] = ifelse(Coverage < Upper & Coverage > Lower, T, F)


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

#multiple imputation results, single level
imp_bias1 = c((Trt1_effect[1]-mean(implist_coef1)), (Trt1_effect[2]-mean(implist_se1)), length(which(imp_pvalue1[,1] > 0.05)))
imp_bias1

#multiple imputation results, multilevel level
imp_bias2 = c((Trt1_effect[1]-mean(implist_coef3)), (Trt1_effect[2]-mean(implist_se3)), length(which(imp_pvalue2[,1] > 0.05)))
imp_bias2

#linear mixed model results
LMM_bias2 = c((Trt1_effect[1]-mean(LMM_coef[,1])), (Trt1_effect[2]-mean(LMM_se[,1])), length(which(LMM_p[,1] > 0.05)))
LMM_bias2

#complete case analysis results
CCA_bias2 = c((Trt1_effect[1]-mean(CCA_coef[,1])), (Trt1_effect[2]-mean(CCA_se[,1])), length(which(CCA_p[,1] > 0.05)))
CCA_bias2

#last observation carried forward results
LOCF_bias2 = c((Trt1_effect[1]-mean(LOCF_coef[,1])), (Trt1_effect[2]-mean(LOCF_se[,1])), length(which(LOCF_p[,1] > 0.05)))
LOCF_bias2

#mean imputation results
Mean_bias2 = c((Trt1_effect[1]-mean(Mean_coef[,1])), (Trt1_effect[2]-mean(Mean_se[,1])), length(which(Mean_p[,1] > 0.05)))
Mean_bias2

#clustered mean imputation results
MeanClus_bias2 = c((Trt1_effect[1]-mean(MeanClus_coef[,1])), (Trt1_effect[2]-mean(MeanClus_se[,1])), length(which(MeanClus_p[,1] > 0.05)))
MeanClus_bias2



## Second slope/effect bias
OpenMx_bias2 = c((Trt2_effect[1]-mean(OpenMx_coef[,2])), (Trt2_effect[2]-mean(OpenMx_SE[,2])), (length(which(mxp[,2]==0))))
OpenMx_bias2

imp_bias3 = c((Trt2_effect[1]-mean(implist_coef2)), (Trt2_effect[2]-mean(implist_se2)), length(which(imp_pvalue1[,2] > 0.05)))
imp_bias3

imp_bias4 = c((Trt2_effect[1]-mean(implist_coef4)), (Trt2_effect[2]-mean(implist_se4)), length(which(imp_pvalue2[,2] > 0.05)))
imp_bias4

LMM_bias3 = c((Trt2_effect[1]-mean(LMM_coef[,2])), (Trt2_effect[2]-mean(LMM_se[,2])), length(which(LMM_p[,2]> 0.05)))
LMM_bias3

CCA_bias3 = c((Trt2_effect[1]-mean(CCA_coef[,2])), (Trt2_effect[2]-mean(CCA_se[,2])), length(which(CCA_p[,2]> 0.05)))
CCA_bias3

LOCF_bias3 = c((Trt2_effect[1]-mean(LOCF_coef[,2])), (Trt2_effect[2]-mean(LOCF_se[,2])), length(which(LOCF_p[,2]> 0.05)))
LOCF_bias3

Mean_bias3 = c((Trt2_effect[1]-mean(Mean_coef[,2])), (Trt2_effect[2]-mean(Mean_se[,2])), length(which(Mean_p[,2]> 0.05)))
Mean_bias3

MeanClus_bias3 = c((Trt2_effect[1]-mean(MeanClus_coef[,2])), (Trt2_effect[2]-mean(MeanClus_se[,2])), length(which(MeanClus_p[,2]> 0.05)))
MeanClus_bias3



#combining all results
Slope1Bias = rbind(Trt1_effect, CCA_bias2, LMM_bias2, OpenMx_bias1, imp_bias1, imp_bias2, LOCF_bias2, Mean_bias2, MeanClus_bias2)
Slope2Bias = rbind(Trt2_effect, CCA_bias3, LMM_bias3, OpenMx_bias2, imp_bias3, imp_bias4, LOCF_bias3, Mean_bias3, MeanClus_bias3)



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



## imputation Coefficient MCSEs
z = rep(mean(implist_coef1), times=1000)
MCSE_impCoef_1 = sqrt(sum((implist_coef1 - z)^2)/999000)
z = rep(mean(implist_coef2), times=1000)
MCSE_impCoef_2 = sqrt(sum((implist_coef2 - z)^2)/999000)

## SEs SEs
z = rep(mean(implist_se1), times=1000)
MCSE_impSE_1 = sqrt(sum((implist_se1 - z)^2)/999000)
z = rep(mean(implist_se2), times=1000)
MCSE_impSE_2 = sqrt(sum((implist_se2 - z)^2)/999000)



## imputation Coefficient MCSEs
z = rep(mean(implist_coef3), times=1000)
MCSE_impCoefML_1 = sqrt(sum((implist_coef3 - z)^2)/999000)
z = rep(mean(implist_coef4), times=1000)
MCSE_impCoefML_2 = sqrt(sum((implist_coef4 - z)^2)/999000)

## SEs SEs
z = rep(mean(implist_se3), times=1000)
MCSE_impSEML_1 = sqrt(sum((implist_se3 - z)^2)/999000)
z = rep(mean(implist_se4), times=1000)
MCSE_impSEML_2 = sqrt(sum((implist_se4 - z)^2)/999000)



## CCA Coefficient MCSEs
z = rep(mean(CCA_coef[,1]), times=1000)
MCSE_CCACoef_1 = sqrt(sum((CCA_coef[,1] - z)^2)/999000)
z = rep(mean(CCA_coef[,2]), times=1000)
MCSE_CCACoef_2 = sqrt(sum((CCA_coef[,2] - z)^2)/999000)

## SEs SEs
z = rep(mean(CCA_se[,1]), times=1000)
MCSE_CCASE_1 = sqrt(sum((CCA_se[,1] - z)^2)/999000)
z = rep(mean(CCA_se[,2]), times=1000)
MCSE_CCASE_2 = sqrt(sum((CCA_se[,2] - z)^2)/999000)



## LOCF Coefficient MCSEs
z = rep(mean(LOCF_coef[,1]), times=1000)
MCSE_LOCFCoef_1 = sqrt(sum((LOCF_coef[,1] - z)^2)/999000)
z = rep(mean(LOCF_coef[,2]), times=1000)
MCSE_LOCFCoef_2 = sqrt(sum((LOCF_coef[,2] - z)^2)/999000)

## SEs SEs
z = rep(mean(LOCF_se[,1]), times=1000)
MCSE_LOCFSE_1 = sqrt(sum((LOCF_se[,1] - z)^2)/999000)
z = rep(mean(LOCF_se[,2]), times=1000)
MCSE_LOCFSE_2 = sqrt(sum((LOCF_se[,2] - z)^2)/999000)



## MEAN imp Coefficient MCSEs
z = rep(mean(Mean_coef[,1]), times=1000)
MCSE_MeanCoef_1 = sqrt(sum((Mean_coef[,1] - z)^2)/999000)
z = rep(mean(Mean_coef[,2]), times=1000)
MCSE_MeanCoef_2 = sqrt(sum((Mean_coef[,2] - z)^2)/999000)

## SEs SEs
z = rep(mean(Mean_se[,1]), times=1000)
MCSE_MeanSE_1 = sqrt(sum((Mean_se[,1] - z)^2)/999000)
z = rep(mean(Mean_se[,2]), times=1000)
MCSE_MeanSE_2 = sqrt(sum((Mean_se[,2] - z)^2)/999000)



## clustered MEAN imp Coefficient MCSEs
z = rep(mean(MeanClus_coef[,1]), times=1000)
MCSE_MeanCoefclus_1 = sqrt(sum((MeanClus_coef[,1] - z)^2)/999000)
z = rep(mean(MeanClus_coef[,2]), times=1000)
MCSE_MeanCoefclus_2 = sqrt(sum((MeanClus_coef[,2] - z)^2)/999000)

## SEs SEs
z = rep(mean(MeanClus_se[,1]), times=1000)
MCSE_MeanSEclus_1 = sqrt(sum((MeanClus_se[,1] - z)^2)/999000)
z = rep(mean(MeanClus_se[,2]), times=1000)
MCSE_MeanSEclus_2 = sqrt(sum((MeanClus_se[,2] - z)^2)/999000)






Slope1Bias = rbind(Trt1_effect, CCA_bias2, LMM_bias2, OpenMx_bias1, imp_bias1, imp_bias2, LOCF_bias2, Mean_bias2, MeanClus_bias2)
Slope2Bias = rbind(Trt2_effect, CCA_bias3, LMM_bias3, OpenMx_bias2, imp_bias3, imp_bias4, LOCF_bias3, Mean_bias3, MeanClus_bias3)

#creating dataset of the results of the coefficient and SE biases and Type II error
Total = rbind(Slope1Bias, Slope2Bias)
Total = as.data.frame(Total)

#adding column of monte carlo standard errors for the coefficient and SE estimates
Total$MC_SE_Coef = c(NA, MCSE_CCACoef_1, MCSE_LMMCoef_1, MCSE_MxCoef_1, MCSE_impCoef_1, MCSE_impCoefML_1, MCSE_LOCFCoef_1, MCSE_MeanCoef_1, MCSE_MeanCoefclus_1, 
                     NA, MCSE_CCACoef_2, MCSE_LMMCoef_2, MCSE_MxCoef_2, MCSE_impCoef_2, MCSE_impCoefML_2, MCSE_LOCFCoef_2, MCSE_MeanCoef_2, MCSE_MeanCoefclus_2)
Total$MC_SE_SEs = c(NA, MCSE_CCASE_1, MCSE_LMMSE_1, MCSE_MxSE_1, MCSE_impSE_1, MCSE_impSEML_1, MCSE_LOCFSE_1, MCSE_MeanSE_1, MCSE_MeanSEclus_1,
                    NA, MCSE_CCASE_2, MCSE_LMMSE_2, MCSE_MxSE_2, MCSE_impSE_2, MCSE_impSEML_2, MCSE_LOCFSE_2, MCSE_MeanSE_2, MCSE_MeanSEclus_2)


#adding coverage for each method
Total$Coverage = c(
  prop.table(table(UnDel_coverage[1:1000,1]))[2],
  prop.table(table(CCA_coverage[1:1000,1]))[2],
  prop.table(table(LMM_coverage[1:1000,1]))[2],
  prop.table(table(OpenMx_coverage[1:1000,1]))[2],
  prop.table(table(imp_coverage1[1:1000,1]))[2],
  prop.table(table(imp_coverage2[1:1000,1]))[2],
  prop.table(table(LOCF_coverage[1:1000,1]))[2],
  prop.table(table(Mean_coverage[1:1000,1]))[2], 
  prop.table(table(MeanClus_coverage[1:1000,1]))[2], 
  prop.table(table(UnDel_coverage[1:1000,2]))[2], 
  prop.table(table(CCA_coverage[1:1000,2]))[2],
  prop.table(table(LMM_coverage[1:1000,2]))[2],
  prop.table(table(OpenMx_coverage[1:1000,2]))[2],
  prop.table(table(imp_coverage1[1:1000,2]))[2],
  prop.table(table(imp_coverage2[1:1000,2]))[2],
  prop.table(table(LOCF_coverage[1:1000,2]))[2],
  prop.table(table(Mean_coverage[1:1000,2]))[2]
  prop.table(table(MeanClus_coverage[1:1000,2]))[2]
)



#export as excel. Named with number of clusters, mechanism, and percent missing
WriteXLS(Total, ExcelFileName = "CopeSmart_10_MNAR1000_20.xlsx", col.names = TRUE, row.names=TRUE)
