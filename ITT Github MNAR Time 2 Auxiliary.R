
##########################################
### ITT Chapter code/Noncompliance#######
##########################################

#Creating blank elements to fill in from loops
#Coefficient, standard error, p value, coverage

#full analysis set
Undeleted_coef = NULL
Undeleted_SE = NULL
Undeleted_p = NULL
UnDel_coverage = matrix(nrow=1000, ncol=1)

#linear mixed model/modified Per protocol (PP)
LMM_coef=NULL
LMM_se = NULL
LMM_p = NULL
LMM_coverage = matrix(nrow=1000, ncol=1)

#linear mixed model/modified Per protocol (PP)2
LMM_coef2=NULL
LMM_se2 = NULL
LMM_p2 = NULL
LMM_coverage2 = matrix(nrow=1000, ncol=1)

#complete case analysis/per protocol (PP)
CCA_coef=NULL
CCA_se = NULL
CCA_p = NULL
CCA_coverage = matrix(nrow=10000, ncol=1)

#complete case analysis/per protocol (PP)2
CCA_coef2 = NULL
CCA_se2 = NULL
CCA_p2 = NULL
CCA_coverage2 = matrix(nrow=1000, ncol=1)

#Last observation carried forward (0% compliant, retain treatment effect)
LOCF_coef=NULL
LOCF_se = NULL
LOCF_p = NULL
LOCF_coverage = matrix(nrow=1000, ncol=1)

#mean imputation
Mean_coef=NULL
Mean_se = NULL
Mean_p = NULL
Mean_coverage = matrix(nrow=1000, ncol=1)

#multiple imputation
implist_coef=NULL
implist_se = NULL
imp_pvalue=NULL
imp_coverage = matrix(nrow=1000, ncol=1)

#multiple imputation
implist_coef2 = NULL
implist_se2 = NULL
imp_pvalue = NULL
imp_coverage2 = matrix(nrow=1000, ncol=1)

#Intention to Treat (ITT) 80% compliant
Switch_coef80=NULL
Switch_se80 = NULL
Switch_pvalue80=NULL
Switch_coverage80 = matrix(nrow=1000, ncol=1)


#Intention to Treat (ITT) 80% compliant
Switch_coef802=NULL
Switch_se802 = NULL
Switch_pvalue802=NULL
Switch_coverage802 = matrix(nrow=1000, ncol=1)


#full information maximum likelihood/structural equation model
OpenMx_coef = NULL
OpenMx_SE = NULL
OpenMx_coverage = matrix(nrow=1000, ncol=)
#for p-value for SEM
mxp = data.frame(pvalue1 = rep(2, length=1000), pvalue2 = rep(2, length=1000))

#full information maximum likelihood/structural equation model
OpenMx_coef2 = NULL
OpenMx_SE2 = NULL
OpenMx_coverage2 = matrix(nrow=1000, ncol=)
#for p-value for SEM
mxp2 = data.frame(pvalue1 = rep(2, length=1000), pvalue2 = rep(2, length=1000))




###########################################
###### Simulates 1000 X #######################
###########################################
for (i in 1:1000) {
  
  #creation of dataset with 180 individuals
  dtTrial <- genData(120)
  
  #Adding treatment, control or treatment, randomly assignd to half of participants
  dtTrial <- trtAssign(dtTrial, n=2)
  #Adding Gender, randomly assigned half male half female
  dtTrial = trtAssign(dtTrial, n=2, balanced=TRUE, grpName = "Gender")
  
  #creating a correlation matrix among the 5 repeated measurements
  C = matrix(c(1, 0.7, 0.7, 0.7, 0.7, 
               0.7, 1, 0.7, 0.7, 0.7, 
               0.7, 0.7, 1, 0.7, 0.7, 
               0.7, 0.7, 0.7, 1, 0.7, 
               0.7, 0.7, 0.7, 0.7, 1), nrow=5)
  
  #creating dataset of the 5 repeated measures, length is same as dtTrial. Mean is 0 at baselie and SD is 1
  dt = genCorData(length(dtTrial$id), mu=c(0, 0, 0, 0, 0), sigma=c(1, 1, 1, 1, 1), corMatrix=C)
  
  
  #creating the Treatment*Time effects. Four because 4 follow-up times
  gen.effect = defDataAdd(varname = "Tr1", dist = "normal", formula = 0.10, variance = 0.10)
  gen.effect = defDataAdd(gen.effect, varname = "Tr2", dist = "normal", formula = 0.10, variance = 0.10)
  gen.effect = defDataAdd(gen.effect, varname = "Tr3", dist = "normal", formula = 0.10, variance = 0.10)
  gen.effect = defDataAdd(gen.effect, varname = "Tr4", dist = "normal", formula = 0.10, variance = 0.10)
 
   #Adding age as covariate
  gen.effect = defDataAdd(gen.effect, varname = "Age", dist = "uniform", formula = "18;60")
  
  
  #combined participant and effects information
  dtTrial = addColumns(gen.effect, dtTrial)
  
  dtTrial <- cbind(dtTrial, Outcome_1 = dt$V1)
  dtTrial <- cbind(dtTrial, Outcome_2 = dt$V2)
  dtTrial <- cbind(dtTrial, Outcome_3 = dt$V3)
  dtTrial <- cbind(dtTrial, Outcome_4 = dt$V4)
  dtTrial <- cbind(dtTrial, Outcome_5 = dt$V5)
  

  #adding the treatment*time effects to those in the intervention group
  dtTrial$Outcome_1=dtTrial$Outcome_1 
  dtTrial$Outcome_2=dtTrial$Outcome_2 + (dtTrial$Tr1*dtTrial$trtGrp) 
  dtTrial$Outcome_3=dtTrial$Outcome_3 + (dtTrial$Tr1*dtTrial$trtGrp) + (dtTrial$Tr2*dtTrial$trtGrp)
  dtTrial$Outcome_4=dtTrial$Outcome_4 + (dtTrial$Tr1*dtTrial$trtGrp) + (dtTrial$Tr2*dtTrial$trtGrp) + (dtTrial$Tr3*dtTrial$trtGrp)
  dtTrial$Outcome_5=dtTrial$Outcome_5 + (dtTrial$Tr1*dtTrial$trtGrp) + (dtTrial$Tr2*dtTrial$trtGrp) + (dtTrial$Tr3*dtTrial$trtGrp) + (dtTrial$Tr4*dtTrial$trtGrp)
  
  #recording the simulated values for the "worst" case of 0% compliant and drop back to control group/baseline
  dtTrial$ITT_2 = dt$V2
  dtTrial$ITT_3 = dt$V3
  dtTrial$ITT_4 = dt$V4
  dtTrial$ITT_5 = dt$V5
  
  #getting just the variables necessary
  dtTrial = dtTrial[, c("id", "trtGrp", "Outcome_1", "Outcome_2", "Outcome_3", "Outcome_4", "Outcome_5", "Gender", "Age", "ITT_2", "ITT_3", "ITT_4", "ITT_5", "Tr1", "Tr2", "Tr3", "Tr4")]
  dtTrial1 = dtTrial[, c("id", "trtGrp", "Outcome_1", "Outcome_2", "Outcome_3", "Outcome_4", "Outcome_5", "Gender", "Age")]
  
  #transforming to long format
  dtTrial_long <- melt(dtTrial1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("id", "trtGrp", "Gender", "Age"),
                         # The source columns
                         measure.vars=c("Outcome_1", "Outcome_2", "Outcome_3", "Outcome_4", "Outcome_5" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="Outcome")
  
  dtTrial_long$Time = recode(dtTrial_long$Time, " 'Outcome_1'=1")
  dtTrial_long$Time = recode(dtTrial_long$Time, " 'Outcome_2'=2")
  dtTrial_long$Time = recode(dtTrial_long$Time, " 'Outcome_3'=3")
  dtTrial_long$Time = recode(dtTrial_long$Time, " 'Outcome_4'=4")
  dtTrial_long$Time = recode(dtTrial_long$Time, " 'Outcome_5'=5")
  
  
  #ensuring it's one slope over the 5 time-points by keeping it as an integer
  dtTrial_long$Time=as.integer(dtTrial_long$Time)
  
  
  #linear mixed model of full analysis set
  #covariance structure as compound symmetry
  lmm_fas <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
              random = list(id = ~1),
              corr = corCompSymm(form= ~Time),
              data=dtTrial_long, na.action="na.omit",
              method = "ML",
              control=(msMaxIter=100))
  
  #recording results of coefficient, SE, and p value
  TestSum = summary(lmm_fas)$tTable[4,1]
  TestSum2 = summary(lmm_fas)$tTable[4,2]
  TestSum3 = summary(lmm_fas)$tTable[4,5]
  
  #add to objects per loop
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  Undeleted_SE = rbind(Undeleted_SE, TestSum2)
  Undeleted_p = rbind(Undeleted_p, TestSum3)
  
  ## Coverage: should be 0.10 for treatment*time effect
  Coverage = c(0.10)
  
  #getting 95% confidence interval
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  UnDel_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  

  
  
  #################################################
  ######## Introducing noncompliance ###############
  #################################################
  
  #sorting it by outcome value for MNAR deletions
  dtTrial = dtTrial[order(dtTrial$Outcome_2, decreasing=F), ]
  #ordering by trtGrp since only noncompliant in treatment group
  dtTrial = dtTrial[order(dtTrial$trtGrp), ]
  
  #create object length of control group, all 1s
  Del = rep(1, length(dtTrial$Outcome_3[dtTrial$trtGrp==0]))
  
  #create object length of intervention group, blank for now
  Del1 = rep(NA, length(dtTrial$Outcome_3[dtTrial$trtGrp==1]))
  
  #subsetting those in treatment group
  x = subset(dtTrial,trtGrp==1)
  
  
  #creating probabilities ranging from .01 to .40, average is 0.20 or 20%
  test2 = defData(varname = "w", dist = "uniform", formula = "1;40")
  test1= genData(length(x$Outcome_2), test2)
  test1 = test1[, "w"]
  #dividing by 100 to get probabilities
  test1=test1/100
  #sorting small to large to make it that larger outcome values more likely to be noncompliant
  test1 = sort(test1$w, decreasing=FALSE)
  #v is the opposite probability, for the sample() code
  v = 1-test1

  
  #creating the auxiliary variable, which is only correlated with the missingness probability for the treatment group
  #control group is just a random variable with mean of 0 and sd of 1
  library(dplyr)
  library(faux)
  Aux = c(rnorm(length(Del), 0, 1), rnorm_pre(test1, mu=0, sd=1, r=0.9))
  detach(package:faux)
  detach(package:dplyr)

 dtTrial$Aux = Aux
  
  
  #creating loop for each individual in the dataset in treatment group
  #each person has their own probability of being noncompliant or not
  for (j in 1:length(x$Outcome_2))
  {
    Del1[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }
  
  #combining the Del object (all 1s) with the new Del1 object
  dtTrial$Del2 = c(Del, Del1)
  
  #creating blanks if Del2 is a zero. Will fill in with different noncompliant values later.
  #if someone is noncompliant at Time 2, will be noncompliant for remainder of trial
  dtTrial$Outcome_2_05MNAR = ifelse(dtTrial$Del2==0, NA, dtTrial$Outcome_2)
  dtTrial$Outcome_3_05MNAR = ifelse(dtTrial$Del2==0, NA, dtTrial$Outcome_3)
  dtTrial$Outcome_4_05MNAR = ifelse(dtTrial$Del2==0, NA, dtTrial$Outcome_4)
  dtTrial$Outcome_5_05MNAR = ifelse(dtTrial$Del2==0, NA, dtTrial$Outcome_5)
  
  dtTrial = dtTrial[order(id), ]
  
  #creating new dataset for the "worst case" scenario
  dtTrial3 = dtTrial
  
  #filling in the blanks with the "ITT2" etc values. This represents if they switched into the control group.
  dtTrial3$Outcome_2_05MNAR = ifelse(is.na(dtTrial3$Outcome_2_05MNAR), dtTrial3$ITT_2, dtTrial3$Outcome_2_05MNAR)
  dtTrial3$Outcome_3_05MNAR = ifelse(is.na(dtTrial3$Outcome_3_05MNAR), dtTrial3$ITT_3, dtTrial3$Outcome_3_05MNAR)
  dtTrial3$Outcome_4_05MNAR = ifelse(is.na(dtTrial3$Outcome_4_05MNAR), dtTrial3$ITT_4, dtTrial3$Outcome_4_05MNAR)
  dtTrial3$Outcome_5_05MNAR = ifelse(is.na(dtTrial3$Outcome_5_05MNAR), dtTrial3$ITT_5, dtTrial3$Outcome_5_05MNAR)
  
  #just getting the variables of interest
  dtTrial2 = dtTrial3[, c("id", "trtGrp", "Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR", "Gender", "Age")]
  
  #transforming to long format
  dtTrial_long2 <- melt(dtTrial2,
                          # ID variables - all the variables to keep but not split apart on
                          id.vars=c("id", "trtGrp", "Gender", "Age"),
                          # The source columns
                          measure.vars=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR" ),
                          # Name of the destination column that will identify the original
                          # column that the measurement came from
                          variable.name="Time",
                          value.name="Outcome")
  
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_1'=1")
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_2_05MNAR'=2")
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_3_05MNAR'=3")
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_4_05MNAR'=4")
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_5_05MNAR'=5")
  
  dtTrial_long2$Time=as.integer(dtTrial_long2$Time)
  
  #linear mixed model of the "worst case" (switch into control group) data
  lmm_worst <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
               random = list(id = ~1),
               corr = corCompSymm(form= ~Time),
               data=dtTrial_long2, na.action="na.omit",
               method = "ML",
               control=(msMaxIter=100))
  
  
  TestSum = summary(lmm_worst)$tTable[4,1]
  TestSum2 = summary(lmm_worst)$tTable[4,2]
  TestSum3 = summary(lmm_worst)$tTable[4,5]
  
  
  #recording results
  Worst_coef = rbind(Worst_coef, TestSum)
  Worst_se = rbind(Worst_se, TestSum2)
  Worst_p = rbind(Worst_p, TestSum3)
  
  
  #getting 95% confidence interval for coverage
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  Worst_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  
  #########################################
  ######## Intention to Treat Datasets ######
  #########################################
  
  #this time, the blanks are filled in with 80% of the treatment effect for the remainder of the trial
  #since 80% compliant
  Outcome_2_ITT= ifelse(is.na(dtTrial$Outcome_2_05MNAR), (dtTrial$Outcome_1 + dtTrial$Tr1*.80), dtTrial$Outcome_2)
  Outcome_3_ITT= ifelse(is.na(dtTrial$Outcome_3_05MNAR), (dtTrial$Outcome_1 + dtTrial$Tr1*.80 + dtTrial$Tr2*0.80), dtTrial$Outcome_3)
  Outcome_4_ITT= ifelse(is.na(dtTrial$Outcome_4_05MNAR), (dtTrial$Outcome_1 + dtTrial$Tr1*.80 + dtTrial$Tr2*0.80 + dtTrial$Tr3*0.80), dtTrial$Outcome_4)
  Outcome_5_ITT= ifelse(is.na(dtTrial$Outcome_5_05MNAR), (dtTrial$Outcome_1 + dtTrial$Tr1*.80 + dtTrial$Tr2*0.80 + dtTrial$Tr3*0.80 + dtTrial$Tr4*0.80), dtTrial$Outcome_5)
  
  #creating new datasets
  rm(Data1)
  Data1 = cbind(dtTrial, Outcome_2_ITT, Outcome_3_ITT, Outcome_4_ITT, Outcome_5_ITT)
  
  
  #For the Per Protocol dataset--with the blanks for those who are noncompliant
  Data1_cca = Data1[, c("id", "trtGrp", "Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR", "Gender", "Age", "Aux")]
  
  #for the ITT80 dataset (80%compliant)
  Data1_ITT = Data1[, c("id", "trtGrp", "Outcome_1", "Outcome_2_ITT", "Outcome_3_ITT", "Outcome_4_ITT", "Outcome_5_ITT", "Gender", "Age", "Aux")]
  
  
  
  #transforming modified per protocol dataset to long format
  Data1_long <- melt(Data1_cca,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("id", "trtGrp", "Gender", "Age", "Aux"),
                     # The source columns
                     measure.vars=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="Outcome")
  
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_1'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_2_05MNAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_3_05MNAR'=3")
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_4_05MNAR'=4")
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_5_05MNAR'=5")
  
  
  #transforming ITT80 to long format
  Data1_long_ITT <- melt(Data1_ITT,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("id", "trtGrp", "Gender", "Age", "Aux"),
                         # The source columns
                         measure.vars=c("Outcome_1", "Outcome_2_ITT", "Outcome_3_ITT", "Outcome_4_ITT", "Outcome_5_ITT" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="Outcome")
  
  Data1_long_ITT$Time = recode(Data1_long_ITT$Time, " 'Outcome_1'=1")
  Data1_long_ITT$Time = recode(Data1_long_ITT$Time, " 'Outcome_2_ITT'=2")
  Data1_long_ITT$Time = recode(Data1_long_ITT$Time, " 'Outcome_3_ITT'=3")
  Data1_long_ITT$Time = recode(Data1_long_ITT$Time, " 'Outcome_4_ITT'=4")
  Data1_long_ITT$Time = recode(Data1_long_ITT$Time, " 'Outcome_5_ITT'=5")
  
  
  
  
  
  
  ########################################
  ### Modified Per Protocol ##########
  ########################################
  
  Data1_long$Time=as.integer(Data1_long$Time)
  
  #liner mixed model, modified per protocol
  lmm_modpp <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                random = list(id = ~1),
                corr = corCompSymm(form= ~Time),
                data=Data1_long, na.action="na.omit",
                method = "ML",
                control=(msMaxIter=100))
  
  
  #recording results
  TestSum = summary(lmm_modpp)$tTable[4,1]
  TestSum2 = summary(lmm_modpp)$tTable[4,2]
  TestSum3 = summary(lmm_modpp)$tTable[4,5]
  
  
  LMM_coef = rbind(LMM_coef, TestSum)
  LMM_se = rbind(LMM_se, TestSum2)
  LMM_p = rbind(LMM_p, TestSum3)
  

  #getting 95% confidence interval for coverage
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  LMM_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  

  
  
  
  #liner mixed model, modified per protocol with the auxiliary interaction variable with time and treatment group
  lmm_modpp2 <- lme(Outcome ~ Time + trtGrp + trtGrp*Time + Aux*trtGrp*Time, 
                   random = list(id = ~1),
                   corr = corCompSymm(form= ~Time),
                   data=Data1_long, na.action="na.omit",
                   method = "ML",
                   control=(msMaxIter=100))
  
  
  #recording results

  TestSum = summary(lmm_modpp2)$tTable[5,1]
  TestSum2 = summary(lmm_modpp2)$tTable[5,2]
  TestSum3 = summary(lmm_modpp2)$tTable[5,5]
  
  
  LMM_coef2 = rbind(LMM_coef2, TestSum)
  LMM_se2 = rbind(LMM_se2, TestSum2)
  LMM_p2 = rbind(LMM_p2, TestSum3)
  
  
  #getting 95% confidence interval for coverage
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  LMM_coverage2[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  ####################################
  ##### complete case analysis #####
  ####### Per Protocol ############
  ####################################
  
  #forcing out participants who were noncompliant
  Data2 = na.omit(Data1_cca)
  
  #transforming to long format
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("id", "trtGrp", "Gender", "Age", "Aux"),
                     # The source columns
                     measure.vars=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="Outcome")
  
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_1'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_2_05MNAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_3_05MNAR'=3")
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_4_05MNAR'=4")
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_5_05MNAR'=5")
  
  
  Data2_long$Time=as.integer(Data2_long$Time)
  
  #linear mixed model of per protocol
  lmm_pp <-  lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                random = list(id = ~1),
                corr = corCompSymm(form= ~Time),
                data=Data2_long, na.action="na.omit",
                method = "ML",
                control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(lmm_pp)$tTable[4,1]
  TestSum2 = summary(lmm_pp)$tTable[4,2]
  TestSum3 = summary(lmm_pp)$tTable[4,5]
  
  CCA_coef = rbind(CCA_coef, TestSum)
  CCA_se = rbind(CCA_se, TestSum2)
  CCA_p = rbind(CCA_p, TestSum3)
  
  #getting 95% confidence interval for coverage
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  CCA_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  #linear mixed model of per protocol, but with auxiliary variable included
  lmm_pp2 <-  lme(Outcome ~ Time + trtGrp + trtGrp*Time + Aux*trtGrp*Time, 
                 random = list(id = ~1),
                 corr = corCompSymm(form= ~Time),
                 data=Data2_long, na.action="na.omit",
                 method = "ML",
                 control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(lmm_pp2)$tTable[5,1]
  TestSum2 = summary(lmm_pp2)$tTable[5,2]
  TestSum3 = summary(lmm_pp2)$tTable[5,5]
  
  CCA_coef2 = rbind(CCA_coef2, TestSum)
  CCA_se2 = rbind(CCA_se2, TestSum2)
  CCA_p2 = rbind(CCA_p2, TestSum3)
  
  #getting 95% confidence interval for coverage
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  CCA_coverage2[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  
  
  ######## ITT, 80% compliant ############
  
  Data1_long_ITT$Time=as.integer(Data1_long_ITT$Time)
  
  #linear mixed model on ITT80 data
  lmm_80 <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                   random = list(id = ~1),
                   corr = corCompSymm(form= ~Time),
                   data=Data1_long_ITT, na.action="na.omit",
                   method = "ML",
                   control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(lmm_80)$tTable[4,1]
  TestSum2 = summary(lmm_80)$tTable[4,2]
  TestSum3 = summary(lmm_80)$tTable[4,5]
  
  Switch_coef80 = rbind(Switch_coef80, TestSum)
  Switch_se80 = rbind(Switch_se80, TestSum2)
  Switch_pvalue80 = rbind(Switch_pvalue80, TestSum3)
  

  #getting 95% confidence interval for coverage
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  Switch_coverage80[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  ######## ITT, 80% compliant ############
  
  Data1_long_ITT$Time=as.integer(Data1_long_ITT$Time)
  
  #linear mixed model on ITT80 data
  lmm_80 <- lme(Outcome ~ Time + trtGrp + trtGrp*Time + Aux*trtGrp*Time, 
                random = list(id = ~1),
                corr = corCompSymm(form= ~Time),
                data=Data1_long_ITT, na.action="na.omit",
                method = "ML",
                control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(lmm_80)$tTable[5,1]
  TestSum2 = summary(lmm_80)$tTable[5,2]
  TestSum3 = summary(lmm_80)$tTable[5,5]
  
  Switch_coef802 = rbind(Switch_coef802, TestSum)
  Switch_se802 = rbind(Switch_se802, TestSum2)
  Switch_pvalue802 = rbind(Switch_pvalue802, TestSum3)
  
  
  #getting 95% confidence interval for coverage
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  Switch_coverage802[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  
  #################################################
  ######## structural equation model #############
  #################################################
  
  Data5=Data1_cca 
  
  #labelling manifest variables (observed variables) and latent variables (I = intercept, S = slope)
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", 
                                                  "Outcome_5_05MNAR", "trtGrp"),
    latentVars=c("I", "S"), 
    mxData(observed=Data5, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=2, free=T, values=c(1,1,1,1,1),
           labels=c("variance1","variance1", "variance1", "variance1", "variance1")),
    mxPath(from="Outcome_1", to=c("Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=2, free=T, values=c(.8, .8, .8, .8),
           labels=c("covar1", "covar1", "covar1", "covar1")),
    mxPath(from="Outcome_2_05MNAR", to=c("Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=2, free=T, values=c(.8,.8,.8),
           labels=c("covar1", "covar1", "covar1")),
    mxPath(from="Outcome_3_05MNAR", to=c("Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=2, free=T, values=c(.8,.8),
           labels=c("covar1", "covar1")),
    mxPath(from="Outcome_4_05MNAR", to=c("Outcome_5_05MNAR"), arrows=2, free=T, values=.8,
           labels="covar1"),
    #one slope (S) from T1 to T5
    mxPath(from=c("I", "S"), arrows=2, connect="unique.pairs", free=F, values=0), 
    #for latent growth curves, the intercept loadings have to be all 1s from each repeated measure
    mxPath(from="I", to=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=1, free=F, 
           values=c(1, 1, 1,1,1)),
    #and the slope loadings have to be 0,1,2,3,4 from earliest to latest repeated measure
    mxPath(from="S", to=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=1, free=F, 
           values=c(0,1,2,3,4)),
    #manifest means not estimated
    mxPath(from="one", to=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=1, free=F,
           values=c(0, 0, 0, 0, 0)),
    #latent means estimated
    mxPath(from="one", to=c("I", "S"), arrows=1, free=T, values=c(1, 1), 
           labels=c("meanI", "meanS")),
    #getting the regression coefficients for treatment onto intercept and slope
     mxPath(from="trtGrp", to=c("I", "S"), arrows=1, free=TRUE, values=c(.5, .5),
           labels=c("ireg", "Sreg")),
    #manifest mean for trtGrp not estimated
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    #variance for trtGrp not estimated
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  #running this model using mxRun()
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  
  #recording coefficient and SE
  OpenMx_stats2 = SingleLevelModel1$output$estimate[2]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats2)
  
  OpenMx_stats3 = SingleLevelModel1$output$standardErrors[2]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats3)
  
  
  #getting p value for the coefficient
  m2 = mxModel(SingleLevelModel1, mxCI(c("Sreg"))) # list the things you want CIs for.
  m2 = mxRun(m2, intervals= T)
  x2 = m2$output$confidenceIntervals
  #recording if confidence interval does not contain 0 (then significant)

  mxp[i,1] = ifelse(x2[1,1] > 0, 1, 
                     ifelse(x2[1,3] < 0, 1, 0))
  
  
  #getting 95% confidence interval for coverage
  Upper = OpenMx_stats2 + (OpenMx_stats3*1.96)
  Lower = OpenMx_stats2 - (OpenMx_stats3*1.96)
  
  OpenMx_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  
  
  #labelling manifest variables (observed variables) and latent variables (I = intercept, S = slope)
  SingleLevelModel2= mxModel(
    "One Level Model", type="RAM", manifestVars=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", 
                                                  "Outcome_5_05MNAR", "trtGrp", "Aux"),
    latentVars=c("I", "S"), 
    mxData(observed=Data5, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=2, free=T, values=c(1,1,1,1,1),
           labels=c("variance1","variance1", "variance1", "variance1", "variance1")),
    mxPath(from="Outcome_1", to=c("Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=2, free=T, values=c(.8, .8, .8, .8),
           labels=c("covar1", "covar1", "covar1", "covar1")),
    mxPath(from="Outcome_2_05MNAR", to=c("Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=2, free=T, values=c(.8,.8,.8),
           labels=c("covar1", "covar1", "covar1")),
    mxPath(from="Outcome_3_05MNAR", to=c("Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=2, free=T, values=c(.8,.8),
           labels=c("covar1", "covar1")),
    mxPath(from="Outcome_4_05MNAR", to=c("Outcome_5_05MNAR"), arrows=2, free=T, values=.8,
           labels="covar1"),
    #one slope (S) from T1 to T5
    mxPath(from=c("I", "S"), arrows=2, connect="unique.pairs", free=F, values=0), 
    #for latent growth curves, the intercept loadings have to be all 1s from each repeated measure
    mxPath(from="I", to=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=1, free=F, 
           values=c(1, 1, 1,1,1)),
    #and the slope loadings have to be 0,1,2,3,4 from earliest to latest repeated measure
    mxPath(from="S", to=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=1, free=F, 
           values=c(0,1,2,3,4)),
    #manifest means not estimated
    mxPath(from="one", to=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR"), arrows=1, free=F,
           values=c(0, 0, 0, 0, 0)),
    #latent means estimated
    mxPath(from="one", to=c("I", "S"), arrows=1, free=T, values=c(1, 1), 
           labels=c("meanI", "meanS")),
    #getting the regression coefficients for treatment onto intercept and slope
    mxPath(from="trtGrp", to=c("I", "S"), arrows=1, free=TRUE, values=c(.5, .5),
           labels=c("ireg", "Sreg")),
    #manifest mean for trtGrp not estimated
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    #variance for trtGrp not estimated
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from = "Aux", to = c("trtGrp", "S", "I"), free=T, values = c(0.5, 0.5, 0.5)),
    mxPath(from = "Aux", arrows = 2, free=F, values=1),
    mxPath(from="one", to="Aux", arrows=1, free=F, values=0)
    )
  
  #running this model using mxRun()
  SingleLevelModel2 = mxRun(SingleLevelModel2)
  
  #recording coefficient and SE
  OpenMx_stats2 = SingleLevelModel2$output$estimate[2]
  OpenMx_coef2 = rbind(OpenMx_coef2, OpenMx_stats2)
  
  
  OpenMx_stats3 = SingleLevelModel2$output$standardErrors[2]
  OpenMx_SE2 = rbind(OpenMx_SE2, OpenMx_stats3)
  
  
  #getting p value for the coefficient
  m2 = mxModel(SingleLevelModel2, mxCI(c("Sreg"))) # list the things you want CIs for.
  m2 = mxRun(m2, intervals= T)
  x2 = m2$output$confidenceIntervals
  
  #recording if confidence interval does not contain 0 (then significant)
  mxp2[i,1] = ifelse(x2[1,1] > 0, 1, 
                     ifelse(x2[1,3] < 0, 1, 0))
  
  
  #getting 95% confidence interval for coverage
  Upper = OpenMx_stats2 + (OpenMx_stats3*1.96)
  Lower = OpenMx_stats2 - (OpenMx_stats3*1.96)
  
  OpenMx_coverage2[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  
  
  
  #################################
  ##### multiple imputation ######
  #################################
  
  #setting up blank imputation model
  ini1 = mice(Data1_cca, maxit=0)
  #predictor matrix
  pred = ini1$pred

  
  #method list for each variable
  meth = ini1$meth
  #leave blank for fully observed variables
  meth[c("id", "trtGrp", "Gender", "Age", "Outcome_1")] = ""
  
  #predictor variables include TrtGrp, Gender, Age, and other outcome variables
  pred["Outcome_2_05MNAR", ] = c(0, 1, 1, 0, 1, 1, 1, 1, 1, 0)
  pred["Outcome_3_05MNAR", ] = c(0, 1, 1, 1, 0, 1, 1, 1, 1, 0)
  pred["Outcome_4_05MNAR", ] = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 0)
  pred["Outcome_5_05MNAR", ] = c(0, 1, 1, 1, 1, 1, 0, 1, 1, 0)

  
  #method to impute is "norm", for normally-distributed continous variables
  meth[c("Outcome_5_05MNAR", "Outcome_4_05MNAR", "Outcome_3_05MNAR", "Outcome_2_05MNAR")] = "norm"

  #run imputation, 40 datasets
  imps = mice(Data1_cca, meth=meth, pred=pred, maxit=100, m = 40, pri=F)
  
  #getting just the appropriate imputed datasets from object
  com_MNAR05 = complete(imps, "long")
  
  #Next, changing to further long format via "Time"
  impMNAR05_ITT <- melt(com_MNAR05,
                        # ID variables - all the variables to keep but not split apart on
                        id.vars=c(".imp", ".id", "id", "trtGrp", "Gender", "Age", "Aux"),
                        # The source columns
                        measure.vars=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR" ),
                        # Name of the destination column that will identify the original
                        # column that the measurement came from
                        variable.name="Time",
                        value.name="Outcome")
  
  #Renaming values as 1, 2, and 3 in Time variable
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_1'=1")
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_2_05MNAR'=2")
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_3_05MNAR'=3")
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_4_05MNAR'=4")
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_5_05MNAR'=5")
  
  
  #creating objects to fill
  rm(implist_c)
  implist_c= NULL
  rm(implist_s)
  implist_s= NULL
  rm(implist_p)
  implist_p=NULL
  
  #loop to analyse each imputed dataset, to be pooled
  for (j in 1:40){
    impMNAR05_ITT$Time=as.integer(impMNAR05_ITT$Time)
    
    lmm_mi <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                 random = list(id = ~1), 
                 corr = corCompSymm(form= ~Time),
                 data=impMNAR05_ITT, subset=c(impMNAR05_ITT$.imp==j), na.action="na.omit",
                 method = "ML",
                 control=(msMaxIter=1000))  
    
    #recording coefficient, SE, and p value results
    implist_c = rbind(implist_c, summary(lmm_mi)$tTable[4,1])
    implist_s = rbind(implist_s, summary(lmm_mi)$tTable[4,2])
    implist_p = rbind(implist_p, summary(lmm_mi)$tTable[4,5])
  }
  
  #using rubins rules, pooling results
  combined.results <- mi.meld(q = implist_c, se = implist_s)
  imp_p = mean(implist_p)
  
  
  #recording pooled results
  implist_coef=rbind(implist_coef, combined.results$q.mi)
  implist_se=rbind(implist_se, combined.results$se.mi)
  imp_pvalue=rbind(imp_pvalue, imp_p)
  
  #getting 95% confidence interval for coverage
  estimates = combined.results$q.mi
  ses = combined.results$se.mi
  
  Upper = estimates + (ses * 1.96)
  Lower = estimates - (ses * 1.96)
  
  imp_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)

  
  
  
  
  
  
  
  
  
  #setting up blank imputation model
  ini1 = mice(Data1_cca, maxit=0)
  #predictor matrix
  pred = ini1$pred
  
  
  #method list for each variable
  meth = ini1$meth
  #leave blank for fully observed variables
  meth[c("id", "trtGrp", "Gender", "Age", "Outcome_1", "Aux")] = ""
  
  #predictor variables include TrtGrp, Gender, Age, and other outcome variables
  pred["Outcome_2_05MNAR", ] = c(0, 1, 1, 0, 1, 1, 1, 1, 1, 1)
  pred["Outcome_3_05MNAR", ] = c(0, 1, 1, 1, 0, 1, 1, 1, 1, 1)
  pred["Outcome_4_05MNAR", ] = c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1)
  pred["Outcome_5_05MNAR", ] = c(0, 1, 1, 1, 1, 1, 0, 1, 1, 1)
  
  #method to impute is "norm", for normally-distributed continous variables
  meth[c("Outcome_5_05MNAR", "Outcome_4_05MNAR", "Outcome_3_05MNAR", "Outcome_2_05MNAR")] = "norm"
  
  #run imputation, 40 datasets
  imps = mice(Data1_cca, meth=meth, pred=pred, maxit=100, m = 40, pri=F)
  
  #getting just the appropriate imputed datasets from object
  com_MNAR05 = complete(imps, "long")
  
  #Next, changing to further long format via "Time"
  impMNAR05_ITT <- melt(com_MNAR05,
                        # ID variables - all the variables to keep but not split apart on
                        id.vars=c(".imp", ".id", "id", "trtGrp", "Gender", "Age", "Aux"),
                        # The source columns
                        measure.vars=c("Outcome_1", "Outcome_2_05MNAR", "Outcome_3_05MNAR", "Outcome_4_05MNAR", "Outcome_5_05MNAR" ),
                        # Name of the destination column that will identify the original
                        # column that the measurement came from
                        variable.name="Time",
                        value.name="Outcome")
  
  #Renaming values as 1, 2, and 3 in Time variable
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_1'=1")
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_2_05MNAR'=2")
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_3_05MNAR'=3")
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_4_05MNAR'=4")
  impMNAR05_ITT$Time = recode(impMNAR05_ITT$Time, " 'Outcome_5_05MNAR'=5")
  
  
  #creating objects to fill
  rm(implist_c)
  implist_c= NULL
  rm(implist_s)
  implist_s= NULL
  rm(implist_p)
  implist_p=NULL
  
  #loop to analyse each imputed dataset, to be pooled
  for (j in 1:40){
    impMNAR05_ITT$Time=as.integer(impMNAR05_ITT$Time)
    
    lmm_mi <- lme(Outcome ~ Time + trtGrp + trtGrp*Time + Aux*trtGrp*Time, 
                  random = list(id = ~1), 
                  corr = corCompSymm(form= ~Time),
                  data=impMNAR05_ITT, subset=c(impMNAR05_ITT$.imp==j), na.action="na.omit",
                  method = "ML",
                  control=(msMaxIter=1000))  
    
    #recording coefficient, SE, and p value results
    implist_c = rbind(implist_c, summary(lmm_mi)$tTable[5,1])
    implist_s = rbind(implist_s, summary(lmm_mi)$tTable[5,2])
    implist_p = rbind(implist_p, summary(lmm_mi)$tTable[5,5])
  }
  
  #using rubins rules, pooling results
  combined.results <- mi.meld(q = implist_c, se = implist_s)
  imp_p = mean(implist_p)
  
  
  #recording pooled results
  implist_coef2 = rbind(implist_coef2, combined.results$q.mi)
  implist_se2 = rbind(implist_se2, combined.results$se.mi)
  imp_pvalue2 = rbind(imp_pvalue2, imp_p)
  
  #getting 95% confidence interval for coverage
  estimates = combined.results$q.mi
  ses = combined.results$se.mi
  
  Upper = estimates + (ses * 1.96)
  Lower = estimates - (ses * 1.96)
  
  imp_coverage2[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  print(i)
  
}




########### FINISHED!!! ##############################

## Full analysis set treatment effect
Trt1_effect=c(mean(Undeleted_coef[,1]), mean(Undeleted_SE[,1]), length(which(Undeleted_p[,1] > 0.05)))
Trt1_effect


## Slope/effect bias of other methods
#multiple imputation
imp_bias = c((Trt1_effect[1]-mean(implist_coef[,1])), (Trt1_effect[2]-mean(implist_se[,1])), length(which(imp_pvalue[,1] > 0.05)))

## Slope/effect bias of other methods
#multiple imputation
imp_bias2 = c((Trt1_effect[1]-mean(implist_coef2[,1])), (Trt1_effect[2]-mean(implist_se2[,1])), length(which(imp_pvalue2[,1] > 0.05)))


#Modified Per Protocol
LMM_bias = c((Trt1_effect[1]-mean(LMM_coef[,1])), (Trt1_effect[2]-mean(LMM_se[,1])), length(which(LMM_p[,1] > 0.05)))

#Modified Per Protocol
LMM_bias2 = c((Trt1_effect[1]-mean(LMM_coef2[,1])), (Trt1_effect[2]-mean(LMM_se2[,1])), length(which(LMM_p2[,1] > 0.05)))

#Per Protocol 
CCA_bias = c((Trt1_effect[1]-mean(CCA_coef[,1])), (Trt1_effect[2]-mean(CCA_se[,1])), length(which(CCA_p[,1] > 0.05)))

#Per Protocol 
CCA_bias2 = c((Trt1_effect[1]-mean(CCA_coef2[,1])), (Trt1_effect[2]-mean(CCA_se2[,1])), length(which(CCA_p2[,1] > 0.05)))

#80% compliant, ITT
ITT80_Bias = c((Trt1_effect[1]-mean(Switch_coef80[,1])), (Trt1_effect[2]-mean(Switch_se80[,1])), length(which(Switch_pvalue80[,1] > 0.05)))

ITT80_Bias2 = c((Trt1_effect[1]-mean(Switch_coef802[,1])), (Trt1_effect[2]-mean(Switch_se802[,1])), length(which(Switch_pvalue802[,1] > 0.05)))

#structural equation model
SEM_bias = c((Trt1_effect[1]-mean(OpenMx_coef[,1])), (Trt1_effect[2]-mean(OpenMx_SE[,1])), length(which(mxp[,1] ==0)))

#structural equation model
SEM_bias2 = c((Trt1_effect[1]-mean(OpenMx_coef2[,1])), (Trt1_effect[2]-mean(OpenMx_SE2[,1])), length(which(mxp2[,1] ==0)))



#combining results into dataset
Slope1Bias = rbind(Trt1_effect, CCA_bias, CCA_bias2, LMM_bias, LMM_bias2, SEM_bias, SEM_bias2, ITT80_Bias, ITT80_Bias2,  
                    imp_bias, imp_bias2)




### getting monte carlo standard errors ###

## modPP Coefficient MCSEs
z = rep(mean(LMM_coef[,1]), times=1000)
MCSE_LMMCoef_1 = sqrt(sum((LMM_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(LMM_se[,1]), times=1000)
MCSE_LMMSE_1 = sqrt(sum((LMM_se[,1] - z)^2)/999000)

## modPP Coefficient MCSEs
z = rep(mean(LMM_coef2[,1]), times=1000)
MCSE_LMMCoef_2 = sqrt(sum((LMM_coef2[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(LMM_se2[,1]), times=1000)
MCSE_LMMSE_2 = sqrt(sum((LMM_se2[,1] - z)^2)/999000)


## SEM Coefficient MCSEs
z = rep(mean(OpenMx_coef[,1]), times=1000)
MCSE_SEMCoef_1 = sqrt(sum((OpenMx_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(OpenMx_SE[,1]), times=1000)
MCSE_SEMSE_1 = sqrt(sum((OpenMx_SE[,1] - z)^2)/999000)

## SEM Coefficient MCSEs
z = rep(mean(OpenMx_coef2[,1]), times=1000)
MCSE_SEMCoef_2 = sqrt(sum((OpenMx_coef2[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(OpenMx_SE2[,1]), times=1000)
MCSE_SEMSE_2 = sqrt(sum((OpenMx_SE2[,1] - z)^2)/999000)


## imputation Coefficient MCSEs
z = rep(mean(implist_coef), times=1000)
MCSE_impCoef_1 = sqrt(sum((implist_coef - z)^2)/999000)
## SEs SEs
z = rep(mean(implist_se), times=1000)
MCSE_impSE_1 = sqrt(sum((implist_se - z)^2)/999000)

## imputation Coefficient MCSEs
z = rep(mean(implist_coef2), times=1000)
MCSE_impCoef_2 = sqrt(sum((implist_coef2 - z)^2)/999000)
## SEs SEs
z = rep(mean(implist_se2), times=1000)
MCSE_impSE_2 = sqrt(sum((implist_se2 - z)^2)/999000)


## PP Coefficient MCSEs
z = rep(mean(CCA_coef[,1]), times=1000)
MCSE_CCACoef_1 = sqrt(sum((CCA_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(CCA_se[,1]), times=1000)
MCSE_CCASE_1 = sqrt(sum((CCA_se[,1] - z)^2)/999000)

## PP Coefficient MCSEs
z = rep(mean(CCA_coef2[,1]), times=1000)
MCSE_CCACoef_2 = sqrt(sum((CCA_coef2[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(CCA_se2[,1]), times=1000)
MCSE_CCASE_2 = sqrt(sum((CCA_se2[,1] - z)^2)/999000)



## ITT80 Coefficient MCSEs
z = rep(mean(Switch_coef80[,1]), times=1000)
MCSE_SwitchCoef_1 = sqrt(sum((Switch_coef80[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(Switch_se80[,1]), times=1000)
MCSE_SwitchSE_1 = sqrt(sum((Switch_se80[,1] - z)^2)/999000)





#creating new dataset
Total = Slope1Bias
Total = as.data.frame(Total)

#adding variables for MCSEs for coefficient and SEs
Total$MC_SE_Coef = c(NA, MCSE_CCACoef_1, MCSE_CCACoef_2, MCSE_LMMCoef_1, MCSE_LMMCoef_2, MCSE_SEMCoef_1, MCSE_SEMCoef_2, MCSE_SwitchCoef_1, MCSE_SwitchCoef80_2, 
                     MCSE_impCoef_1, MCSE_impCoef_2)
Total$MC_SE_SEs = c(NA, MCSE_CCASE_1, MCSE_CCASE_2, MCSE_LMMSE_1, MCSE_LMMSE_2, MCSE_SEMSE_1, MCSE_SEMSE_2, MCSE_SwitchSE_1, MCSE_SwitchSE80_2, 
                    MCSE_impSE_1, MCSE_impSE_2)



#adding variable for coverage
Total$Coverage = c(
  prop.table(table(UnDel_coverage[1:1000,1]))[2],
  prop.table(table(CCA_coverage[1:1000,1]))[2],
  prop.table(table(CCA_coverage2[1:1000,1]))[2],
  prop.table(table(LMM_coverage[1:1000,1]))[2],
  prop.table(table(LMM_coverage2[1:1000,1]))[2],
  prop.table(table(OpenMx_coverage[1:1000,1]))[2],
  prop.table(table(OpenMx_coverage2[1:1000,1]))[2],
  prop.table(table(Switch_coverage80[1:1000,1]))[2],
  prop.table(table(Switch_coverage802[1:1000,1]))[2], 
  prop.table(table(imp_coverage[1:1000,1]))[2],
  prop.table(table(imp_coverage2[1:1000,1]))[2]
)



#export as excel, with percent missing and time-point introduced
WriteXLS(Total, ExcelFileName = "ITT_MNAR1000_20_trt_TP2_Aux.xlsx", col.names = TRUE, row.names=TRUE)


