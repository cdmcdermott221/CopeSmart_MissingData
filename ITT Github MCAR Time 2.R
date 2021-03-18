
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

#complete case analysis/per protocol (PP)
CCA_coef=NULL
CCA_se = NULL
CCA_p = NULL
CCA_coverage = matrix(nrow=10000, ncol=1)

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

#Intention to Treat (ITT) 80% compliant
Switch_coef80=NULL
Switch_se80 = NULL
Switch_pvalue80=NULL
Switch_coverage80 = matrix(nrow=1000, ncol=1)

#ITT, 60% compliant
Switch_coef60 =NULL
Switch_se60 = NULL
Switch_pvalue60 =NULL
Switch_coverage60 = matrix(nrow=1000, ncol=1)

#ITT, 40% compliant
Switch_coef40=NULL
Switch_se40 = NULL
Switch_pvalue40 =NULL
Switch_coverage40 = matrix(nrow=1000, ncol=1)

#ITT, 20% compliant
Switch_coef20=NULL
Switch_se20 = NULL
Switch_pvalue20 =NULL
Switch_coverage20 = matrix(nrow=1000, ncol=1)

#0% compliant, drop to baseline
Worst_coef=NULL
Worst_se = NULL
Worst_p =NULL
Worst_coverage = matrix(nrow=1000, ncol=1)

#full information maximum likelihood/structural equation model
OpenMx_coef = NULL
OpenMx_SE = NULL
OpenMx_coverage = matrix(nrow=10000, ncol=)
#for p-value for SEM
mxp = data.frame(pvalue1 = rep(2, length=5000), pvalue2 = rep(2, length=5000))




###########################################
###### Simulates 1000 X #######################
###########################################
for (i in 1:1000) {
  
  #creation of dataset with 180 individuals
  dtTrial <- genData(180)
  
  #Adding treatment, control or treatment, randomly assignd to half of participants
  dtTrial <- trtAssign(dtTrial, n=2)
  #Adding Gender, randomly assigned half male half female
  dtTrial = trtAssign(dtTrial, n=2, balanced=TRUE, grpName = "Gender")
  
  #creating a correlation matrix among the 5 repeated measurements
  C = matrix(c(1, 0.8, 0.7, 0.6, 0.5, 
               0.8, 1, 0.8, 0.7, 0.6, 
               0.7, 0.8, 1, 0.8, 0.7, 
               0.6, 0.7, 0.8, 1, 0.8, 
               0.5, 0.6, 0.7, 0.8, 1), nrow=5)
  
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
  dtTrial = dtTrial[, c("id", "trtGrp", "Outcome_1", "Outcome_2", "Outcome_3", "Outcome_4", "Outcome_5", "Gender", "Age", "ITT_2", "ITT_3", "ITT_4", "ITT_5")]
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
              method = "REML",
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
  
  #ordering by id number
  dtTrial = dtTrial[order(dtTrial$id), ]
  #ordering by treatment group
  dtTrial = dtTrial[order(dtTrial$trtGrp), ]
  
  #split by treatment group. Del = list of 1s
  Del = rep(1, length(dtTrial$Outcome_2[dtTrial$trtGrp==0]))
  
  #Del1 = blank, length of treatment group
  Del1 = rep(NA, length(dtTrial$Outcome_2[dtTrial$trtGrp==1]))
  
  #subsetting just those in the treatment group
  x = subset(dtTrial,trtGrp==1)
  
  #noncompliant, completely at random. Probability here is 20%, but can also be 5%, 10%, or 40%.
  Del1 = sample(c(0,1), length(x$Outcome_2), replace=T, prob=c(0.20, 0.8))
  
  
  #combining the two objects
  dtTrial$Del2 = c(Del, Del1)
  
  #creating blanks if Del2 is a zero. Will fill in with different noncompliant values later.
  #if someone is noncompliant at Time 2, will be noncompliant for remainder of trial
  dtTrial$Outcome_2_05MCAR = ifelse(dtTrial$Del2==0, NA, dtTrial$Outcome_2)
  dtTrial$Outcome_3_05MCAR = ifelse(dtTrial$Del2==0, NA, dtTrial$Outcome_3)
  dtTrial$Outcome_4_05MCAR = ifelse(dtTrial$Del2==0, NA, dtTrial$Outcome_4)
  dtTrial$Outcome_5_05MCAR = ifelse(dtTrial$Del2==0, NA, dtTrial$Outcome_5)
  
  #re-order by id again
  dtTrial = dtTrial[order(id), ]

  
  #creating new dataset for the "worst case" scenario
  dtTrial3 = dtTrial
  
  #filling in the blanks with the "ITT2" etc values. This represents if they switched into the control group.
  dtTrial3$Outcome_2_05MCAR = ifelse(is.na(dtTrial3$Outcome_2_05MCAR), dtTrial3$ITT_2, dtTrial3$Outcome_2_05MCAR)
  dtTrial3$Outcome_3_05MCAR = ifelse(is.na(dtTrial3$Outcome_3_05MCAR), dtTrial3$ITT_3, dtTrial3$Outcome_3_05MCAR)
  dtTrial3$Outcome_4_05MCAR = ifelse(is.na(dtTrial3$Outcome_4_05MCAR), dtTrial3$ITT_4, dtTrial3$Outcome_4_05MCAR)
  dtTrial3$Outcome_5_05MCAR = ifelse(is.na(dtTrial3$Outcome_5_05MCAR), dtTrial3$ITT_5, dtTrial3$Outcome_5_05MCAR)
  
  #just getting the variables of interest
  dtTrial2 = dtTrial3[, c("id", "trtGrp", "Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR", "Gender", "Age")]
  
  #transforming to long format
  dtTrial_long2 <- melt(dtTrial2,
                          # ID variables - all the variables to keep but not split apart on
                          id.vars=c("id", "trtGrp", "Gender", "Age"),
                          # The source columns
                          measure.vars=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR" ),
                          # Name of the destination column that will identify the original
                          # column that the measurement came from
                          variable.name="Time",
                          value.name="Outcome")
  
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_1'=1")
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_2_05MCAR'=2")
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_3_05MCAR'=3")
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_4_05MCAR'=4")
  dtTrial_long2$Time = recode(dtTrial_long2$Time, " 'Outcome_5_05MCAR'=5")
  
  dtTrial_long2$Time=as.integer(dtTrial_long2$Time)
  
  #linear mixed model of the "worst case" (switch into control group) data
  lmm_worst <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
               random = list(id = ~1),
               corr = corCompSymm(form= ~Time),
               data=dtTrial_long2, na.action="na.omit",
               method = "REML",
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
  Outcome_2_ITT= ifelse(is.na(dtTrial$Outcome_2_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*.80), dtTrial$Outcome_2)
  Outcome_3_ITT= ifelse(is.na(dtTrial$Outcome_3_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*.80 + dtTrial$S*.80), dtTrial$Outcome_3)
  Outcome_4_ITT= ifelse(is.na(dtTrial$Outcome_4_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*.80 + dtTrial$S*0.80 + dtTrial$R*0.80), dtTrial$Outcome_4)
  Outcome_5_ITT= ifelse(is.na(dtTrial$Outcome_5_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*.80 + dtTrial$S*0.80 + dtTrial$R*0.80 + dtTrial$Q*0.80), dtTrial$Outcome_5)
  
  #creating new datasets
  rm(Data1)
  Data1 = cbind(dtTrial, Outcome_2_ITT, Outcome_3_ITT, Outcome_4_ITT, Outcome_5_ITT)
  
  #For the Per Protocol dataset--with the blanks for those who are noncompliant
  Data1_cca = Data1[, c("id", "trtGrp", "Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR", "Gender", "Age")]
  
  #for the ITT80 dataset (80%compliant)
  Data1_ITT = Data1[, c("id", "trtGrp", "Outcome_1", "Outcome_2_ITT", "Outcome_3_ITT", "Outcome_4_ITT", "Outcome_5_ITT", "Gender", "Age")]
  
  
  
  #this time, the blanks are filled in with 60% of the treatment effect for the remainder of the trial
  #since 60% compliant
  Outcome_2_ITT60= ifelse(is.na(dtTrial$Outcome_2_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.60), dtTrial$Outcome_2)
  Outcome_3_ITT60= ifelse(is.na(dtTrial$Outcome_3_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.60 + dtTrial$S*0.60), dtTrial$Outcome_3)
  Outcome_4_ITT60= ifelse(is.na(dtTrial$Outcome_4_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.60 + dtTrial$S*0.60 + dtTrial$R*0.60), dtTrial$Outcome_4)
  Outcome_5_ITT60= ifelse(is.na(dtTrial$Outcome_5_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.60 + dtTrial$Q*0.60 + dtTrial$S*0.60 + dtTrial$R*0.60), dtTrial$Outcome_5)
  
  #creating ITT60 dataset
  rm(Data1)
  Data1 = cbind(dtTrial,Outcome_2_ITT60, Outcome_3_ITT60, Outcome_4_ITT60, Outcome_5_ITT60)
  Data1_ITT60 = Data1[, c("id", "trtGrp", "Outcome_1", "Outcome_2_ITT60", "Outcome_3_ITT60", "Outcome_4_ITT60", "Outcome_5_ITT60", "Gender", "Age")]
  
  
  
  #this time, the blanks are filled in with 40% of the treatment effect for the remainder of the trial
  #since 40% compliant
  Outcome_2_ITT40= ifelse(is.na(dtTrial$Outcome_2_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.40), dtTrial$Outcome_2)
  Outcome_3_ITT40= ifelse(is.na(dtTrial$Outcome_3_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.40 + dtTrial$S*0.40), dtTrial$Outcome_3)
  Outcome_4_ITT40= ifelse(is.na(dtTrial$Outcome_4_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.40 + dtTrial$S*0.40 + dtTrial$R*0.40), dtTrial$Outcome_4)
  Outcome_5_ITT40= ifelse(is.na(dtTrial$Outcome_5_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.40 + dtTrial$S*0.40 + dtTrial$R*0.40 + dtTrial$Q*0.40), dtTrial$Outcome_5)
  
  #creating ITT40 dataset
  rm(Data1)
  Data1 = cbind(dtTrial, Outcome_2_ITT40, Outcome_3_ITT40, Outcome_4_ITT40, Outcome_5_ITT40)
  Data1_ITT40 = Data1[, c("id", "trtGrp", "Outcome_1", "Outcome_2_ITT40", "Outcome_3_ITT40", "Outcome_4_ITT40", "Outcome_5_ITT40", "Gender", "Age")]
  
  
  
  #this time, the blanks are filled in with 20% of the treatment effect for the remainder of the trial
  #since 20% compliant
  Outcome_2_ITT20= ifelse(is.na(dtTrial$Outcome_2_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.20), dtTrial$Outcome_2)
  Outcome_3_ITT20= ifelse(is.na(dtTrial$Outcome_5_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.20 + dtTrial$S*0.20), dtTrial$Outcome_3)
  Outcome_4_ITT20= ifelse(is.na(dtTrial$Outcome_5_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.20 + dtTrial$S*0.20 + dtTrial$R*0.20), dtTrial$Outcome_4)
  Outcome_5_ITT20= ifelse(is.na(dtTrial$Outcome_5_05MCAR), (dtTrial$Outcome_1 + dtTrial$Tr*0.20 + dtTrial$Q*0.20 + dtTrial$S*0.20 + dtTrial$R*0.20), dtTrial$Outcome_5)
  
  #creating ITT20 dataset
  rm(Data1)
  Data1 = cbind(dtTrial, Outcome_2_ITT20, Outcome_3_ITT20,Outcome_4_ITT20, Outcome_5_ITT20)
  Data1_ITT20 = Data1[, c("id", "trtGrp", "Outcome_1", "Outcome_2_ITT20", "Outcome_3_ITT20", "Outcome_4_ITT20", "Outcome_5_ITT20", "Gender", "Age")]
  
  

  
  
  
  #transforming modified per protocol dataset to long format
  Data1_long <- melt(Data1_cca,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("id", "trtGrp", "Gender", "Age"),
                     # The source columns
                     measure.vars=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="Outcome")
  
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_1'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_2_05MCAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_3_05MCAR'=3")
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_4_05MCAR'=4")
  Data1_long$Time = recode(Data1_long$Time, " 'Outcome_5_05MCAR'=5")
  
  
  #transforming ITT80 to long format
  Data1_long_ITT <- melt(Data1_ITT,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("id", "trtGrp", "Gender", "Age"),
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
  
  
  
  #transforming ITT60 to long format
  Data1_long_ITT60 <- melt(Data1_ITT60,
                           # ID variables - all the variables to keep but not split apart on
                           id.vars=c("id", "trtGrp", "Gender", "Age"),
                           # The source columns
                           measure.vars=c("Outcome_1", "Outcome_2_ITT60", "Outcome_3_ITT60", "Outcome_4_ITT60", "Outcome_5_ITT60" ),
                           # Name of the destination column that will identify the original
                           # column that the measurement came from
                           variable.name="Time",
                           value.name="Outcome")
  
  Data1_long_ITT60$Time = recode(Data1_long_ITT60$Time, " 'Outcome_1'=1")
  Data1_long_ITT60$Time = recode(Data1_long_ITT60$Time, " 'Outcome_2_ITT60'=2")
  Data1_long_ITT60$Time = recode(Data1_long_ITT60$Time, " 'Outcome_3_ITT60'=3")
  Data1_long_ITT60$Time = recode(Data1_long_ITT60$Time, " 'Outcome_4_ITT60'=4")
  Data1_long_ITT60$Time = recode(Data1_long_ITT60$Time, " 'Outcome_5_ITT60'=5")
  
  
  
  #transforming ITT40 to long format
  Data1_long_ITT40 <- melt(Data1_ITT40,
                           # ID variables - all the variables to keep but not split apart on
                           id.vars=c("id", "trtGrp", "Gender", "Age"),
                           # The source columns
                           measure.vars=c("Outcome_1", "Outcome_2_ITT40", "Outcome_3_ITT40", "Outcome_4_ITT40", "Outcome_5_ITT40" ),
                           # Name of the destination column that will identify the original
                           # column that the measurement came from
                           variable.name="Time",
                           value.name="Outcome")
  
  Data1_long_ITT40$Time = recode(Data1_long_ITT40$Time, " 'Outcome_1'=1")
  Data1_long_ITT40$Time = recode(Data1_long_ITT40$Time, " 'Outcome_2_ITT40'=2")
  Data1_long_ITT40$Time = recode(Data1_long_ITT40$Time, " 'Outcome_3_ITT40'=3")
  Data1_long_ITT40$Time = recode(Data1_long_ITT40$Time, " 'Outcome_4_ITT40'=4")
  Data1_long_ITT40$Time = recode(Data1_long_ITT40$Time, " 'Outcome_5_ITT40'=5")
  
  
  
  
  #transforming ITT20 to long format
  Data1_long_ITT20 <- melt(Data1_ITT20,
                           # ID variables - all the variables to keep but not split apart on
                           id.vars=c("id", "trtGrp", "Gender", "Age"),
                           # The source columns
                           measure.vars=c("Outcome_1", "Outcome_2_ITT20", "Outcome_3_ITT20", "Outcome_4_ITT20", "Outcome_5_ITT20" ),
                           # Name of the destination column that will identify the original
                           # column that the measurement came from
                           variable.name="Time",
                           value.name="Outcome")
  
  Data1_long_ITT20$Time = recode(Data1_long_ITT20$Time, " 'Outcome_1'=1")
  Data1_long_ITT20$Time = recode(Data1_long_ITT20$Time, " 'Outcome_2_ITT20'=2")
  Data1_long_ITT20$Time = recode(Data1_long_ITT20$Time, " 'Outcome_3_ITT20'=3")
  Data1_long_ITT20$Time = recode(Data1_long_ITT20$Time, " 'Outcome_4_ITT20'=4")
  Data1_long_ITT20$Time = recode(Data1_long_ITT20$Time, " 'Outcome_5_ITT20'=5")
  
  
  
  
  
  
  
  ############################
  ### Per Protocol ##########
  ############################
  
  Data1_long$Time=as.integer(Data1_long$Time)
  
  #liner mixed model, modified per protocol
  lmm_modpp <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                random = list(id = ~1),
                corr = corCompSymm(form= ~Time),
                data=Data1_long, na.action="na.omit",
                method = "REML",
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
  
  
  
  
  ####################################
  ##### complete case analysis #####
  ####### Per Protocol ############
  
  #forcing out participants who were noncompliant
  Data2 = na.omit(Data1_cca)
  
  #transforming to long format
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("id", "trtGrp", "Gender", "Age"),
                     # The source columns
                     measure.vars=c("Outcome_1", "Outcome_3_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="Outcome")
  
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_1'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_3_05MCAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_3_05MCAR'=3")
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_4_05MCAR'=4")
  Data2_long$Time = recode(Data2_long$Time, " 'Outcome_5_05MCAR'=5")
  
  
  Data2_long$Time=as.integer(Data2_long$Time)
  
  #linear mixed model of per protocol
  lmm_pp <-  lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                random = list(id = ~1),
                corr = corCompSymm(form= ~Time),
                data=Data2_long, na.action="na.omit",
                method = "REML",
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
  
  
  
  
  
  ######## ITT, 80% compliant ############
  
  Data1_long_ITT$Time=as.integer(Data1_long_ITT$Time)
  
  #linear mixed model on ITT80 data
  lmm_80 <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                   random = list(id = ~1),
                   corr = corCompSymm(form= ~Time),
                   data=Data1_long_ITT, na.action="na.omit",
                   method = "REML",
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
  
  
  
  
  ######## ITT, 60% compliant ############
  
  Data1_long_ITT60$Time=as.integer(Data1_long_ITT60$Time)
  
  #linear mixed model, 60% compliant
  itt_60 <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                     random = list(id = ~1),
                     corr = corCompSymm(form= ~Time),
                     data=Data1_long_ITT60, na.action="na.omit",
                     method = "REML",
                     control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(itt_60)$tTable[4,1]
  TestSum2 = summary(itt_60)$tTable[4,2]
  TestSum3 = summary(itt_60)$tTable[4,5]
  
  Switch_coef60 = rbind(Switch_coef60, TestSum)
  Switch_se60 = rbind(Switch_se60, TestSum2)
  Switch_pvalue60 = rbind(Switch_pvalue60, TestSum3)
  

  #getting 95% confidence interval for coverage  
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  Switch_coverage60[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  ######## ITT, 40% compliant ############
  
  Data1_long_ITT40$Time=as.integer(Data1_long_ITT40$Time)
  
  #linear mixed model, 40% compliant
  lmm_40 <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                     random = list(id = ~1),
                     corr = corCompSymm(form= ~Time),
                     data=Data1_long_ITT40, na.action="na.omit",
                     method = "REML",
                     control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(lmm_40)$tTable[4,1]
  TestSum2 = summary(lmm_40)$tTable[4,2]
  TestSum3 = summary(lmm_40)$tTable[4,5]
  
  Switch_coef40 = rbind(Switch_coef40, TestSum)
  Switch_se40 = rbind(Switch_se40, TestSum2)
  Switch_pvalue40 = rbind(Switch_pvalue40, TestSum3)

  
  #getting 95% confidence interval for coverage
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  Switch_coverage40[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  ######## ITT, 20% compliant ############
  
  Data1_long_ITT20$Time=as.integer(Data1_long_ITT20$Time)
  
  #linear mixed model, 40% compliant
  lmm_20 <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                     random = list(id = ~1),
                     corr = corCompSymm(form= ~Time),
                     data=Data1_long_ITT20, na.action="na.omit",
                     method = "REML",
                     control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(lmm_20)$tTable[4,1]
  TestSum2 = summary(lmm_20)$tTable[4,2]
  TestSum3 = summary(lmm_20)$tTable[4,5]
  
  Switch_coef20 = rbind(Switch_coef20, TestSum)
  Switch_se20 = rbind(Switch_se20, TestSum2)
  Switch_pvalue20 = rbind(Switch_pvalue20, TestSum3)
  

  #getting 95% confidence interval for coverage  
  Upper = TestSum + (TestSum2*1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  Switch_coverage20[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  ################################################
  ##### Last observation carried forward  ########
  ## also, 0% compliant, keep treatment effect###
  ################################################
  
  Data3=Data1_cca
  
  #replacing Time-point 2,,3,4,5, if noncompliant, with the Time-point 1 outcome
  LOCF2 = ifelse(is.na(Data3$Outcome_2_05MCAR), Data3$Outcome_1, Data3$Outcome_2_05MCAR)
  LOCF3 = ifelse(is.na(Data3$Outcome_3_05MCAR), Data3$Outcome_1, Data3$Outcome_3_05MCAR)
  LOCF4 = ifelse(is.na(Data3$Outcome_4_05MCAR), Data3$Outcome_1, Data3$Outcome_4_05MCAR)
  LOCF5 = ifelse(is.na(Data3$Outcome_5_05MCAR), Data3$Outcome_1, Data3$Outcome_5_05MCAR)
  Data3=cbind(Data3, LOCF2, LOCF3, LOCF4, LOCF5)
  
  #just getting new variables
  Data3=Data3[,c("id", "trtGrp", "Outcome_1", "LOCF2", "LOCF3", "LOCF4", "LOCF5", "Gender", "Age")]
  
  #transforming into long format
  Data3_long <- melt(Data3,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("id", "trtGrp", "Gender", "Age"),
                     # The source columns
                     measure.vars=c("Outcome_1", "LOCF2", "LOCF3", "LOCF4", "LOCF5" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="Outcome")
  
  Data3_long$Time = recode(Data3_long$Time, " 'Outcome_1'=1")
  Data3_long$Time = recode(Data3_long$Time, " 'LOCF2'=2")
  Data3_long$Time = recode(Data3_long$Time, " 'LOCF3'=3")
  Data3_long$Time = recode(Data3_long$Time, " 'LOCF4'=4")
  Data3_long$Time = recode(Data3_long$Time, " 'LOCF5'=5")
  
  
  Data3_long$Time=as.integer(Data3_long$Time)
  
  #linear mixed model of locf
  lmm_locf <-  lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                random = list(id = ~1),
                corr = corCompSymm(form= ~Time),
                data=Data3_long, na.action="na.omit",
                method = "REML",
                control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(lmm_locf)$tTable[4,1]
  TestSum2 = summary(lmm_locf)$tTable[4,2]
  TestSum3 = summary(lmm_locf)$tTable[4,5]
  
  LOCF_coef = rbind(LOCF_coef, TestSum)
  LOCF_se = rbind(LOCF_se, TestSum2)
  LOCF_p = rbind(LOCF_p, TestSum3)
  

  #95% confidence interval for coverage
  Upper = TestSum + (TestSum2 * 1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  LOCF_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  
  
  ################################################
  ############ Mean Imputation  ###################
  ################################################
  
  
  Data4=Data1_cca
  
  #replacing the blank values with the mean of the treatment group at that time-point
  Data4$Outcome_2_05MCAR[is.na(Data4$Outcome_2_05MCAR)] <- mean(Data4$Outcome_2_05MCAR[Data4$trtGrp==1], na.rm = TRUE)
  Data4$Outcome_3_05MCAR[is.na(Data4$Outcome_3_05MCAR)] <- mean(Data4$Outcome_3_05MCAR[Data4$trtGrp==1], na.rm = TRUE)
  Data4$Outcome_4_05MCAR[is.na(Data4$Outcome_4_05MCAR)] <- mean(Data4$Outcome_4_05MCAR[Data4$trtGrp==1], na.rm = TRUE)
  Data4$Outcome_5_05MCAR[is.na(Data4$Outcome_5_05MCAR)] <- mean(Data4$Outcome_5_05MCAR[Data4$trtGrp==1], na.rm = TRUE)
  
  
  #transforming to long format
  Data4_long <- melt(Data4,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("id", "trtGrp", "Gender", "Age"),
                     # The source columns
                     measure.vars=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="Outcome")
  
  Data4_long$Time = recode(Data4_long$Time, " 'Outcome_1'=1")
  Data4_long$Time = recode(Data4_long$Time, " 'Outcome_2_05MCAR'=2")
  Data4_long$Time = recode(Data4_long$Time, " 'Outcome_3_05MCAR'=3")
  Data4_long$Time = recode(Data4_long$Time, " 'Outcome_4_05MCAR'=4")
  Data4_long$Time = recode(Data4_long$Time, " 'Outcome_5_05MCAR'=5")
  
  
  Data4_long$Time=as.integer(Data4_long$Time)
  
  #linear mixed model of mean imputation
  lmm_mean <-  lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                random = list(id = ~1),
                corr = corCompSymm(form= ~Time),
                data=Data4_long, na.action="na.omit",
                method = "REML",
                control=(msMaxIter=100))
  
  #recording results
  TestSum = summary(lmm_mean)$tTable[4,1]
  TestSum2 = summary(lmm_mean)$tTable[4,2]
  TestSum3 = summary(lmm_mean)$tTable[4,5]
  
  Mean_coef = rbind(Mean_coef, TestSum)
  Mean_se = rbind(Mean_se, TestSum2)
  Mean_p = rbind(Mean_p, TestSum3)
  

  #95% confidence interval for coverage  
  Upper = TestSum + (TestSum2 * 1.96)
  Lower = TestSum - (TestSum2 * 1.96)
  
  Mean_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  
  

  #################################################
  ######## structural equation model #############
  #################################################
  
  Data5=Data1_cca 
  
  #labelling manifest variables (observed variables) and latent variables (I = intercept, S = slope)
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", 
                                                  "Outcome_5_05MCAR", "trtGrp"),
    latentVars=c("I", "S"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR"), arrows=2, free=T, values=c(1,1,1,1,1),
           labels=c("variance1","variance1", "variance1", "variance1", "variance1")),
    mxPath(from="Outcome_1", to=c("Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR"), arrows=2, free=T, values=c(.8, .8, .8, .8),
           labels=c("covar1", "covar1", "covar1", "covar1")),
    mxPath(from="Outcome_2_05MCAR", to=c("Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR"), arrows=2, free=T, values=c(.8,.8,.8),
           labels=c("covar1", "covar1", "covar1")),
    mxPath(from="Outcome_3_05MCAR", to=c("Outcome_4_05MCAR", "Outcome_5_05MCAR"), arrows=2, free=T, values=c(.8,.8),
           labels=c("covar1", "covar1")),
    mxPath(from="Outcome_4_05MCAR", to=c("Outcome_5_05MCAR"), arrows=2, free=T, values=.8,
           labels="covar1"),
    #one slope (S) from T1 to T5
    mxPath(from=c("I", "S"), arrows=2, connect="unique.pairs", free=F, values=0), 
    #for latent growth curves, the intercept loadings have to be all 1s from each repeated measure
    mxPath(from="I", to=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR"), arrows=1, free=F, 
           values=c(1, 1, 1,1,1)),
    #and the slope loadings have to be 0,1,2,3,4 from earliest to latest repeated measure
    mxPath(from="S", to=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR"), arrows=1, free=F, 
           values=c(0,1,2,3,4)),
    #manifest means not estimated
    mxPath(from="one", to=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR"), arrows=1, free=F,
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
  mxp[i,1] = ifelse(x2[1,1] > 0, 1, 0)
  
  
  #getting 95% confidence interval for coverage
  Upper = OpenMx_stats2 + (OpenMx_stats3*1.96)
  Lower = OpenMx_stats2 - (OpenMx_stats3*1.96)
  
  OpenMx_coverage[i, 1] = ifelse(Coverage < Upper & Coverage > Lower, T, F)
  
  
  
  
  
  
  
  
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
  pred["Outcome_2_05MCAR", ] = c(0, 1, 1, 0, 1, 1, 1, 1, 1)
  pred["Outcome_3_05MCAR", ] = c(0, 1, 1, 1, 0, 1, 1, 1, 1)
  pred["Outcome_4_05MCAR", ] = c(0, 1, 1, 1, 1, 0, 1, 1, 1)
  pred["Outcome_5_05MCAR", ] = c(0, 1, 1, 1, 1, 1, 0, 1, 1)
  
  #method to impute is "norm", for normally-distributed continous variables
  meth[c("Outcome_5_05MCAR", "Outcome_4_05MCAR", "Outcome_3_05MCAR", "Outcome_2_05MCAR")] = "norm"

  #run imputation, 40 datasets
  imps = mice(Data1_cca, meth=meth, pred=pred, maxit=10, m = 40, pri=F)
  
  #getting just the appropriate imputed datasets from object
  com_MCAR05 = complete(imps, "long")
  
  #Next, changing to further long format via "Time"
  impMCAR05_ITT <- melt(com_MCAR05,
                        # ID variables - all the variables to keep but not split apart on
                        id.vars=c(".imp", ".id", "id", "trtGrp", "Gender", "Age"),
                        # The source columns
                        measure.vars=c("Outcome_1", "Outcome_2_05MCAR", "Outcome_3_05MCAR", "Outcome_4_05MCAR", "Outcome_5_05MCAR" ),
                        # Name of the destination column that will identify the original
                        # column that the measurement came from
                        variable.name="Time",
                        value.name="Outcome")
  
  #Renaming values as 1, 2, and 3 in Time variable
  impMCAR05_ITT$Time = recode(impMCAR05_ITT$Time, " 'Outcome_1'=1")
  impMCAR05_ITT$Time = recode(impMCAR05_ITT$Time, " 'Outcome_2_05MCAR'=2")
  impMCAR05_ITT$Time = recode(impMCAR05_ITT$Time, " 'Outcome_3_05MCAR'=3")
  impMCAR05_ITT$Time = recode(impMCAR05_ITT$Time, " 'Outcome_4_05MCAR'=4")
  impMCAR05_ITT$Time = recode(impMCAR05_ITT$Time, " 'Outcome_5_05MCAR'=5")
  
  
  #creating objects to fill
  rm(implist_c)
  implist_c= NULL
  rm(implist_s)
  implist_s= NULL
  rm(implist_p)
  implist_p=NULL
  
  #loop to analyse each imputed dataset, to be pooled
  for (j in 1:40){
    impMCAR05_ITT$Time=as.integer(impMCAR05_ITT$Time)
    
    lmm_mi <- lme(Outcome ~ Time + trtGrp + trtGrp*Time, 
                 random = list(id = ~1), 
                 corr = corCompSymm(form= ~Time),
                 data=impMCAR05_ITT, subset=c(impMCAR05_ITT$.imp==j), na.action="na.omit",
                 method = "REML",
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

  
  
  
}




########### FINISHED!!! ##############################

## Full analysis set treatment effect
Trt1_effect=c(mean(Undeleted_coef[,1]), mean(Undeleted_SE[,1]), length(which(Undeleted_p[,1] > 0.05)))



## Slope/effect bias of other methods
#multiple imputation
imp_bias = c((Trt1_effect[1]-mean(implist_coef[,1])), (Trt1_effect[2]-mean(implist_se[,1])), length(which(imp_pvalue[,1] > 0.05)))

#Modified Per Protocol
LMM_bias = c((Trt1_effect[1]-mean(LMM_coef[,1])), (Trt1_effect[2]-mean(LMM_se[,1])), length(which(LMM_p[,1] > 0.05)))

#Per Protocol 
CCA_bias = c((Trt1_effect[1]-mean(CCA_coef[,1])), (Trt1_effect[2]-mean(CCA_se[,1])), length(which(CCA_p[,1] > 0.05)))

#0% compliant, last observation carried forward
LOCF_bias = c((Trt1_effect[1]-mean(LOCF_coef[,1])), (Trt1_effect[2]-mean(LOCF_se[,1])), length(which(LOCF_p[,1] > 0.05)))

#mean imputation
Mean_bias = c((Trt1_effect[1]-mean(Mean_coef[,1])), (Trt1_effect[2]-mean(Mean_se[,1])), length(which(Mean_p[,1] > 0.05)))

#0% compliant, switch into control group
Worst_bias = c((Trt1_effect[1]-mean(Worst_coef[,1])), (Trt1_effect[2]-mean(Worst_se[,1])), length(which(Worst_p[,1] > 0.05)))

#80% compliant, ITT
ITT80_Bias = c((Trt1_effect[1]-mean(Switch_coef80[,1])), (Trt1_effect[2]-mean(Switch_se80[,1])), length(which(Switch_pvalue80[,1] > 0.05)))

#60% compliant, ITT
ITT60_Bias = c((Trt1_effect[1]-mean(Switch_coef60[,1])), (Trt1_effect[2]-mean(Switch_se60[,1])), length(which(Switch_pvalue60[,1] > 0.05)))

#40% compliant, ITT
ITT40_Bias = c((Trt1_effect[1]-mean(Switch_coef40[,1])), (Trt1_effect[2]-mean(Switch_se40[,1])), length(which(Switch_pvalue40[,1] > 0.05)))

#20% compliant, ITT
ITT20_Bias = c((Trt1_effect[1]-mean(Switch_coef20[,1])), (Trt1_effect[2]-mean(Switch_se20[,1])), length(which(Switch_pvalue20[,1] > 0.05)))

#structural equation model
SEM_bias = c((Trt1_effect[1]-mean(OpenMx_coef[,1])), (Trt1_effect[2]-mean(OpenMx_SE[,1])), length(which(mxp[,1] ==0)))



#combining results into dataset
Slope1Bias = rbind(Trt1_effect, CCA_bias, LMM_bias, SEM_bias, ITT80_Bias, ITT60_Bias, ITT40_Bias, 
                   ITT20_Bias, Worst_bias, imp_bias, LOCF_bias, Mean_bias)




### getting monte carlo standard errors ###

## modPP Coefficient MCSEs
z = rep(mean(LMM_coef[,1]), times=1000)
MCSE_LMMCoef_1 = sqrt(sum((LMM_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(LMM_se[,1]), times=1000)
MCSE_LMMSE_1 = sqrt(sum((LMM_se[,1] - z)^2)/999000)


## SEM Coefficient MCSEs
z = rep(mean(OpenMx_coef[,1]), times=1000)
MCSE_SEMCoef_1 = sqrt(sum((OpenMx_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(OpenMx_SE[,1]), times=1000)
MCSE_SEMSE_1 = sqrt(sum((OpenMx_SE[,1] - z)^2)/999000)


## imputation Coefficient MCSEs
z = rep(mean(implist_coef), times=1000)
MCSE_impCoef_1 = sqrt(sum((implist_coef - z)^2)/999000)
## SEs SEs
z = rep(mean(implist_se), times=1000)
MCSE_impSE_1 = sqrt(sum((implist_se - z)^2)/999000)


## PP Coefficient MCSEs
z = rep(mean(CCA_coef[,1]), times=1000)
MCSE_CCACoef_1 = sqrt(sum((CCA_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(CCA_se[,1]), times=1000)
MCSE_CCASE_1 = sqrt(sum((CCA_se[,1] - z)^2)/999000)


## LOCF Coefficient MCSEs
z = rep(mean(LOCF_coef[,1]), times=1000)
MCSE_LOCFCoef_1 = sqrt(sum((LOCF_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(LOCF_se[,1]), times=1000)
MCSE_LOCFSE_1 = sqrt(sum((LOCF_se[,1] - z)^2)/999000)


## MEAN imp Coefficient MCSEs
z = rep(mean(Mean_coef[,1]), times=1000)
MCSE_MeanCoef_1 = sqrt(sum((Mean_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(Mean_se[,1]), times=1000)
MCSE_MeanSE_1 = sqrt(sum((Mean_se[,1] - z)^2)/999000)


## ITT80 Coefficient MCSEs
z = rep(mean(Switch_coef[,1]), times=1000)
MCSE_SwitchCoef_1 = sqrt(sum((Switch_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(Switch_se[,1]), times=1000)
MCSE_SwitchSE_1 = sqrt(sum((Switch_se[,1] - z)^2)/999000)


## ITT60 Coefficient MCSEs
z = rep(mean(Switch_coef60[,1]), times=1000)
MCSE_SwitchCoef60_1 = sqrt(sum((Switch_coef60[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(Switch_se60[,1]), times=1000)
MCSE_SwitchSE60_1 = sqrt(sum((Switch_se60[,1] - z)^2)/999000)


## ITT80 Coefficient MCSEs
z = rep(mean(Switch_coef40[,1]), times=1000)
MCSE_SwitchCoef40_1 = sqrt(sum((Switch_coef40[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(Switch_se40[,1]), times=1000)
MCSE_SwitchSE40_1 = sqrt(sum((Switch_se40[,1] - z)^2)/999000)


## ITT20  Coefficient MCSEs
z = rep(mean(Switch_coef20[,1]), times=1000)
MCSE_SwitchCoef20_1 = sqrt(sum((Switch_coef20[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(Switch_se20[,1]), times=1000)
MCSE_SwitchSE20_1 = sqrt(sum((Switch_se20[,1] - z)^2)/999000)


## Worst Case Coefficient MCSEs
z = rep(mean(Worst_coef[,1]), times=1000)
MCSE_WorstCoef_1 = sqrt(sum((Worst_coef[,1] - z)^2)/999000)
## SEs SEs
z = rep(mean(Worst_se[,1]), times=1000)
MCSE_WorstSE_1 = sqrt(sum((Worst_se[,1] - z)^2)/999000)





#creating new dataset
Total = Slope1Bias
Total = as.data.frame(Total)

#adding variables for MCSEs for coefficient and SEs
Total$MC_SE_Coef = c(NA, MCSE_CCACoef_1, MCSE_LMMCoef_1, MCSE_SEMCoef_1, MCSE_SwitchCoef_1, MCSE_SwitchCoef60_1, MCSE_SwitchCoef40_1, 
                     MCSE_SwitchCoef20_1, MCSE_WorstCoef_1, MCSE_impCoef_1, MCSE_LOCFCoef_1, MCSE_MeanCoef_1)
Total$MC_SE_SEs = c(NA, MCSE_CCASE_1, MCSE_LMMSE_1, MCSE_SEMSE_1, MCSE_SwitchSE_1, MCSE_SwitchSE60_1, MCSE_SwitchSE40_1, 
                    MCSE_SwitchSE20_1, MCSE_WorstSE_1, MCSE_impSE_1, MCSE_LOCFSE_1, MCSE_MeanSE_1)



#adding variable for coverage
Total$Coverage = c(
  prop.table(table(UnDel_coverage[1:1000,1]))[2],
  prop.table(table(CCA_coverage[1:1000,1]))[2],
  prop.table(table(LMM_coverage[1:1000,1]))[2],
  prop.table(table(OpenMx_coverage[1:1000,1]))[2],
  prop.table(table(Switch_coverage80[1:1000,1]))[2],
  prop.table(table(Switch_coverage60[1:1000,1]))[2],
  prop.table(table(Switch_coverage40[1:1000,1]))[2],
  prop.table(table(Switch_coverage20[1:1000,1]))[2], 
  prop.table(table(Worst_coverage[1:1000,1]))[2], 
  prop.table(table(imp_coverage[1:1000,1]))[2],
  prop.table(table(LOCF_coverage[1:1000,1]))[2],
  prop.table(table(Mean_coverage[1:1000,1]))[2]
)



#export as excel, with percent missing and time-point introduced
WriteXLS(Total, ExcelFileName = "ITT_MCAR1000_20_trt_TP2.xlsx", col.names = TRUE, row.names=TRUE)


