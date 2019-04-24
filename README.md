# CopeSmart_MissingData
Simulations of missing data within cluster-randomised trials under different mechanisms



# Missing Completely at Random, 5% deleted
######## MCAR 5% ############

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)


OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2)



###### LOOPS 100X #######################
for (i in 1:100) {
  
  # creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  #################################################
  # Deletions
  prop.m = .05  # 5% missingness
  MCAR   = runif(dtStudent1$ESA_T1, min=0, max=1)
  ESA_1_05MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T1)  # unrelated to anything
  
  
  MCAR   = runif(dtStudent1$ESA_T2, min=0, max=1)
  ESA_2_05MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T2)  # unrelated to anything
  
  
  MCAR   = runif(dtStudent1$ESA_T3, min=0, max=1)
  ESA_3_05MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T3)  # unrelated to anything
  
  Data1 = cbind(dtStudent1, ESA_1_05MCAR, ESA_2_05MCAR, ESA_3_05MCAR)
  Data1 = Data1[, c("School", "trtGrp", "idChild", "ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_05MCAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_05MCAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_05MCAR'=3")
  
  
  # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_05MCAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_05MCAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_05MCAR'=3")
  
  
  
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_05MCAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_05MCAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_05MCAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR")] = "norm"
  meth
  
  # 40 imputed datasets, 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MCAR05 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMCAR05_long_01 <- melt(com_MCAR05,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                            # The source columns
                            measure.vars=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR" ),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="Time",
                            value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMCAR05_long_01$Time = recode(impMCAR05_long_01$Time, " 'ESA_1_05MCAR'=1")
  impMCAR05_long_01$Time = recode(impMCAR05_long_01$Time, " 'ESA_2_05MCAR'=2")
  impMCAR05_long_01$Time = recode(impMCAR05_long_01$Time, " 'ESA_3_05MCAR'=3")
  
  # to change to Imputation_
  impMCAR05_long_01= cbind(impMCAR05_long_01, Imputation_ = as.integer(impMCAR05_long_01$.imp)-1)               
  
  # export imputed data and dictionary from R to SPSS
  WriteXLS(impMCAR05_long_01, ExcelFileName = paste("100_MCAR05_Change_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  
  
  
  
  ###### Structural Equation Model ########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    # compound symmetry covariance structure
    mxPath(from=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_05MCAR", to=c("ESA_2_05MCAR", "ESA_3_05MCAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_05MCAR", to="ESA_3_05MCAR", arrows=2, free=T, values=.5, labels="covar"),
    # two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_05MCAR", "ESA_2_05MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_05MCAR", "ESA_3_05MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)
  statistics
  
  
  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_05MCAR", to=c("ESA_2_05MCAR", "ESA_3_05MCAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_05MCAR", to="ESA_3_05MCAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_05MCAR", "ESA_2_05MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_05MCAR", "ESA_3_05MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_05MCAR", "ESA_2_05MCAR", "ESA_3_05MCAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  
  
  
}





MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)


#export deleted and SEM coefficients and data, MCAR 5% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MCAR05_Deletions_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MCAR05_Deletions_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MCAR05_SEM_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MCAR05_SEM_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MCAR05_Undeleted_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MCAR05_Undeleted_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MCAR05_CCA_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MCAR05_CCA_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MCAR05_OpenMx_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MCAR05_OpenMx_SE.xls", col.names = TRUE, row.names=FALSE)




























# Missing completely at random, 10% deleted
######## MCAR 10% ############################################################

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)

OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2)





############# CHANGE OVER TIME ###################
###### LOOPS 100 X, MCAR 10% #######################
for (i in 1:100) {
  
  # creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  #################################################
  # Deletions
  prop.m = .10  # 10% missingness
  MCAR   = runif(dtStudent1$ESA_T1, min=0, max=1)
  ESA_1_10MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T1)  # unrelated to anything
  
  
  MCAR   = runif(dtStudent1$ESA_T2, min=0, max=1)
  ESA_2_10MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T2)  # unrelated to anything
  
  
  MCAR   = runif(dtStudent1$ESA_T3, min=0, max=1)
  ESA_3_10MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T3)  # unrelated to anything
  
  Data1 = cbind(dtStudent1, ESA_1_10MCAR, ESA_2_10MCAR, ESA_3_10MCAR)
  Data1 = Data1[, c("School", "trtGrp", "idChild", "ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_10MCAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_10MCAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_10MCAR'=3")
  
  
  
   # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_10MCAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_10MCAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_10MCAR'=3")
  
  
  # Again analyse using MLM
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_10MCAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_10MCAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_10MCAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR")] = "norm"
  meth
  
  # 40 imputed datasets, 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MCAR10 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMCAR10_long_01 <- melt(com_MCAR10,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                            # The source columns
                            measure.vars=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR" ),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="Time",
                            value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMCAR10_long_01$Time = recode(impMCAR10_long_01$Time, " 'ESA_1_10MCAR'=1")
  impMCAR10_long_01$Time = recode(impMCAR10_long_01$Time, " 'ESA_2_10MCAR'=2")
  impMCAR10_long_01$Time = recode(impMCAR10_long_01$Time, " 'ESA_3_10MCAR'=3")
  
  # to change to Imputation_
  impMCAR10_long_01= cbind(impMCAR10_long_01, Imputation_ = as.integer(impMCAR10_long_01$.imp)-1)               
  
  # export imputed data and dictionary from R to SPSS
  WriteXLS(impMCAR10_long_01, ExcelFileName = paste("100_MCAR10_Change_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  
  
  
  
  
  
  
  
  ###### Structural Equation Model ################
  
  ###### with covariates
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_10MCAR", to=c("ESA_2_10MCAR", "ESA_3_10MCAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_10MCAR", to="ESA_3_10MCAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_10MCAR", "ESA_2_10MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_10MCAR", "ESA_3_10MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)
  
  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_10MCAR", to=c("ESA_2_10MCAR", "ESA_3_10MCAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_10MCAR", to="ESA_3_10MCAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_10MCAR", "ESA_2_10MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_10MCAR", "ESA_3_10MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_10MCAR", "ESA_2_10MCAR", "ESA_3_10MCAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  
  
  
}





MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)

#export deleted and SEM coefficients and data, MCAR 10% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MCAR10_Deletions_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MCAR10_Deletions_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MCAR10_SEM_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MCAR10_SEM_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MCAR10_Undeleted_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MCAR10_Undeleted_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MCAR10_CCA_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MCAR10_CCA_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MCAR10_OpenMx_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MCAR10_OpenMx_SE.xls", col.names = TRUE, row.names=FALSE)





































######## MCAR 20% ############################################################

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)


OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2)




############# CHANGE OVER TIME ###################
###### LOOPS 100 X, MCAR 10% #######################
for (i in 1:100) {
  
  # creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  
  #################################################
  # Deletions
  prop.m = .20  # 10% missingness
  MCAR   = runif(dtStudent1$ESA_T1, min=0, max=1)
  ESA_1_20MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T1)  # unrelated to anything
  
  
  MCAR   = runif(dtStudent1$ESA_T2, min=0, max=1)
  ESA_2_20MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T2)  # unrelated to anything
  
  
  MCAR   = runif(dtStudent1$ESA_T3, min=0, max=1)
  ESA_3_20MCAR = ifelse(MCAR<prop.m, NA, dtStudent1$ESA_T3)  # unrelated to anything
  
  Data1 = cbind(dtStudent1, ESA_1_20MCAR, ESA_2_20MCAR, ESA_3_20MCAR)
  Data1 = Data1[, c("School", "trtGrp", "idChild", "ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_20MCAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_20MCAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_20MCAR'=3")
  
  
  
   # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_20MCAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_20MCAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_20MCAR'=3")
  
  
  # Again run MLM
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_20MCAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_20MCAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_20MCAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR")] = "norm"
  meth
  
  
  # 40 imputed datasets and 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MCAR20 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMCAR20_long_01 <- melt(com_MCAR20,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                            # The source columns
                            measure.vars=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR" ),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="Time",
                            value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMCAR20_long_01$Time = recode(impMCAR20_long_01$Time, " 'ESA_1_20MCAR'=1")
  impMCAR20_long_01$Time = recode(impMCAR20_long_01$Time, " 'ESA_2_20MCAR'=2")
  impMCAR20_long_01$Time = recode(impMCAR20_long_01$Time, " 'ESA_3_20MCAR'=3")
  
  # to change to Imputation_
  impMCAR20_long_01= cbind(impMCAR20_long_01, Imputation_ = as.integer(impMCAR20_long_01$.imp)-1)               
  
  # export imputed data and dictionary from R to SPSS
  WriteXLS(impMCAR20_long_01, ExcelFileName = paste("100_MCAR20_Change_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  
  
  
  
  
  
  
  
  ###### Structural Equation Model ########
  
  ###### with covariates ########
  
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_20MCAR", to=c("ESA_2_20MCAR", "ESA_3_20MCAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_20MCAR", to="ESA_3_20MCAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_20MCAR", "ESA_2_20MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_20MCAR", "ESA_3_20MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)
  
  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  

  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_20MCAR", to=c("ESA_2_20MCAR", "ESA_3_20MCAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_20MCAR", to="ESA_3_20MCAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_20MCAR", "ESA_2_20MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_20MCAR", "ESA_3_20MCAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_20MCAR", "ESA_2_20MCAR", "ESA_3_20MCAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  
  
  
  
  
}


MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)

#export deleted and SEM coefficients and data, MCAR 20% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MCAR20_Deletions_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MCAR20_Deletions_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MCAR20_SEM_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MCAR20_SEM_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MCAR20_Undeleted_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MCAR20_Undeleted_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MCAR20_CCA_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MCAR20_CCA_SE.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MCAR20_OpenMx_Coef.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MCAR20_OpenMx_SE.xls", col.names = TRUE, row.names=FALSE)




































# Missing At Random Dependent on Covariate, 5%
######## MAR 5% ############################################################

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)


OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2)




############# CHANGE OVER TIME ###################
###### LOOPS 100 X, MAR 10% #######################
for (i in 1:100) {
  
   # creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  #################################################
  # Deletions based on DEIS status
  
  sort.ESA1 = dtStudent1[order(DEIS), ]
  Del = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.075, 1-.075)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.025, 1-.025)))
  sort.ESA1 = cbind(sort.ESA1, Del)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_1_05MAR = ifelse(sort.ESA1$Del==0, NA, dtStudent1$ESA_T1)  # doesn't show up when larger value
  

  Del2 = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.075, 1-.075)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.025, 1-.025)))
  sort.ESA1 = cbind(sort.ESA1, Del2)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_2_05MAR = ifelse(sort.ESA1$Del2==0, NA, dtStudent1$ESA_T2)  # doesn't show up when larger value
  
  
  Del3 = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.075, 1-.075)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.025, 1-.025)))
  sort.ESA1 = cbind(sort.ESA1, Del3)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_3_05MAR = ifelse(sort.ESA1$Del3==0, NA, dtStudent1$ESA_T3)  # doesn't show up when larger value
  

  

  
  Data1 = cbind(dtStudent1, ESA_1_05MAR, ESA_2_05MAR, ESA_3_05MAR)
  Data1 = Data1[, c("School", "trtGrp", "idChild", "ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_05MAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_05MAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_05MAR'=3")
  
  
  
   # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_05MAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_05MAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_05MAR'=3")
  
  
  # Again run MLM
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_05MAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_05MAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_05MAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR")] = "norm"
  meth
  
  # 40 imputed datasets and 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MAR05 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMAR05_long_01 <- melt(com_MAR05,
                           # ID variables - all the variables to keep but not split apart on
                           id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                           # The source columns
                           measure.vars=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR" ),
                           # Name of the destination column that will identify the original
                           # column that the measurement came from
                           variable.name="Time",
                           value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMAR05_long_01$Time = recode(impMAR05_long_01$Time, " 'ESA_1_05MAR'=1")
  impMAR05_long_01$Time = recode(impMAR05_long_01$Time, " 'ESA_2_05MAR'=2")
  impMAR05_long_01$Time = recode(impMAR05_long_01$Time, " 'ESA_3_05MAR'=3")
  
  # to change to Imputation_
  impMAR05_long_01= cbind(impMAR05_long_01, Imputation_ = as.integer(impMAR05_long_01$.imp)-1)               
  
  # export imputed data and dictionary from R to SPSS
  WriteXLS(impMAR05_long_01, ExcelFileName = paste("100_MAR05_DEIS_Change_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  
  
  
  
  
  
  
  
  ###### Structural Equation Model ########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_05MAR", to=c("ESA_2_05MAR", "ESA_3_05MAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_05MAR", to="ESA_3_05MAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_05MAR", "ESA_2_05MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_05MAR", "ESA_3_05MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)
  
  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_05MAR", to=c("ESA_2_05MAR", "ESA_3_05MAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_05MAR", to="ESA_3_05MAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_05MAR", "ESA_2_05MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_05MAR", "ESA_3_05MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_05MAR", "ESA_2_05MAR", "ESA_3_05MAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  
  
  
}





MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)


#export deleted and SEM coefficients and data, MAR 5% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MAR05_Deletions_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MAR05_Deletions_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MAR05_SEM_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MAR05_SEM_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MAR05_Undeleted_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MAR05_Undeleted_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MAR05_CCA_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MAR05_CCA_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MAR05_OpenMx_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MAR05_OpenMx_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)



























# Missing at Random Dependent on Covariate, 10% deleted
######## MAR 10% ############################################################

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)


OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2)




############# CHANGE OVER TIME ###################
###### LOOPS 100 X, MAR 10% #######################
for (i in 1:100) {
  
    # creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  #################################################
  # Deletions by DEIS status
 
   sort.ESA1 = dtStudent1[order(DEIS), ]
  Del = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.125, 1-.125)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.075, 1-.075)))
  sort.ESA1 = cbind(sort.ESA1, Del)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_1_10MAR = ifelse(sort.ESA1$Del==0, NA, dtStudent1$ESA_T1)  # doesn't show up when larger value
  
  
  Del2 = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.125, 1-.125)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.075, 1-.075)))
  sort.ESA1 = cbind(sort.ESA1, Del2)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_2_10MAR = ifelse(sort.ESA1$Del2==0, NA, dtStudent1$ESA_T2)  # doesn't show up when larger value
  
  
  Del3 = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.125, 1-.125)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.075, 1-.075)))
  sort.ESA1 = cbind(sort.ESA1, Del3)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_3_10MAR = ifelse(sort.ESA1$Del3==0, NA, dtStudent1$ESA_T3)  # doesn't show up when larger value
  
  
  
  
  Data1 = cbind(dtStudent1, ESA_1_10MAR, ESA_2_10MAR, ESA_3_10MAR)
  Data1 = Data1[, c("School", "trtGrp", "idChild", "ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_10MAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_10MAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_10MAR'=3")
  
  
  
   # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_10MAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_10MAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_10MAR'=3")
  
  
  # Again run MLM
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_10MAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_10MAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_10MAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR")] = "norm"
  meth
  
  # 40 imputed datasets and 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MAR10 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMAR10_long_01 <- melt(com_MAR10,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                            # The source columns
                            measure.vars=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR" ),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="Time",
                            value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMAR10_long_01$Time = recode(impMAR10_long_01$Time, " 'ESA_1_10MAR'=1")
  impMAR10_long_01$Time = recode(impMAR10_long_01$Time, " 'ESA_2_10MAR'=2")
  impMAR10_long_01$Time = recode(impMAR10_long_01$Time, " 'ESA_3_10MAR'=3")
  
  # to change to Imputation_
  impMAR10_long_01= cbind(impMAR10_long_01, Imputation_ = as.integer(impMAR10_long_01$.imp)-1)               
  
  #export imputed data and dictionary from R to SPSS
  WriteXLS(impMAR10_long_01, ExcelFileName = paste("100_MAR10_DEIS_Change_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  
  
  
  
  
  
  
  
  ###### Structural Equation Model ########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_10MAR", to=c("ESA_2_10MAR", "ESA_3_10MAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_10MAR", to="ESA_3_10MAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_10MAR", "ESA_2_10MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_10MAR", "ESA_3_10MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)
  
  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  
  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_10MAR", to=c("ESA_2_10MAR", "ESA_3_10MAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_10MAR", to="ESA_3_10MAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_10MAR", "ESA_2_10MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_10MAR", "ESA_3_10MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_10MAR", "ESA_2_10MAR", "ESA_3_10MAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  
  
  
  
  
  
}





MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)


#export deleted and SEM coefficients and data, MAR 10% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MAR10_Deletions_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MAR10_Deletions_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MAR10_SEM_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MAR10_SEM_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MAR10_Undeleted_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MAR10_Undeleted_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MAR10_CCA_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MAR10_CCA_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MAR10_OpenMx_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MAR10_OpenMx_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)




































# Missing at Random dependent on covariate, 20% deleted
######## MAR 20% ############################################################

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)


OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2) 




############# CHANGE OVER TIME ###################
###### LOOPS 100 X, MAR 10% #######################

for (i in 1:100) {
   
   # creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  #################################################
  # Deletions by DEIS status
  
  sort.ESA1 = dtStudent1[order(DEIS), ]
  Del = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.25, 1-.25)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.15, 1-.15)))
  sort.ESA1 = cbind(sort.ESA1, Del)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_1_20MAR = ifelse(sort.ESA1$Del==0, NA, dtStudent1$ESA_T1)  # doesn't show up when larger value
  
  
  Del2 = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.25, 1-.25)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.15, 1-.15)))
  sort.ESA1 = cbind(sort.ESA1, Del2)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_2_20MAR = ifelse(sort.ESA1$Del2==0, NA, dtStudent1$ESA_T2)  # doesn't show up when larger value
  
  
  Del3 = 
    c(sample(c(0,1), size=length(which(sort.ESA1$DEIS==1)), replace=T, prob=c(.25, 1-.25)),
      sample(c(0,1), size=length(which(sort.ESA1$DEIS==0)), replace=T, prob=c(0.15, 1-.15)))
  sort.ESA1 = cbind(sort.ESA1, Del3)
  sort.ESA1 = sort.ESA1[order(idChild)]
  ESA_3_20MAR = ifelse(sort.ESA1$Del3==0, NA, dtStudent1$ESA_T3)  # doesn't show up when larger value
  
  
  
  
  Data1 = cbind(dtStudent1, ESA_1_20MAR, ESA_2_20MAR, ESA_3_20MAR)
  Data1 = Data1[, c("School", "trtGrp", "idChild", "ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_20MAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_20MAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_20MAR'=3")
  
  
  
   # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_20MAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_20MAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_20MAR'=3")
  
  
  # Again, MLM
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_20MAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_20MAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_20MAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR")] = "norm"
  meth
  
  # 40 imputed datasets and 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MAR20 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMAR20_long_01 <- melt(com_MAR20,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                            # The source columns
                            measure.vars=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR" ),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="Time",
                            value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMAR20_long_01$Time = recode(impMAR20_long_01$Time, " 'ESA_1_20MAR'=1")
  impMAR20_long_01$Time = recode(impMAR20_long_01$Time, " 'ESA_2_20MAR'=2")
  impMAR20_long_01$Time = recode(impMAR20_long_01$Time, " 'ESA_3_20MAR'=3")
  
  # to change to Imputation_
  impMAR20_long_01= cbind(impMAR20_long_01, Imputation_ = as.integer(impMAR20_long_01$.imp)-1)               
  
  # export imputed data and dictionary from R to SPSS
  WriteXLS(impMAR20_long_01, ExcelFileName = paste("100_MAR20_DEIS_Change_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  
  
  
  
  
  
  
  
  ###### Structural Equation Model ########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_20MAR", to=c("ESA_2_20MAR", "ESA_3_20MAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_20MAR", to="ESA_3_20MAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_20MAR", "ESA_2_20MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_20MAR", "ESA_3_20MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)
  
  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  
  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_20MAR", to=c("ESA_2_20MAR", "ESA_3_20MAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_20MAR", to="ESA_3_20MAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_20MAR", "ESA_2_20MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_20MAR", "ESA_3_20MAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_20MAR", "ESA_2_20MAR", "ESA_3_20MAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  
  
  
}





MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)

# export deleted and SEM coefficients and data, MAR 20% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MAR20_Deletions_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MAR20_Deletions_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MAR20_SEM_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MAR20_SEM_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MAR20_Undeleted_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MAR20_Undeleted_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MAR20_CCA_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MAR20_CCA_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MAR20_OpenMx_Coef_DEIS.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MAR20_OpenMx_SE_DEIS.xls", col.names = TRUE, row.names=FALSE)
































# Missing not at random, 5% deleted 
######## MNAR 5% ############################################################

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)


OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2)



############# CHANGE OVER TIME ###################
###### LOOPS 100 X, MNAR 10% #######################

for (i in 1:100) {

# creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  #################################################
  # Deletions, larger values more likely to be missing
  
  dtStudent1 = dtStudent1[order(ESA_T1), ]
  nlength = length(dtStudent1$ESA_T1)
  

  
  test2 = defData(varname = "w", dist = "uniform", formula = "0;10")
  test1= genData(length(dtStudent1$ESA_T1), test2)
  test1 = test1[, "w"]
  test1=test1/100
  test1 = sort(test1$w, decreasing=FALSE)
  v = 1-test1
 

  Del = rep(NA, length(dtStudent1$ESA_T1))
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }



  ESA_1_05MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T1)  # doesn't show up when larger value
  dtStudent1 = cbind(dtStudent1, ESA_1_05MNAR)
  
  
  
  dtStudent1 = dtStudent1[order(ESA_T2), ]
  
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }

  
  ESA_2_05MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T2)  # doesn't show up when larger value
  dtStudent1 = cbind(dtStudent1, ESA_2_05MNAR)
  
  
  dtStudent1 = dtStudent1[order(ESA_T3), ]
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }
  

  ESA_3_05MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T3)  # doesn't show up when larger value
  
  dtStudent1 = cbind(dtStudent1, ESA_3_05MNAR)
  dtStudent1 = dtStudent1[order(idChild), ]
  
  
  Data1 = dtStudent1[, c("School", "trtGrp", "idChild", "ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_05MNAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_05MNAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_05MNAR'=3")
  
  
  
   # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_05MNAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_05MNAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_05MNAR'=3")
  
  
  # Again, MLM
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_05MNAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_05MNAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_05MNAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR")] = "norm"
  meth
  
  # 40 imputed datasets and 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MNAR05 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMNAR05_long_01 <- melt(com_MNAR05,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                            # The source columns
                            measure.vars=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR" ),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="Time",
                            value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMNAR05_long_01$Time = recode(impMNAR05_long_01$Time, " 'ESA_1_05MNAR'=1")
  impMNAR05_long_01$Time = recode(impMNAR05_long_01$Time, " 'ESA_2_05MNAR'=2")
  impMNAR05_long_01$Time = recode(impMNAR05_long_01$Time, " 'ESA_3_05MNAR'=3")
  
  # to change to Imputation_
  impMNAR05_long_01= cbind(impMNAR05_long_01, Imputation_ = as.integer(impMNAR05_long_01$.imp)-1)               
  
  # export imputed data and dictionary from R to SPSS
  WriteXLS(impMNAR05_long_01, ExcelFileName = paste("100_MNAR05_Prob_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  

  
  
  
  
  
  
  ###### Structural Equation Model ########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_05MNAR", to=c("ESA_2_05MNAR", "ESA_3_05MNAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_05MNAR", to="ESA_3_05MNAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_05MNAR", "ESA_2_05MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_05MNAR", "ESA_3_05MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)
  
  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  
  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_05MNAR", to=c("ESA_2_05MNAR", "ESA_3_05MNAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_05MNAR", to="ESA_3_05MNAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_05MNAR", "ESA_2_05MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_05MNAR", "ESA_3_05MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_05MNAR", "ESA_2_05MNAR", "ESA_3_05MNAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  
  
  
}





MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)


# export deleted and SEM coefficients and data, MNAR 5% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MNAR05_Deletions_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MNAR05_Deletions_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MNAR05_SEM_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MNAR05_SEM_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MNAR05_Undeleted_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MNAR05_Undeleted_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MNAR05_CCA_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MNAR05_CCA_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MNAR05_OpenMx_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MNAR05_OpenMx_SE_Prob.xls", col.names = TRUE, row.names=FALSE)

















































# Missing Not at Random, 10% deleted
######## MNAR 10% ############################################################

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)


OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2)





############# CHANGE OVER TIME ###################
###### LOOPS 100 X, MNAR 10% #######################
for (i in 1:100) {
  
    # creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  #################################################
  # Deletions, larger values more likely to be deleted
  
  dtStudent1 = dtStudent1[order(ESA_T1), ]
  nlength = length(dtStudent1$ESA_T1)
  Del = rep(NA, length(dtStudent1$ESA_T1))
  
  
  test2 = defData(varname = "w", dist = "uniform", formula = "1;20")
  test1= genData(length(dtStudent1$ESA_T1), test2)
  test1 = test1[, "w"]
  test1=test1/100
  test1 = sort(test1$w, decreasing=FALSE)
  v = 1-test1
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }
  
  
  ESA_1_10MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T1)  # doesn't show up when larger value
  dtStudent1 = cbind(dtStudent1, ESA_1_10MNAR)
  
  
  
  dtStudent1 = dtStudent1[order(ESA_T2), ]
  
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }
  
  
  ESA_2_10MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T2)  # doesn't show up when larger value
  dtStudent1 = cbind(dtStudent1, ESA_2_10MNAR)
  
  
  dtStudent1 = dtStudent1[order(ESA_T3), ]
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }
  
  ESA_3_10MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T3)  # doesn't show up when larger value
  
  dtStudent1 = cbind(dtStudent1, ESA_3_10MNAR)
  dtStudent1 = dtStudent1[order(idChild), ]
  
  
  Data1 = dtStudent1[, c("School", "trtGrp", "idChild", "ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_10MNAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_10MNAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_10MNAR'=3")
  
  
  
   # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_10MNAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_10MNAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_10MNAR'=3")
  
  
  # Again, MLM
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_10MNAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_10MNAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_10MNAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR")] = "norm"
  meth
  
  # 40 imputed datasets and 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MNAR10 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMNAR10_long_01 <- melt(com_MNAR10,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                            # The source columns
                            measure.vars=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR" ),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="Time",
                            value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMNAR10_long_01$Time = recode(impMNAR10_long_01$Time, " 'ESA_1_10MNAR'=1")
  impMNAR10_long_01$Time = recode(impMNAR10_long_01$Time, " 'ESA_2_10MNAR'=2")
  impMNAR10_long_01$Time = recode(impMNAR10_long_01$Time, " 'ESA_3_10MNAR'=3")
  
  # to change to Imputation_
  impMNAR10_long_01= cbind(impMNAR10_long_01, Imputation_ = as.integer(impMNAR10_long_01$.imp)-1)               
  
  # export imputed data and dictionary from R to SPSS
  WriteXLS(impMNAR10_long_01, ExcelFileName = paste("100_MNAR10_Prob_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  
  
  
  
  
  
  
  
  ###### Structural Equation Model ########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_10MNAR", to=c("ESA_2_10MNAR", "ESA_3_10MNAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_10MNAR", to="ESA_3_10MNAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_10MNAR", "ESA_2_10MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_10MNAR", "ESA_3_10MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)

  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  
  
  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_10MNAR", to=c("ESA_2_10MNAR", "ESA_3_10MNAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_10MNAR", to="ESA_3_10MNAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_10MNAR", "ESA_2_10MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_10MNAR", "ESA_3_10MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_10MNAR", "ESA_2_10MNAR", "ESA_3_10MNAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  

  
  
}





MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)


#export deleted and SEM coefficients and data, MNAR 10% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MNAR10_Deletions_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MNAR10_Deletions_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MNAR10_SEM_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MNAR10_SEM_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MNAR10_Undeleted_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MNAR10_Undeleted_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MNAR10_CCA_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MNAR10_CCA_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MNAR10_OpenMx_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MNAR10_OpenMx_SE_Prob.xls", col.names = TRUE, row.names=FALSE)




































# Missing Not at Random, 20% deleted
######## MNAR 20% ############################################################

# Creating table for deleted dataset coefficients
SimSum_Coef = c(1, 2, 3, 4, 5, 6)
SimSum_SE = c(1, 2, 3, 4, 5, 6)

OpenMx_coef = c(1, 2)
OpenMx_SE = c(1, 2)

Undeleted_coef = c(1, 2, 3, 4, 5, 6)
Undeleted_SE = c(1, 2, 3, 4, 5, 6)

CCA_coef = c(1,2,3,4,5,6)
CCA_SE = c(1,2,3,4,5,6)


OpenMx_plain_coef = c(1, 2)
OpenMx_plain_SE = c(1, 2)




############# CHANGE OVER TIME ###################
###### LOOPS 100 X, MNAR 10% #######################
for (i in 1:100) {
  
    # creation of school-level data: 10 schools, with a small, random school-wide shift so that students between schools have      increased variation (s0)
  #Each school/cluster is around 60 students 
  
  gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 10, 
                        id = "School")
  gen.school <- defData(gen.school, varname = "nStudents", formula = 60, dist = "noZeroPoisson")
  dtSchool <- genData(10, gen.school)
  
  
  # Adding treatment, Intervention and Control, to half of schools
  dtSchool <- trtAssign(dtSchool, n=2)
  
  # Adding DEIS status to half of schools
  dtSchool = trtAssign(dtSchool, n=2, balanced=TRUE, grpName = "DEIS")
  
  
  # Adding student data: created random variable for a small added change over time, T
  # created three outcome variables, ESA, with Times 2 and 3 dependent on treatment group and time
  # Adding small effect of DEIS status
  # Creating Age variable ranging from 11 to 16
  
  gen.student = defDataAdd(varname = "Tr", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "S", dist = "normal", formula = 1.0, variance = .2)
  gen.student = defDataAdd(gen.student, varname = "R", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Q", dist = "normal", formula = .5, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "DEIS_add", dist = "normal", formula = .25, variance = .25)
  gen.student = defDataAdd(gen.student, varname = "Age", dist = "uniform", formula = "11;16")
  
  
  
  # clustered the kids within schools, based on "nStudents" variable
  dtStudent = genCluster(dtSchool, "School", numIndsVar = "nStudents", level1ID = "idChild")
  
  # Mean and correlation matrix based on real trial data
  C = matrix(c(1, 0.73, 0.72, 0.73, 1, 0.76, 0.72, 0.76, 1), nrow=3)
  dt = genCorData(length(dtStudent$idChild), mu=c(30.2, 30.2, 30.2), sigma=c(7, 7, 7), corMatrix=C)
  
  #combined student and school information
  dtStudent = addColumns(gen.student, dtStudent)
  
  dtStudent$Age = floor(dtStudent$Age)
  
  
  dtStudent <- cbind(dtStudent, ESA_T1 = dt$V1)
  dtStudent <- cbind(dtStudent, ESA_T2 = dt$V2)
  dtStudent <- cbind(dtStudent, ESA_T3 = dt$V3)
  

# shifting the outcome variables by the school mean shift, Time shift, treatment shift, and DEIS shift  
  dtStudent$ESA_T1=dtStudent$ESA_T1 + dtStudent$s0 - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T2=dtStudent$ESA_T2 + dtStudent$s0 + (dtStudent$S*dtStudent$trtGrp) + (dtStudent$R*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  dtStudent$ESA_T3=dtStudent$ESA_T3 + dtStudent$s0 + (dtStudent$Tr*dtStudent$trtGrp) + (dtStudent$Q*dtStudent$trtGrp) - (dtStudent$DEIS*dtStudent$DEIS_add)
  
  
  dtStudent1 = dtStudent[, c("School", "trtGrp", "idChild", "ESA_T1", "ESA_T2", "ESA_T3", "DEIS", "Age")]
  
  dtStudent_long <- melt(dtStudent1,
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age"),
                         # The source columns
                         measure.vars=c("ESA_T1", "ESA_T2", "ESA_T3" ),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Time",
                         value.name="ESA_recid")
  
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T1'=1")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T2'=2")
  dtStudent_long$Time = recode(dtStudent_long$Time, " 'ESA_T3'=3")
  
  
  # Primary analysis: Mixed Linear model
  # Getting results from the completed datasets to compare after deletions
  
  test <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
              random = list(School = ~1, idChild = ~1), 
              weights =varIdent(School),
              corr = corCompSymm(form= ~Time),
              data=dtStudent_long, na.action="na.omit",
              method = "REML")
  
  
  TestSum = summary(test)$tTable[,1]
  Undeleted_coef = rbind(Undeleted_coef, TestSum)
  
  TestSum_SE = summary(test)$tTable[,2]
  Undeleted_SE = rbind(Undeleted_SE, TestSum_SE)
  
  
  
  #################################################
  # Deletions, larger values more likely to be deleted
  
  dtStudent1 = dtStudent1[order(ESA_T1), ]
  nlength = length(dtStudent1$ESA_T1)
  Del = rep(NA, length(dtStudent1$ESA_T1))
  
  
  test2 = defData(varname = "w", dist = "uniform", formula = "5;35")
  test1= genData(length(dtStudent1$ESA_T1), test2)
  test1 = test1[, "w"]
  test1=test1/100
  test1 = sort(test1$w, decreasing=FALSE)
  v = 1-test1
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }
  
  
  ESA_1_20MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T1)  # doesn't show up when larger value
  dtStudent1 = cbind(dtStudent1, ESA_1_20MNAR)
  
  
  
  dtStudent1 = dtStudent1[order(ESA_T2), ]
  
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }
  
  
  ESA_2_20MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T2)  # doesn't show up when larger value
  dtStudent1 = cbind(dtStudent1, ESA_2_20MNAR)
  
  
  dtStudent1 = dtStudent1[order(ESA_T3), ]
  
  for (j in 1:nlength)
  {
    Del[j] = sample(c(0,1), size = 1, prob=c(test1[j], v[j]))
  }
  
  ESA_3_20MNAR = ifelse(Del==0, NA, dtStudent1$ESA_T3)  # doesn't show up when larger value
  
  dtStudent1 = cbind(dtStudent1, ESA_3_20MNAR)
  dtStudent1 = dtStudent1[order(idChild), ]
  
  
  Data1 = dtStudent1[, c("School", "trtGrp", "idChild", "ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR", "DEIS", "Age")]
  
  constant=Data1$School/Data1$School
  Data1 = cbind(Data1, constant)
  
  
  Data1_long <- melt(Data1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_1_20MNAR'=1")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_2_20MNAR'=2")
  Data1_long$Time = recode(Data1_long$Time, " 'ESA_3_20MNAR'=3")
  
  
  
   # Running Mixed Linear Model again on dataset with deletions
  test2 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data1_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test2)$tTable[,1]
  SimSum_Coef = rbind(SimSum_Coef, TestSum)
  
  TestSum_SE = summary(test2)$tTable[,2]
  SimSum_SE = rbind(SimSum_SE, TestSum_SE)
  
  
  
  ##### complete case analysis #####
  # Force out any observation with missing data and then re-analysed using Mixed Linear Model
  
  Data2 = na.omit(Data1)
  
  Data2_long <- melt(Data2,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                     # The source columns
                     measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="Time",
                     value.name="ESA_recid")
  
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_1_20MNAR'=1")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_2_20MNAR'=2")
  Data2_long$Time = recode(Data2_long$Time, " 'ESA_3_20MNAR'=3")
  
  
  # Again, MLM
  test3 <- lme(ESA_recid ~ Time + trtGrp + trtGrp*Time, 
               random = list(School = ~1, idChild = ~1), 
               weights =varIdent(School),
               corr = corCompSymm(form= ~Time),
               data=Data2_long, na.action="na.omit",
               method = "REML")
  
  
  TestSum = summary(test3)$tTable[,1]
  CCA_coef = rbind(CCA_coef, TestSum)
  
  TestSum_SE = summary(test3)$tTable[,2]
  CCA_SE = rbind(CCA_SE, TestSum_SE)
  
  
  
  
  
  ##### multiple imputation ######
  
  ini1 = mice(Data1, maxit=0)
  pred = ini1$pred
  pred
  
  # Remove all filled from being imputed
  meth = ini1$meth
  meth[c("School", "trtGrp", "idChild", "DEIS", "Age", "constant")] = ""
  pred["ESA_1_20MNAR", ] = c(-2, 1, 0, 0, 1, 1, 1, 1, 2)
  pred["ESA_2_20MNAR", ] = c(-2, 1, 0, 1, 0, 1, 1, 1, 2)
  pred["ESA_3_20MNAR", ] = c(-2, 1, 0, 1, 1, 0, 1, 1, 2)
  pred
  
  meth[c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR")] = "norm"
  meth
  
  # 40 imputed datasets and 40 iterations
  imp = mice(Data1, meth=meth, pred=pred, maxit=40, m = 40, pri=F)
  
  com_MNAR20 = complete(imp, "long")
  
  # Next, changing to further long format via "Time"
  impMNAR20_long_01 <- melt(com_MNAR20,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c(".imp", ".id", "School", "trtGrp", "idChild", "DEIS", "Age", "constant"),
                            # The source columns
                            measure.vars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR" ),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="Time",
                            value.name="ESA_Recid")
  
  # Renaming values as 1, 2, and 3 in Time variable
  impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_1_20MNAR'=1")
  impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_2_20MNAR'=2")
  impMNAR20_long_01$Time = recode(impMNAR20_long_01$Time, " 'ESA_3_20MNAR'=3")
  
  # to change to Imputation_
  impMNAR20_long_01= cbind(impMNAR20_long_01, Imputation_ = as.integer(impMNAR20_long_01$.imp)-1)               
  
  # export imputed data and dictionary from R to SPSS
  WriteXLS(impMNAR20_long_01, ExcelFileName = paste("100_MNAR20_Prob_", i, ".xls", sep = ""), col.names = TRUE, row.names=FALSE)
  
  
  
  
  
  
  
  
  ###### Structural Equation Model ########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR", "trtGrp", "DEIS"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_20MNAR", to=c("ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_20MNAR", to="ESA_3_20MNAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_20MNAR", "ESA_2_20MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1),
    mxPath(from="DEIS", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("DEIS_t1", "DEIS_t2", "DEIS_t3")),
    mxPath(from="one", to="DEIS", arrows=1, free=F, values=0),
    mxPath(from="DEIS", arrows=2, free=F, values=1))
  
  
  SingleLevelModel = mxRun(SingleLevelModel)
  statistics=summary(SingleLevelModel)
  
  OpenMx_stats = statistics$parameters[c(2, 3), c("Estimate")]
  OpenMx_coef = rbind(OpenMx_coef, OpenMx_stats)
  
  OpenMx_stats1 = statistics$parameters[c(2,3), c("Std.Error")]
  OpenMx_SE = rbind(OpenMx_SE, OpenMx_stats1)
  
  
  
  
  
  
  ######## no covariates! ##########
  
  perSchool <- mxModel(
    "perSchool", type="RAM", latentVars=c("school"),
    mxData(data.frame(id=unique(Data1$School)), type='raw', primaryKey = 'id'),
    mxPath("school", arrows=2, values=1)
  )
  
  SingleLevelModel1= mxModel(
    "One Level Model", type="RAM", manifestVars=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR", "trtGrp"), perSchool,
    latentVars=c("I", "S", "S2"), 
    mxData(observed=Data1, type="raw"),
    #compound symmetry covariance structure
    mxPath(from=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(1,1,1),
           labels=c("variance", "variance", "variance")),
    mxPath(from="ESA_1_20MNAR", to=c("ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=2, free=T, values=c(.5, .5),
           labels=c("covar", "covar")),
    mxPath(from="ESA_2_20MNAR", to="ESA_3_20MNAR", arrows=2, free=T, values=.5, labels="covar"),
    #two slopes (S, S2) for T1 to T2, and T1 to T3
    mxPath(from=c("I", "S", "S2"), arrows=2, connect="unique.pairs", free=c(F, F, F, F, F, F), values=c(0, 0, 0, 0, 0, 0)), 
    mxPath(from="I", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
           values=c(1, 1, 1)),
    mxPath(from="S", to=c("ESA_1_20MNAR", "ESA_2_20MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    mxPath(from="S2", to=c("ESA_1_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F, 
           values=c(0, 1)),
    #manifest means not estimated
    mxPath(from="one", to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"), arrows=1, free=F,
           values=c(0, 0, 0)),
    # Account for per-School intercepts
    mxPath('perSchool.school', to=c("ESA_1_20MNAR", "ESA_2_20MNAR", "ESA_3_20MNAR"),
           values=1, free=FALSE, joinKey='School'),
    #latent means estimated
    mxPath(from="one", to=c("I", "S", "S2"), arrows=1, free=T, values=c(1, 1, 1), 
           labels=c("meanI", "meanS", "meanS2")),
    mxPath(from="trtGrp", to=c("I", "S", "S2"), arrows=1, free=TRUE, values=c(.5, .5, .5),
           labels=c("ireg", "Sreg", "S2reg")),
    mxPath(from="one", to="trtGrp", arrows=1, free=F, values=0),
    mxPath(from="trtGrp", arrows=2, free=F, values=1))
  
  
  SingleLevelModel1 = mxRun(SingleLevelModel1)
  statistics1=summary(SingleLevelModel1)
  
  OpenMx_stats2 = statistics1$parameters[c(2, 3), c("Estimate")]
  OpenMx_plain_coef = rbind(OpenMx_plain_coef, OpenMx_stats2)
  
  OpenMx_stats3 = statistics1$parameters[c(2,3), c("Std.Error")]
  OpenMx_plain_SE = rbind(OpenMx_plain_SE, OpenMx_stats3)
  
  
  
}


MNAR20_coef <- data.frame(matrix(unlist(SimSum_Coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
MNAR20_SE <- data.frame(matrix(unlist(SimSum_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05 <- data.frame(matrix(unlist(OpenMx_coef), nrow=101, byrow=F),stringsAsFactors=FALSE)
SEM_05_SE <- data.frame(matrix(unlist(OpenMx_SE), nrow=101, byrow=F),stringsAsFactors=FALSE)
Undel_coef = data.frame(matrix(unlist(Undeleted_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
Undel_SE = data.frame(matrix(unlist(Undeleted_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_coeff = data.frame(matrix(unlist(CCA_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
CCA_SEe = data.frame(matrix(unlist(CCA_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_c = data.frame(matrix(unlist(OpenMx_plain_coef), nrow=101, byrow=F), stringsAsFactors=FALSE)
OpenMx_s = data.frame(matrix(unlist(OpenMx_plain_SE), nrow=101, byrow=F), stringsAsFactors=FALSE)



#export deleted and SEM coefficients and data, MNAR 20% 
WriteXLS(MNAR20_coef, ExcelFileName = "100_MNAR20_Deletions_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(MNAR20_SE, ExcelFileName = "100_MNAR20_Deletions_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05, ExcelFileName = "100_MNAR20_SEM_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(SEM_05_SE, ExcelFileName = "100_MNAR20_SEM_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_coef, ExcelFileName = "100_MNAR20_Undeleted_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(Undel_SE, ExcelFileName = "100_MNAR20_Undeleted_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_coeff, ExcelFileName = "100_MNAR20_CCA_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(CCA_SEe, ExcelFileName = "100_MNAR20_CCA_SE_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_c, ExcelFileName = "100_MNAR20_OpenMx_Coef_Prob.xls", col.names = TRUE, row.names=FALSE)
WriteXLS(OpenMx_s, ExcelFileName = "100_MNAR20_OpenMx_SE_Prob.xls", col.names = TRUE, row.names=FALSE)





