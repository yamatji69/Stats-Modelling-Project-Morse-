'''
HDAT 9600 GROUP ASSESSMENT
TEAM MORSE
GLM MODEL

'''




icu_patients_df1 <- readRDS("~/Documents/HDAT 9600/FINAL hdat9600_final_assignment/icu_patients_df1.rds")


# ================================================================================================
#EXPLORING DATA; CHECKING STRUCTURE OF DATA FRAME
str(icu_patients_df1)
head(icu_patients_df1)
summary(icu_patients_df1)

'''

total of 2061 observations, 120 variables
mostly numeric, some factors (ICUType, Gender), one logical (status)
lots of variables with NAs
diasABP, height, weight, 
survival (since other patients did not die; confirmed by "0" in_hospital_death )

'''

# variable names/headings for easy reference
df_variables <- colnames(icu_patients_df1)
df_variables

# some variable checks
hist(icu_patients_df1$BUN_max)
hist(icu_patients_df1$SAPS1)
hist(icu_patients_df1$Creatinine_max)
hist(icu_patients_df1$Urine_diff)
hist(icu_patients_df1$Urine_max)
hist(icu_patients_df1$MAP_max)
hist(icu_patients_df1$MAP_diff)
hist(icu_patients_df1$HCT_min)
hist(icu_patients_df1$MAP_min)
hist(icu_patients_df1$NISysABP_min)
hist(icu_patients_df1$DiasABP_min)
hist(icu_patients_df1$PaO2_max)
hist(icu_patients_df1$PaO2_min)

plot(icu_patients_df1$PaO2_min)
plot(icu_patients_df1$Urine_diff)
plot(icu_patients_df1$BUN_max)
plot(icu_patients_df1$Creatinine_max)
plot(icu_patients_df1$SAPS1)


# ================================================================================================
# UNIVARIATE ANALYSIS
# univariate exploration for specific variables that may be of importance

glm_ICUtype <- glm(in_hospital_death ~ ICUType, data = icu_patients_df1)
summary(glm_ICUtype)
# ICUtype is a significant variable
glm_Age <- glm(in_hospital_death ~ Age, data = icu_patients_df1)
summary(glm_Age)
# Age is a significant variable
glm_Gender <- glm(in_hospital_death ~ Gender, data = icu_patients_df1)
summary(glm_Gender)
# Gender is not a significant variable
glm_Height <- glm(in_hospital_death ~ Height, data = icu_patients_df1)
summary(glm_Height)
# Height is not a significant variable
glm_BUN_max <- glm(in_hospital_death ~ BUN_max, data = icu_patients_df1)
summary(glm_BUN_max)
# BUN_max is a significant variable
glm_SAPS1 <- glm(in_hospital_death ~ SAPS1, data = icu_patients_df1)
summary(glm_SAPS1)
# SAPS1 is a significant variable
glm_Creatinine_max <- glm(in_hospital_death ~ Creatinine_max, data = icu_patients_df1)
summary(glm_Creatinine_max)
# Creatinine_max is a significant variable
glm_DiasABP_diff <- glm(in_hospital_death ~ DiasABP_diff, data = icu_patients_df1)
summary(glm_DiasABP_diff)
# DiasABP_diff is not a significant variable
glm_FiO2_max <- glm(in_hospital_death ~ FiO2_max, data = icu_patients_df1)
summary(glm_FiO2_max)
# FiO2_max is not a significant variable
glm_GCS_min <- glm(in_hospital_death ~ GCS_min, data = icu_patients_df1)
summary(glm_GCS_min)
# GCS_min is a significant variable
glm_HCO3_diff <- glm(in_hospital_death ~ HCO3_diff, data = icu_patients_df1)
summary(glm_HCO3_diff)
# HCO3_diff is a significant variable
glm_HCT_min <- glm(in_hospital_death ~ HCT_min, data = icu_patients_df1)
summary(glm_HCT_min)
# HCT_min is not a significant variable
glm_HR_diff <- glm(in_hospital_death ~ HR_diff, data = icu_patients_df1)
summary(glm_HR_diff)
# HR_diff is a significant variable
glm_K_diff <- glm(in_hospital_death ~ K_diff, data = icu_patients_df1)
summary(glm_K_diff)
# K_diff is a significant variable
glm_Lactate_max <- glm(in_hospital_death ~ Lactate_max, data = icu_patients_df1)
summary(glm_Lactate_max)
# Lactate_max is a significant variable
glm_MAP_diff <- glm(in_hospital_death ~ MAP_diff, data = icu_patients_df1)
summary(glm_MAP_diff)
# MAP_diff is not a significant variable
glm_Mg_diff <- glm(in_hospital_death ~ Mg_diff, data = icu_patients_df1)
summary(glm_Mg_diff)
# Mg_diff is not a significant variable
glm_Na_diff <- glm(in_hospital_death ~ Na_diff, data = icu_patients_df1)
summary(glm_Na_diff)
# Na_diff is a significant variable
glm_NIDiasABP_diff <- glm(in_hospital_death ~ NIDiasABP_diff, data = icu_patients_df1)
summary(glm_NIDiasABP_diff)
# NIDiasABP_diff is not a significant variable
glm_NIMAP_diff <- glm(in_hospital_death ~ NIMAP_diff, data = icu_patients_df1)
summary(glm_NIMAP_diff)
# NIMAP_diff is not a significant variable
glm_NISysABP_diff <- glm(in_hospital_death ~ NISysABP_diff, data = icu_patients_df1)
summary(glm_NISysABP_diff)
# NISysABP_diff is a significant variable
glm_PaCO2_diff <- glm(in_hospital_death ~ PaCO2_diff, data = icu_patients_df1)
summary(glm_PaCO2_diff)
# PaCO2_diff is a significant variable
glm_PaCO2_max <- glm(in_hospital_death ~ PaCO2_max, data = icu_patients_df1)
summary(glm_PaCO2_max)
# PaCO2_max is not a significant variable
glm_PaO2_max <- glm(in_hospital_death ~ PaO2_max, data = icu_patients_df1)
summary(glm_PaO2_max)
# PaO2_max is a significant variable
glm_Platelets_min <- glm(in_hospital_death ~ Platelets_min, data = icu_patients_df1)
summary(glm_Platelets_min)
# Platelets_min is not a significant variable
glm_RespRate_diff <- glm(in_hospital_death ~ RespRate_diff, data = icu_patients_df1)
summary(glm_RespRate_diff)
# RespRate_diff is a significant variable
glm_SaO2_diff <- glm(in_hospital_death ~ SaO2_diff, data = icu_patients_df1)
summary(glm_SaO2_diff)
# SaO2_diff is not a significant variable
glm_SysABP_diff <- glm(in_hospital_death ~ SysABP_diff, data = icu_patients_df1)
summary(glm_SysABP_diff)
# SysABP_diff is a significant variable
glm_Temp_diff <- glm(in_hospital_death ~ Temp_diff, data = icu_patients_df1)
summary(glm_Temp_diff)
# Temp_diff is a significant variable
glm_TroponinI_max <- glm(in_hospital_death ~ TroponinI_max, data = icu_patients_df1)
summary(glm_TroponinI_max)
# TroponinI_max is not a significant variable
glm_TroponinT_max <- glm(in_hospital_death ~ TroponinT_max, data = icu_patients_df1)
summary(glm_TroponinT_max)
# TroponinT_max is a significant variable
glm_Urine_diff <- glm(in_hospital_death ~ Urine_diff, data = icu_patients_df1)
summary(glm_Urine_diff)
# Urine_diff is a significant variable
glm_WBC_diff <- glm(in_hospital_death ~ WBC_diff, data = icu_patients_df1)
summary(glm_WBC_diff)
# WBC_diff is a significant variable
glm_Weight_max <- glm(in_hospital_death ~ Weight_max, data = icu_patients_df1)
summary(glm_Weight_max)
# Weight_max is a significant variable



# ================================================================================================
# MULTIVARIATE ANALYSIS PART 1: INITIAL ANALYSIS
# all variables
'''
full_icumod <- glm(in_hospital_death ~ ., family = binomial, data = icu_patients_df1)
full_icumod
summary(full_icumod)
step(full_icumod, trace=1)
'''

# exploring combinations of variables 
# trial 1: after checking the data, variables of interest are chosen for initial analysis
icu_glm_initial_1 <- glm(in_hospital_death ~ 
                           # patient descriptor variables
                           ICUType +
                           Age +
                           Gender +
                           Height +
                           BUN_max +
                           # clinical variables
                           SAPS1 + 
                           Creatinine_max +
                           DiasABP_diff +
                           FiO2_max + 
                           GCS_min +
                           HCO3_diff +
                           HCT_min + 
                           HR_diff + 
                           K_diff + 
                           Lactate_max +
                           MAP_diff + 
                           Mg_diff + 
                           Na_diff +
                           NIDiasABP_diff + 
                           NIMAP_diff +
                           NISysABP_diff +
                           PaCO2_diff +
                           PaCO2_max + 
                           PaO2_max +
                           Platelets_min +
                           RespRate_diff + 
                           SaO2_diff + 
                           SysABP_diff +
                           Temp_diff +
                           TroponinI_max +
                           TroponinT_max +
                           Urine_diff +
                           WBC_diff +
                           Weight_max, data = icu_patients_df1, family = binomial)
summary(icu_glm_initial_1)
'
From this (icu_glm_initial_1), the significant variables were:
BUN_max                              ***
SAPS1                                *  
HCT_min                              ** 
Lactate_max                          .  
PaO2_max                             .  
BUN_max and HCT_min appear come out significant for this initial model. 
SAPS1 is also significant, would like to also compare with SOFA in next model. 
Id explore other variables for the clinical markers lactate, MAP, NISysABP, and PaO2_max
to test for stronger significance (i.e. min, max, or diff)
'
```



```{r}
# trial 2: exploring other variables
# trying other markers for lactate, MAP, NISysABP, and PaO2_max
icu_glm_initial_2 <- glm(in_hospital_death ~ 
                           # patient descriptor variables
                           ICUType +
                           Age +
                           Gender +
                           Height +
                           BUN_max +
                           # clinical variables
                           SAPS1 + 
                           Creatinine_max +
                           DiasABP_diff +
                           FiO2_max + 
                           GCS_min +
                           HCO3_diff +
                           HCT_min + 
                           HR_diff + 
                           K_diff + 
                           Lactate_max +
                           MAP_diff + 
                           Mg_diff + 
                           Na_diff +
                           NIDiasABP_diff + 
                           NIMAP_diff +
                           NISysABP_diff +
                           PaCO2_diff +
                           PaCO2_max + 
                           PaO2_max +
                           Platelets_min +
                           RespRate_diff + 
                           SaO2_diff + 
                           SysABP_diff +
                           Temp_diff +
                           TroponinI_max +
                           TroponinT_max +
                           Urine_diff +
                           WBC_diff +
                           Weight_max +
                           #EXPLORING OTHER VARIABLES
                           SOFA +
                           Lactate_diff + # I didn't include Lactate_min as low lactate levels are ideal/expected for a healthy person
                           MAP_max +
                           NISysABP_max +
                           NISysABP_min +
                           PaO2_diff +
                           PaO2_min,
                         data = icu_patients_df1, family = binomial)
summary(icu_glm_initial_2)
'
In this second model, the following are significant:
BUN_max                               0.0377269  0.0102347   3.686 0.000228 ***
SAPS1                                 0.1598306  0.0685511   2.332 0.019724 *  
Creatinine_max                       -0.2921214  0.1466432  -1.992 0.046365 *  
HCT_min                               0.1321537  0.0381673   3.462 0.000535 ***
MAP_diff                              0.0511316  0.0173223   2.952 0.003160 ** 
PaO2_max                             -0.0075585  0.0038625  -1.957 0.050362 .  
Urine_diff                           -0.0007600  0.0004575  -1.661 0.096640 .  
MAP_max                              -0.0440888  0.0162755  -2.709 0.006751 ** 
NISysABP_min                          0.0164209  0.0084823   1.936 0.052880 .  
PaO2_min                              0.0101487  0.0051102   1.986 0.047034 *  
AIC: 387.02 
Will look in other variables for PaO2, Urine, and blood pressure (systolic/diastolic/mean)
'
```



```{r}
# trial 3: trying other combinations
icu_glm_initial_3 <- glm(in_hospital_death ~ 
                           # patient descriptor variables
                           ICUType +
                           Age +
                           Gender +
                           Height +
                           BUN_max +
                           # clinical variables
                           SAPS1 + 
                           Creatinine_max +
                           DiasABP_diff + 
                           FiO2_max + 
                           GCS_min +
                           HCO3_diff +
                           HCT_min + 
                           HR_diff + 
                           K_diff + 
                           Lactate_max +
                           MAP_diff + 
                           Mg_diff + 
                           Na_diff +
                           NIDiasABP_diff + 
                           NIMAP_diff +
                           NISysABP_diff +
                           PaCO2_diff +
                           PaCO2_max + 
                           PaO2_max +
                           Platelets_min +
                           RespRate_diff + 
                           SaO2_diff + 
                           SysABP_diff +
                           Temp_diff +
                           TroponinI_max +
                           TroponinT_max +
                           Urine_diff +
                           WBC_diff +
                           Weight_max +
                           #EXPLORING OTHER VARIABLES
                           SOFA +
                           Lactate_diff + # I didn't include Lactate_min as low lactate levels are ideal/expected for a healthy person
                           MAP_max +
                           NISysABP_max +
                           NISysABP_min +
                           PaO2_diff +
                           PaO2_min +
                           #ADD ONS TO MODEL 3
                           PaO2_diff +
                           SysABP_min + #blood pressure variables
                           SysABP_max +
                           DiasABP_min +
                           DiasABP_max +
                           NIDiasABP_min +
                           NIDiasABP_max +
                           MAP_min +
                           Urine_min +
                           Urine_max,
                         data = icu_patients_df1, family = binomial)
summary(icu_glm_initial_3)
'
On third iteration, I can see some variables are strongly significant across trial models,
Though with varying degrees of significance, 
other variables of interest consistently come out in the models
BUN_max                               3.701e-02  1.142e-02   3.241 0.001192 ** 
SAPS1                                 1.270e-01  7.460e-02   1.702 0.088720 .  
Creatinine_max                       -5.115e-01  1.861e-01  -2.748 0.005988 ** 
HCT_min                               1.386e-01  4.042e-02   3.428 0.000608 ***
MAP_diff                              1.010e-01  3.232e-02   3.125 0.001777 ** 
PaO2_max                             -7.916e-03  4.186e-03  -1.891 0.058615 .  
Urine_diff                            1.511e-02  4.524e-03   3.340 0.000837 ***
MAP_max                              -9.012e-02  3.081e-02  -2.925 0.003447 ** 
NISysABP_min                          2.651e-02  1.287e-02   2.060 0.039400 *  
PaO2_min                              1.157e-02  5.345e-03   2.165 0.030403 *  
DiasABP_min                          -5.038e-02  3.004e-02  -1.677 0.093538 .  
MAP_min                               4.820e-02  2.712e-02   1.777 0.075488 .  
Urine_max                            -1.546e-02  4.410e-03  -3.505 0.000456 ***
AIC: 384.28
Patient descriptors didnt seem to have any significance so far, will cross out for next iteration.
If we group the clinical variables, we have markers for the following so far:
    blood pressure (MAP, SysABP)
    kidney function (BUN, urine, creatinine)
    blood O2 levels (PaO2)
    assessment tool/score (SAPS1)
Ive reviewed the data and would like to check a few more clinical variables which may be of interest/significance:
    glucose
    albumin
    cholesterol
    liver profile markers (AST, ALT, ALP)
    electrolytes (Na, Mg, K)
'
```

```{r}
# trial 4
icu_glm_initial_4 <- glm(in_hospital_death ~ 
                           # CLINICAL VARIABLES GROUPED FROM FIRST 3 TRIAL MODELS
                           # rapid assessment tool
                           SAPS1 +
                           # kidney markers
                           BUN_max +
                           Creatinine_max +
                           Urine_diff +
                           Urine_max +
                           # blood pressure variables
                           MAP_diff +
                           MAP_max +
                           MAP_min +
                           NISysABP_min +
                           DiasABP_min +
                           # blood levels
                           PaO2_max +
                           PaO2_min +
                           HCT_min +
                           # EXPLORING OTHER VARIABLES OF INTEREST
                           Glucose_diff +
                           Albumin_diff +
                           ALP_max +
                           ALT_max +
                           AST_max +
                           K_min +
                           Na_diff +
                           Mg_min +
                           Cholesterol_max,
                         data = icu_patients_df1, family = binomial)
summary(icu_glm_initial_4)
'
From trial model 4, none of the added variables of interest were significant. 
SAPS1            0.1302302  0.0268884   4.843 1.28e-06 ***
BUN_max          0.0275854  0.0061310   4.499 6.82e-06 ***
Creatinine_max  -0.3569116  0.1083592  -3.294 0.000988 ***
Urine_diff       0.0079677  0.0026051   3.059 0.002224 ** 
Urine_max       -0.0086481  0.0024959  -3.465 0.000530 ***
MAP_diff         0.0279541  0.0112775   2.479 0.013185 *  
MAP_max         -0.0251078  0.0106622  -2.355 0.018530 *  
MAP_min          0.0189800  0.0099203   1.913 0.055718 .  
DiasABP_min     -0.0250375  0.0125252  -1.999 0.045612 *  
PaO2_max        -0.0019022  0.0010563  -1.801 0.071739 .  
PaO2_min         0.0051129  0.0021546   2.373 0.017645 *  
HCT_min          0.0444069  0.0202447   2.194 0.028271 *  
AIC: 705.14
Ill try running these for trial model5
'
```



```{r}
# trial 5: preliminary model from the first 4 iterations
# variables trimmed down, chosen based on clinical background and statistical signifance
icu_glm_initial_5 <- glm(in_hospital_death ~ 
                           # CLINICAL VARIABLES GROUPED FROM FIRST 3 TRIAL MODELS
                           # rapid assessment tool
                           SAPS1 +
                           # kidney markers
                           BUN_max +
                           Creatinine_max +
                           Urine_diff +
                           Urine_max +
                           # blood pressure variables
                           MAP_diff +
                           MAP_max +
                           MAP_min +
                           NISysABP_min +
                           DiasABP_min +
                           # blood levels
                           PaO2_max +
                           PaO2_min +
                           HCT_min,
                         data = icu_patients_df1, family = binomial)
summary(icu_glm_initial_5)
```
'''
AIC has inflated to 700s in the latter models despite removing insignificant variables. 
Happy to start with these 13 variables for analysis.
Variables grouped (see code comments) to help with our decision making later in eliminating variables of the same category/context.
'''

# ================================================================================================

# MULTIVARIATE ANALYSIS 2: REFINING INITIAL MODELS 
# refining and combining variables from previous univariate analysis and trial models (loc, darren, dane)
revised_icu_glm_1 <- glm(in_hospital_death ~ 
                           SAPS1 +
                           BUN_max +
                           Urine_diff +
                           NISysABP_diff + #significant in univariate
                           PaO2_max,
                         family=binomial,
                         data=icu_patients_df1)
summary(revised_icu_glm)
'''
high AIC=1185.4, 
                Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -3.7853642  0.3102527 -12.201  < 2e-16 ***
SAPS1          0.1338467  0.0165059   8.109 5.10e-16 ***
BUN_max        0.0141490  0.0029292   4.830 1.36e-06 ***
Urine_diff    -0.0011399  0.0002750  -4.144 3.41e-05 ***
NISysABP_diff  0.0037958  0.0036787   1.032    0.302    
PaO2_max      -0.0011703  0.0007386  -1.584    0.113 
'''

# running same glm with omitted NAs
revised_icu_glm_1_na <- glm(in_hospital_death ~ 
                              SAPS1 +
                              BUN_max +
                              Urine_diff +
                              NISysABP_diff + #significant in univariate
                              PaO2_max,
                            family=binomial,
                            data=icudf1_na_omit)
summary(revised_icu_glm)


# revised 2
revised_icu_glm_2 <- glm(in_hospital_death ~ 
                           SAPS1 +
                           BUN_max +
                           Urine_diff +
                           NISysABP_diff + #significant in univariate
                           PaO2_max +
                           Creatinine_max,
                         family=binomial,
                         data=icu_patients_df1)
summary(revised_icu_glm_2)
'''
still high AIC: 1176.7
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)    -3.8277202  0.3136733 -12.203  < 2e-16 ***
SAPS1           0.1389557  0.0167108   8.315  < 2e-16 ***
BUN_max         0.0223714  0.0039316   5.690 1.27e-08 ***
Urine_diff     -0.0011787  0.0002807  -4.200 2.67e-05 ***
NISysABP_diff   0.0041329  0.0036985   1.117  0.26380    
PaO2_max       -0.0011942  0.0007403  -1.613  0.10674    
Creatinine_max -0.1876276  0.0615693  -3.047  0.00231 ** 
'''

# revised 2 NA df
revised_icu_glm_2_na <- glm(in_hospital_death ~ 
                              SAPS1 +
                              BUN_max +
                              Urine_diff +
                              NISysABP_diff + #significant in univariate
                              PaO2_max +
                              Creatinine_max,
                            family=binomial,
                            data=icudf1_na_omit)
summary(revised_icu_glm_2_na)
'''
much lower AIC when dataframe with omitted values used
AIC: 242.6

                 Estimate Std. Error z value Pr(>|z|)   
(Intercept)    -2.0600208  0.7231507  -2.849  0.00439 **
SAPS1           0.0994843  0.0364809   2.727  0.00639 **
BUN_max         0.0240852  0.0088983   2.707  0.00680 **
Urine_diff     -0.0005783  0.0003585  -1.613  0.10668   
NISysABP_diff  -0.0013340  0.0081299  -0.164  0.86966   
PaO2_max       -0.0018070  0.0014069  -1.284  0.19901   
Creatinine_max -0.1675827  0.1165420  -1.438  0.15045  

'''

# revised 3
# trying variables that have come out strongly significant across all models
# SAPS1, BUN_max, Creatinine_max, Urine_diff
revised_icu_glm_3 <- glm(in_hospital_death ~ SAPS1 + BUN_max + Creatinine_max + 
                           Urine_diff + Urine_max, family = binomial, data = icu_patients_df1)
summary(revised_icu_glm_3)



# ================================================================================================
# MULTIVARIATE ANALYSIS 3: VARIABLES OF INTEREST CONFIRMED
# fit into GLM, assess fit, and see if it can be reduced
icu_glm_initial_5 <- glm(in_hospital_death ~ 
                           # CLINICAL VARIABLES GROUPED FROM FIRST 3 TRIAL MODELS
                           # rapid assessment tool
                           SAPS1 +
                           # kidney markers
                           BUN_max +
                           Creatinine_max +
                           Urine_diff +
                           Urine_max +
                           # blood pressure variables
                           MAP_diff +
                           MAP_max +
                           MAP_min +
                           NISysABP_min +
                           DiasABP_min +
                           # blood levels
                           PaO2_max +
                           PaO2_min +
                           HCT_min,
                         data = icu_patients_df1, family = binomial)

# check if model can be reduced by step function
summary(icu_glm_initial_5)
step(icu_glm_initial_5, trace=1)

# running step function in icu_glm_initial_5 summary asks to remove NAs
# remove NAs from icu dataframe
icudf1_na_omit <- na.omit(icu_patients_df1)
summary(icudf1_na_omit)

# run model 5 in dataframe with omitted NAs
# *********************THIS OUR WORKING GLM MODEL AT THIS POINT********************
icu_glm_initial_5_naomit <- glm(in_hospital_death ~ 
                                  # CLINICAL VARIABLES GROUPED FROM FIRST 3 TRIAL MODELS
                                  # rapid assessment tool
                                  SAPS1 +
                                  # kidney markers
                                  BUN_max +
                                  Creatinine_max +
                                  Urine_diff +
                                  Urine_max +
                                  # blood pressure variables
                                  MAP_diff +
                                  MAP_max +
                                  MAP_min +
                                  NISysABP_min +
                                  DiasABP_min +
                                  # blood levels
                                  PaO2_max +
                                  PaO2_min +
                                  HCT_min,
                                data = icudf1_na_omit, family = binomial)
summary(icu_glm_initial_5_naomit)
# checking if working glm model can be further simplified using STEP function
step(icu_glm_initial_5_naomit , trace=1)
'''
STEP reduces the model to the following 11 variables:
glm(formula = in_hospital_death ~ SAPS1 + BUN_max + Creatinine_max + 
    Urine_diff + Urine_max + MAP_diff + MAP_max + MAP_min + PaO2_max + 
    PaO2_min + HCT_min, family = binomial, data = icudf1_na_omit)
AIC = 224.6
'''

# this is the step-reduced model from icu_glm_initial_5_naomit
# this starts with 11 variables
icu_glm_5_step_reduced_naomit <- glm(in_hospital_death ~ SAPS1 + BUN_max + Creatinine_max + 
                                  Urine_diff + Urine_max + MAP_diff + MAP_max + MAP_min + PaO2_max + 
                                  PaO2_min + HCT_min, family = binomial, data = icudf1_na_omit)
summary(icu_glm_5_step_reduced_naomit)
'''
AIC: 224.63

Intercept)    -2.392557   1.940644  -1.233  0.21763   
SAPS1           0.131656   0.044526   2.957  0.00311 **
BUN_max         0.029235   0.010227   2.859  0.00425 **
Creatinine_max -0.480251   0.158172  -3.036  0.00240 **
Urine_diff      0.013618   0.004749   2.868  0.00413 **
Urine_max      -0.014077   0.004648  -3.028  0.00246 **
MAP_diff        0.060640   0.022564   2.687  0.00720 **
MAP_max        -0.055866   0.020801  -2.686  0.00724 **
MAP_min         0.024672   0.014177   1.740  0.08181 . 
PaO2_max       -0.002673   0.001672  -1.599  0.10984   
PaO2_min        0.006746   0.004292   1.572  0.11604   
HCT_min         0.098712   0.037073   2.663  0.00775 **

From here, the following 8 variables are strongly significant:
SAPS1 + BUN_max + Creatinine_max + Urine_diff + Urine_max + MAP_diff + MAP_max + HCT_min
'''


# checking if working glm model can be further simplified using DROP function
drop1(icu_glm_initial_5_naomit, test = "Chi")
'''
SAPS1           1   207.34 233.34  9.1673 0.0024637 ** 
BUN_max         1   206.47 232.47  8.2895 0.0039876 ** 
Creatinine_max  1   210.37 236.37 12.1872 0.0004812 ***
Urine_diff      1   208.33 234.33 10.1479 0.0014447 ** 
Urine_max       1   209.51 235.51 11.3340 0.0007610 ***
MAP_diff        1   207.54 233.54  9.3652 0.0022115 ** 
MAP_max         1   207.48 233.48  9.2985 0.0022934 ** 
MAP_min         1   202.30 228.30  4.1205 0.0423675 *  
NISysABP_min    1   199.00 225.00  0.8210 0.3648960    
DiasABP_min     1   199.74 225.74  1.5634 0.2111749    
PaO2_max        1   201.46 227.46  3.2824 0.0700263 .  
PaO2_min        1   201.07 227.07  2.8888 0.0891990 .  
HCT_min         1   207.05 233.05  8.8745 0.0028919 ** 

From here, we can see that the same 8 variables (from STEP) are strongly significant
SAPS1 + BUN_max + Creatinine_max + Urine_diff + Urine_max + MAP_diff + MAP_max + HCT_min
'''

# create reduced GLM with 8 variables
# ***********************THIS IS THE PROPOSED ICU GLM MODEL*********************************
icu_glm_reduced_8_var <- glm(in_hospital_death ~ 
                                       SAPS1 + 
                                       BUN_max + 
                                       Creatinine_max + 
                                       Urine_diff + 
                                       Urine_max + 
                                       MAP_diff + 
                                       MAP_max + 
                                       HCT_min,
                                     family = binomial, data = icudf1_na_omit)
summary(icu_glm_reduced_8_var)
'''
AIC: 225.58
This is the lowest AIC with the most reasonable amount of variables. 
Although significance for MAP_diff and MAP_max are not as strong as the other variables, 
we will include these in the suggested model on a contextual/health reason.
Blood pressure is a vital sign and a clinically significant indicator of health status, especially in critical care. 

                Estimate Std. Error z value Pr(>|z|)   
(Intercept)    -1.752233   1.850728  -0.947  0.34375   
SAPS1           0.076408   0.037127   2.058  0.03959 * 
BUN_max         0.029016   0.009742   2.978  0.00290 **
Creatinine_max -0.443282   0.147070  -3.014  0.00258 **
Urine_diff      0.013485   0.004620   2.919  0.00351 **
Urine_max      -0.013843   0.004523  -3.061  0.00221 **
MAP_diff        0.036199   0.018534   1.953  0.05081 . 
MAP_max        -0.032858   0.017090  -1.923  0.05453 . 
HCT_min         0.101858   0.036812   2.767  0.00566 **

Looking at significnat variables here and will try in another model (revised_icu_glm_4)
BUN_max + Creatinine_max + Urine_diff + Urine_max + HCT_min

'''

# =======================================================================================

# MODEL COMPARISON: PSEUDO R2
# Pseudo R2 comparisons of working model(icu_glm_initial_5_naomit) and proposed ICU model (icu_glm_reduced_8_var)
library(DescTools)
options(scipen=999)
# Proposed ICU Model
r2_1 <- PseudoR2(icu_glm_reduced_8_var, which="all")
# some of models attempted 
PseudoR2(icu_glm_initial_5_naomit, which="all")
PseudoR2(icu_glm_initial_5, which="all")
PseudoR2(revised_icu_glm_2_na, which = "all")

'''
From using PseudoR2 statistics, we can see and confirm that the proposed ICU model 
has lower values compared with the other previous models.  


'''
# =======================================================================================
# FURTHER MODEL COMPARISON: AUC
# create dataframe with predicted probabilities
icudf1_na_omit2 <- icudf1_na_omit
library(pROC)
# AUC for Proposed ICU Model (icu_glm_reduced_8_var)
icudf1_na_omit2$predprob_proposed_model=predict(icu_glm_reduced_8_var, type="response")
auc(icudf1_na_omit2$in_hospital_death, icudf1_na_omit2$predprob_proposed_model)
# AUC for other Model (icu_glm_initial_5_naomit)
icudf1_na_omit2$predprob_other_model=predict(icu_glm_initial_5_naomit, type="response")
auc(icudf1_na_omit2$in_hospital_death, icudf1_na_omit2$predprob_other_model)


'''
For the proposed ICU model (icu_glm_reduced_8_var):
the Area under the curve: 0.7712. 

For the other model (icu_glm_initial_5_naomit):
Area under the curve: 0.81

The proposed ICU model (icu_glm_reduced_8_var) has a slightly lower AUC of 0.7712 compared with the other model (icu_glm_initial_5_naomit) which has 0.81.
This is not a surprise, as the latter has far more variables included in the model (13). 
It is reasonable to reduce the model and exclude the 5 variables (MAP_min, NiSysABP_min, DiasABP_min, PaO2_max, PaO2_min), 
as these refer to two main clinical indicators: Blood Pressure and Blood Oxygen Levels. 
The reduced model has enough blood pressure variables (MAP_diff, MAP_max). 
While blood O2 levels are important, this is largely variable and fluctuating in the intensive care setting, 
and largely affected by other factors (level of Oxygen support, respiratory rate) which have not been accounted for in the model. 
Excluding these variables early on effectively narrows down the clinical indicators of interest, while providing enough information for analysis of data in an intensive care context.  

'''





