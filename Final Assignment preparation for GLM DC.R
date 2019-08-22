
icu_patients_df1 <- readRDS("icu_patients_df1.rds")


full_icumod <- glm(in_hospital_death ~ ., family = binomial, data = icu_patients_df1)
full_icumod
summary(full_icumod)
step(full_icumod, trace=1)



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

summary(icudf1_na_omit)

summary(icu_glm_initial_5)
step(icu_glm_initial_5, trace=1)
icu_glm_init5_na_removed <- na.omit(icu_glm_initial_5)
step(icu_glm_init5_na_removed)


icudf1_na_omit <- na.omit(icu_patients_df1)

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
step(icu_glm_initial_5_naomit , trace=1)




