
install.packages("survival")
install.packages("dplyr")


icu_patients_df1
summary(icu_patients_df1)

# convert days survived to years
icu_patients_df1$survived_years <- icu_patients_df1$Days / 365
head(icu_patients_df1)


# check column number for survived_years
colnames(icu_patients_df1) # becomes column number [121]

# summary of only patients who died
summary(icu_patients_df1[icu_patients_df1$in_hospital_death ==1,121])

# summary of patients who died omitting NAs (summaray allows for this anyway)
summary(na.omit(icu_patients_df1[icu_patients_df1$in_hospital_death ==1,121]))

hist(icu_patients_df1[icu_patients_df1$in_hospital_death ==1,5])
mean(na.omit(icu_patients_df1[,121]))
# histogram of patients who died in icu
hist(icu_patients_df1$survived_years)

# create age_at_deatth variable
icu_patients_df1$age_at_death <- icu_patients_df1$Age + 
  icu_patients_df1$survived_years # column number [122]
summary(icu_patients_df1$age_at_death)

# plot age at death
hist(icu_patients_df1$age_at_death, xlab="Age at death in ICU")

# visualise the age of patients and number of days survived
ggplot(data=icu_patients_df1, aes(colour=Gender)) +
  geom_point(mapping=aes(x=icu_patients_df1$age_at_death, 
                         y=icu_patients_df1$survived_years)) + 
  labs(x="AGE", y="YEARS SURVIVED") +
  scale_x_continuous(breaks=seq(20,100,10))+ 
  scale_y_continuous(breaks=seq(0,7,1))

install.packages("tidyverse")
install.packages("dplyr")
# create df with deaths only patients
icu_patdf_deaths <- icu_patients_df1[icu_patients_df1$in_hospital_death==1,]

# visualise again but only for patients that died
ggplot(data=icu_patdf_deaths, aes(colour=Gender)) +
  geom_point(mapping=aes(x=icu_patdf_deaths$age_at_death, 
                         y=icu_patdf_deaths$survived_years)) + 
  labs(x="AGE", y="YEARS SURVIVED") +
  scale_x_continuous(breaks=seq(20,100,10)) + 
  scale_y_continuous(breaks=seq(0,7,1))

# display distribution of years at death in icu
hist(icu_patients_df1[icu_patients_df1$in_hospital_death >=1,5], 
     freq=FALSE, breaks=100, xlab="Days til death in ICU")

# visualise again for icu_patdf_deaths survived years
hist(icu_patdf_deaths$survived_years)

# PDF of follow up times (ie total time in study)
hist(icu_patients_df1$Length_of_stay, prob=TRUE, ylim=c(0,0.08),
     col="grey",
     main="PDF of follow up times", 
     xlab="Length of stay")
lines(density(icu_patients_df1$Length_of_stay), col="red") 

# compare with days followed if died
hist(icu_patients_df1[icu_patients_df1$in_hospital_death >=1,5], prob=TRUE, ylim=c(0,0.08),
     col="grey",
     main="PDF of follow up times for patients that died", 
     xlab="Length of stay")
lines(density(icu_patients_df1[icu_patients_df1$in_hospital_death >=1,5]), col="red") 

# calculate the CDF values for lenth of stay
Length_of_stay.ecdf = ecdf(icu_patients_df1$Length_of_stay)

# plot CDF of length of stay for all patients
plot(Length_of_stay.ecdf, main="CDF of length of stay",
     xlab="Length of stay")

# calculate the CDF for age at death
Age_at_death.ecdf = ecdf(icu_patients_df1$age_at_death)

# plot CDF for Age at death
plot(Age_at_death.ecdf, main="CDF of Age at death",
     xlab="Age at death")

# plot median follow time
Length_of_stay.ecdf = ecdf(icu_patients_df1$Length_of_stay)
plot(Length_of_stay.ecdf, main="CDF of length of stay",
     xlab="Length of stay")
summary(icu_patients_df1$Length_of_stay)

# add the median
abline(v=13.74, h=0.5, col="red")
legend(5.5, 0.45, 'median=13.74',col=1:10,lty=1:10, cex=0.8, xjust=-1)

# load the survival package
install.packages("survival")

# create the survival object
LengthOfstay.surv1 <- survfit(Surv(Length_of_stay, Status) ~ 1,
                              data=icu_patients_df1)
plot(LengthOfstay.surv1,
     main="Survival function",
     xlab="Follow-up time",
     conf.int=TRUE)

# same plot with Age on x Axis with age starting at 20yrs
LengthOfstay.surv2 <- survfit(Surv(Age, age_at_death, Status) ~ 1,
                              data=icu_patients_df1)
plot(LengthOfstay.surv2, xlim=c(20,100),
     main="Survival function",
     xlab="Age")
abline(v=48, h=0.5, col="red")

# install and load the bshazard package
install.packages("bshazard")
library(bshazard)

# test for Hazard function
icudeaths.haz1 <- bshazard(Surv(Length_of_stay, Status) ~ 1,
                          data=icu_patients_df1)
plot(icudeaths.haz,
     main="Hazard function",
     xlab="Follow-up time",
     ylim=c(0.005,0.1))

# plotting cumulative hazard distribution with age on x axis
icudeaths.haz2 <- bshazard(Surv(Age, age_at_death, Status) ~ 1,
                          data=icu_patients_df1)
plot(icudeaths.haz2,
     main="Hazard function",
     xlab="Age")

# create objext for survival full df
icu.fit <- survfit(Surv(Age, age_at_death, Status) ~ 1,
                   data=icu_patients_df1)
plot(icu.fit, xlim=c(20,100),
     main='Kaplan-Meier estimate of survival function',
     xlab='Age')

# create dataset for sample
indx <- sample(nrow(icu_patients_df1),
               size=50,
               replace=FALSE)
rsa <- icu_patients_df1[indx,]
rsa
rsa.fit <- survfit(Surv(Age, age_at_death, Status) ~ 1,
                   data=rsa)
summary(rsa.fit)
plot(rsa.fit, xlim=c(20,100),
     main='Kaplan-Meier estimate of survival function',
     xlab='Age')

print(icu.fit, print.rmean=TRUE)

plot(icu.fit, xlim=c(20,100),
     fun= "cumhaz",
     main="Nelson-Aalen estimate of cumulative hazard function",
     xlab="Age")

# plot survival function by gender
icu.fitbyGender <- survfit(Surv(Age, age_at_death, Status) ~ Gender,
                           data=icu_patients_df1)
plot(icu.fitbyGender,
     col=c("blue","red"),
     xlim=c(20,100),
     main="KP estimate by gender",
     xlab="Age")

icu.fitbyGender2 <- coxph(Surv(Age, age_at_death, Status) ~ Gender,
                              data=icu_patients_df1)
summary(icu.fitbyGender2)
plot(icu.fitbyGender2,
     col=c("blue","red"),
     xlim=c(20,100),
     main="KP estimate by gender",
     xlab="Age")
