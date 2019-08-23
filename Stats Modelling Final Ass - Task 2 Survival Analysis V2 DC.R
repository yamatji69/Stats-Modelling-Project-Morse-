
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
LengthOfstay.surv1 <- survfit(Surv(Length_of_stay, age_at_death) ~ 1,
                              data=icu_patients_df1)

plot(LengthOfstay.surv1,
     main="Survival function",
     xlab="Follow up time",
     conf.int=FALSE)

