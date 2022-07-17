
library(dplyr)
library(tidyverse)
library(lmtest)
library(sandwich)

# General Settings and Data Read ------------------------------------------

options(scipen=999) # tell R not to use Scientific notation
options(digits = 5) # controls how many digits are printed by default
rm(list=ls()) # del all objects and functions
df1 <- read.csv("H20181362Data.csv") # load data

# Cleaning up variables, ordering them ---------------------------------------------------

#subset MerutzeChaim, minn, MatzavMishp, DatiutYehudi, DatiutLoYehudi, TeudaGvoha, ShnotlimudKlali_C, hachnasa_lenefesh, Dat, mispYeladim, Gil
df<-df1 %>% select(MerutzeChaim, Minn, MatzavMishp, DatiutYehudi, DatiutLoYehudi, TeudaGvoha, ShnotlimudKlali_C, hachnasa_lenefesh, Dat, MispYeladim, Gil, HachnasaAvoda)


# clean merutzechaim values of 888888
table(df$MerutzeChaim)
df$MerutzeChaim[df$MerutzeChaim==888888] <- NA
table(df$MerutzeChaim)

# recode MerutzeChaim - to go up from 1 to 4
table(df$MerutzeChaim)
df$MerutzeChaim[df$MerutzeChaim==4] <- 5
df$MerutzeChaim[df$MerutzeChaim==1] <- 4
df$MerutzeChaim[df$MerutzeChaim==5] <- 1
df$MerutzeChaim[df$MerutzeChaim==2] <- 6
df$MerutzeChaim[df$MerutzeChaim==3] <- 2
df$MerutzeChaim[df$MerutzeChaim==6] <- 3
table(df$MerutzeChaim)

#create log_MerutzeChaim
df$log_MerutzeChaim<-log(df$MerutzeChaim)
table(df$log_MerutzeChaim)

#create MerutzeChaim_percentage
df$MerutzeChaim_percentage<-df$MerutzeChaim
df$MerutzeChaim_percentage[df$MerutzeChaim_percentage==1] <- 0
df$MerutzeChaim_percentage[df$MerutzeChaim_percentage==2] <- 33.3
df$MerutzeChaim_percentage[df$MerutzeChaim_percentage==3] <- 66.6
df$MerutzeChaim_percentage[df$MerutzeChaim_percentage==4] <- 100
table(df$MerutzeChaim_percentage)



#create MerutzeChaim_zscore
df$MerutzeChaim_zscores<-(df$MerutzeChaim-mean(df$MerutzeChaim,na.rm=TRUE))/sd(df$MerutzeChaim, na.rm=TRUE)
table(df$MerutzeChaim_zscores)

sd(df$MerutzeChaim, na.rm=TRUE)

#create MerutzeChaim_dummy
df$MerutzeChaim_dummy<-ifelse(df$MerutzeChaim==3 | df$MerutzeChaim==4,1,0)
table(df$MerutzeChaim)
table(df$MerutzeChaim_dummy)


# recode minn , rename minn to female
table(df$Minn)
df$Minn[df$Minn==1] <- 0
df$Minn[df$Minn==2] <- 1
table(df$Minn)
names(df)[names(df) == 'Minn'] <- "Female"
table(df$Female)

#recode matzav mishpachti
df$Married<-ifelse(df$MatzavMishp==1,1,0)
#check
table(df$Married)
table(df$MatzavMishp)

#recode datiyehudi, leave 888888, 999999 as is
table(df$DatiutYehudi)
df$DatiutYehudi[df$DatiutYehudi==5] <- 6
df$DatiutYehudi[df$DatiutYehudi==1] <- 5
df$DatiutYehudi[df$DatiutYehudi==6] <- 1
df$DatiutYehudi[df$DatiutYehudi==4] <- 6
df$DatiutYehudi[df$DatiutYehudi==2] <- 4
df$DatiutYehudi[df$DatiutYehudi==6] <- 2
table(df$DatiutYehudi)

#recode datiutloyehudi leave 888888, 999999 as is
table(df$DatiutLoYehudi)
df$DatiutLoYehudi[df$DatiutLoYehudi==4] <- 5
df$DatiutLoYehudi[df$DatiutLoYehudi==1] <- 4
df$DatiutLoYehudi[df$DatiutLoYehudi==5] <- 1
df$DatiutLoYehudi[df$DatiutLoYehudi==2] <- 6
df$DatiutLoYehudi[df$DatiutLoYehudi==3] <- 2
df$DatiutLoYehudi[df$DatiutLoYehudi==6] <- 3
table(df$DatiutLoYehudi)

#clean TeudaGvoha
table(df$TeudaGvoha)
df$TeudaGvoha[df$TeudaGvoha==888888] <- NA
df$TeudaGvoha[df$TeudaGvoha==7] <- 0
table(df$TeudaGvoha)

#create teudagvoha_factor, rename teudagvoha_factor
df$TeudaGvoha_factor<-factor(df$TeudaGvoha)
table(df$TeudaGvoha_factor)

#clean ShnotlimudKlali_C
table(df$ShnotlimudKlali_C)
df$ShnotlimudKlali_C[df$ShnotlimudKlali_C==999999] <- NA
table(df$ShnotlimudKlali_C)

#turn ShnotlimudKlali_C to discrete years
df$ShnotLimud_Discrete<-df$ShnotlimudKlali_C
table(df$ShnotLimud_Discrete)
df$ShnotLimud_Discrete[df$ShnotLimud_Discrete==6] <- 18
df$ShnotLimud_Discrete[df$ShnotLimud_Discrete==5] <- 15
df$ShnotLimud_Discrete[df$ShnotLimud_Discrete==4] <- 12
df$ShnotLimud_Discrete[df$ShnotLimud_Discrete==3] <- 9
df$ShnotLimud_Discrete[df$ShnotLimud_Discrete==2] <- 6
df$ShnotLimud_Discrete[df$ShnotLimud_Discrete==1] <- 3
table(df$ShnotlimudKlali_C)
table(df$ShnotLimud_Discrete)

#add factor to ShnotlimudKlali_C, ShnotLimud_Discrete
df$ShnotlimudKlali_C_factor <- factor(df$ShnotlimudKlali_C)
df$ShnotlimudKlali_C_discrete_factor <- factor(df$ShnotLimud_Discrete)


table(df$ShnotlimudKlali_C_factor)
table(df$ShnotlimudKlali_C_discrete_factor)
table(df$ShnotlimudKlali_C)



#create dummy variable - finished_ba_or_more
table(df$ShnotlimudKlali_C)
df$finished_ba_or_more<-ifelse(df$ShnotlimudKlali_C==6,1,0)
table(df$finished_ba_or_more)

#clean hachnasa_lenefesh
table(df$hachnasa_lenefesh)
df$hachnasa_lenefesh[df$hachnasa_lenefesh==888888] <- NA
table(df$hachnasa_lenefesh)

#clean mispYeladim
table(df$MispYeladim)
df$MispYeladim[df$MispYeladim==888888] <- NA
table(df$MispYeladim)

#mispYeladim squared
df$MispYeladim_sq<-df$MispYeladim^2
table(df$MispYeladim_sq)

#HachnasaAvoda
table(df$HachnasaAvoda)
df$HachnasaAvoda[df$HachnasaAvoda==888888] <- NA
df$HachnasaAvoda[df$HachnasaAvoda==999999] <- NA
df$HachnasaAvoda[df$HachnasaAvoda==11] <- 0
table(df$HachnasaAvoda)

#HachnasaAvoda squared
df$HachnasaAvoda_sq<-df$HachnasaAvoda^2
table(df$HachnasaAvoda)
table(df$HachnasaAvoda_sq)

#clean Dat
table(df$Dat)
df$Dat[df$Dat==888888] <-NA
table(df$Dat)


#check all variables MerutzeChaim, female, MatzavMishp, DatiutYehudi, DatiutLoYehudi, TeudaGvoha, ShnotlimudKlali_C, hachnasa_lenefesh, Dat, mispYeladim
table(df$Gil)
table(df$Female)
table(df$TeudaGvoha)
table(df$MispYeladim)
table(df$hachnasa_lenefesh)
table(df$Married)
table(df$MerutzeChaim)
table(df$MispYeladim)
table(df$MatzavMishp)
table(df$DatiutYehudi)
table(df$DatiutLoYehudi)
table(df$ShnotlimudKlali_C)


#divide population to jews and arabs. create dummy variable - jewish=1, non jewish=0
df$jewish<-ifelse(df$Dat==1,1,0)
table(df$jewish)

#create age squared
df$Gil_sq<-df$Gil^2



# Create database of Jews and non Jews and Clean ---------------------------------------------

#since reilgiousness level is divided to jews and non jews, we need to subset our data accordingly.

# subset all jews
df_jews<-subset(df, Dat==1) 
#clean df_jews datiut yehudi
table(df_jews$DatiutYehudi)
df_jews$DatiutYehudi[df_jews$DatiutYehudi==888888] <-NA
table(df_jews$DatiutYehudi)

# subset all non jews
df_non_jews<-subset(df, Dat!=1) 
#clean df_non_jews datiut yehudi
table(df_non_jews$DatiutLoYehudi)
df_non_jews$DatiutLoYehudi[df_non_jews$DatiutLoYehudi==888888] <-NA
df_non_jews$DatiutLoYehudi[df_non_jews$DatiutLoYehudi==999999] <-NA
table(df_non_jews$DatiutLoYehudi)

df_arab_muslim_and_christian<-subset(df, Dat==2 | Dat==3) 




#create dummy for religiousness level
df_jews$DatiutYehudi_dummy<-ifelse(df_jews$DatiutYehudi==3 | df_jews$DatiutYehudi==4 | df_jews$DatiutYehudi==5,1,0)
table(df_jews$DatiutYehudi)
table(df_jews$DatiutYehudi_dummy)


#create dummy for religiousness level
table(df_non_jews$DatiutLoYehudi)
df_non_jews$DatiutLoYehudi_dummy<-ifelse(df_non_jews$DatiutLoYehudi==3 | df_non_jews$DatiutLoYehudi==4,1,0)
table(df_non_jews$DatiutLoYehudi_dummy)

# Regressions 1 -----------------------------------------------------------
#possible options:

# df$ShnotLimud_Discrete
# df$ShnotlimudKlali_C
# table(df$ShnotlimudKlali_C_discrete)
# table(df$ShnotlimudKlali_C_factor)
# df$ShnotlimudKlali_C_discrete_factor
# df$MerutzeChaim
# df$MerutzeChaim_dummy
# df$MerutzeChaim_zscores
# df$MerutzeChaim_percentage
# table(df1$TeudaGvoha)

# basic first regression life satisfaction and schooling years
model1 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete, data=df) 
summary(model1)
lmtest::bptest(model1)

# basic first regression life satisfaction and and schooling years divided to categories
model1 <-lm(MerutzeChaim_zscores ~ ShnotlimudKlali_C_discrete_factor, data=df) 
summary(model1)
lmtest::bptest(model1)

# our result from the the basic regression - schooling years has a positive and significant effect on happiness


# basic first regression life satisfaction and highest degree with z scores
model1 <-lm(MerutzeChaim_zscores ~ TeudaGvoha, data=df) 
summary(model1)
lmtest::bptest(model1)

# basic first regression life satisfaction and highest degree with z scores
model1 <-lm(MerutzeChaim_zscores ~ TeudaGvoha_factor, data=df) 
summary(model1)
lmtest::bptest(model1)

#second regression - with supervising variables

# second regression life satisfaction and everything else with schooling years
model2 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete+Female+Gil+Gil_sq+Married+MispYeladim+HachnasaAvoda, data=df)
summary(model2)
lmtest::bptest(model2)

# second regression life satisfaction and everything else with schooling years
model2 <-lm(MerutzeChaim_zscores ~ ShnotlimudKlali_C_discrete_factor+Female+Gil+Gil_sq+Married+MispYeladim+HachnasaAvoda, data=df)
summary(model2)
lmtest::bptest(model2)


# second regression life satisfaction and everything else with highest degree
model2 <-lm(MerutzeChaim_zscores ~ TeudaGvoha_factor+Female+Gil+Gil_sq+Married+MispYeladim+HachnasaAvoda, data=df)
summary(model2)
lmtest::bptest(model2)

#our result - gender has no effect on reported happiness. Age has a negative effect on happiness, but the effect becomes positive after a certain age. number of children has a positive effect on happiness. income has a positive effect on happiness. being married has a positive effect on happiness.

#life satisfaction comparison for jews and non jews
model6 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete+Female+Gil+Married+MispYeladim+HachnasaAvoda+jewish, data=df)
summary(model6)
lmtest::bptest(model6)

#life satisfaction comparison for jews and non jews with interactions of gender on years of study.
model6 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete+Female+Female*ShnotLimud_Discrete+Gil+Married+MispYeladim+HachnasaAvoda+jewish, data=df)
summary(model6)
lmtest::bptest(model6)


#jews are happiers on average than non jews.

#what is the effect of being jewish on the effect of education on happiness?
model7 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete+Female+Gil+Married+MispYeladim+HachnasaAvoda+jewish+jewish*ShnotLimud_Discrete, data=df) # 
summary(model7)
lmtest::bptest(model7)

#for jews, happiness is less depenedent on the level of education. for arabs happiness is more dependent on level of education

#is there an effect of gender on happiness? seems like none. p value is wrong. after correcting still not significant
model9 <-lm(MerutzeChaim_zscores ~ Female, data=df) # 
summary(model9)
lmtest::bptest(model9)
coeftest(model9, vcov = vcovHC(model9, "HC1"))

#for jews - is there an effect of gender on happiness? seems like none. p value is wrong. after correcting still not significant
model9_jews <-lm(MerutzeChaim_zscores ~ Female, data=df_jews) # 
summary(model9_jews)
lmtest::bptest(model9)
coeftest(model9, vcov = vcovHC(model9, "HC1"))

#for non jews - is there an effect of gender on happiness? seems like none.  p value is wrong. after correcting still not significant
model9_non_jews <-lm(MerutzeChaim_zscores ~ Female, data=df_non_jews) # 
summary(model9_non_jews)
lmtest::bptest(model9)
coeftest(model9, vcov = vcovHC(model9, "HC1"))


# for jews effect of level of religiousness on happiness 
model10 <-lm(MerutzeChaim_zscores ~ DatiutYehudi, data=df_jews) # 
summary(model10)
lmtest::bptest(model10)

#for jews more religious is more happy for basic regression. now add additional supervising parameters

model10_add_supervising_variables <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete+Female+Gil+Gil_sq+Married+MispYeladim+HachnasaAvoda+DatiutYehudi, data=df_jews) # 
summary(model10_add_supervising_variables)
lmtest::bptest(model10_add_supervising_variables)

# after adding supervising variables the effect of religiousness level on happniess is still strong in jews

# for non jews effect of level of religiousness on happiness  p value is wrong?
model11 <-lm(MerutzeChaim_zscores ~ DatiutLoYehudi, data=df_non_jews) # 
summary(model11)
lmtest::bptest(model11)

#regression of religion on happiness with factor p value is wrong? after correcting not much difference
model12 <-lm(MerutzeChaim_zscores ~ factor(Dat), data=df_non_jews) #
summary(model12)
lmtest::bptest(model12)
coeftest(model12, vcov = vcovHC(model12, "HC1"))

#regression of religion on happiness with factor 
model12 <-lm(MerutzeChaim_zscores ~ factor(Dat), data=df) #
summary(model12)
lmtest::bptest(model12)

#same effect for non jews and for arabs and muslems.
#for non jews there is no significant effect for religious level on happiness.

# for jews - regression of education and happiness
model13 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete, data=df_jews) 
summary(model13)
lmtest::bptest(model13)

# for non jews - regression of education and happiness
model13 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete, data=df_non_jews) 
summary(model13)
lmtest::bptest(model13)

#conclusion -for basic regression, for jews, the effect of education on happiness is stronger than for arabs. For both jews and non jews the effect is significant and positive.

#adding all parameters
#for each group jews and non jews, calculating the effect of education on happiness, with additional supervising variables.
model15 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete+ShnotLimud_Discrete*jewish+Female+Gil+Married+MispYeladim+MispYeladim_sq+HachnasaAvoda, data=df) 
summary(model15)
lmtest::bptest(model15)

#for extended model, for jews, the effect of education on happiness is less strong than for arabs. For both jews and non jews the effect is significant and positive.

#effect of level or religiousness on return of education to happiness

# for jews - simple regression of education and happiness with level of religiousness
model17 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete+DatiutYehudi_dummy*ShnotLimud_Discrete, data=df_jews) 
summary(model17)
lmtest::bptest(model17)

# for non jews - simple regression of education and happiness with level of religiousness
model18 <-lm(MerutzeChaim_zscores ~ ShnotLimud_Discrete+DatiutLoYehudi_dummy*ShnotLimud_Discrete, data=df_non_jews) 
summary(model18)
lmtest::bptest(model18)


