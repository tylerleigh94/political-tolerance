####Libraries####
library(easypackages)
libs<-c("tidyverse", "haven", "car", "psych", "FactoMineR", "naniar", "labelled", "plm", "lubridate")
libraries(libs)
####Import Data####
dat.orig.4 <- read_sav("Data/s7991_UPenn_LongPollW4_FINAL_WEIGHTED_client_7.11.2019.sav")
dat.orig.5 <- read_sav("Data/7991_UPenn_LongPoll W5_client_04.13.2020.sav")

# Rename datasets
dat.4 <- dat.orig.4
dat.5 <- dat.orig.5

####Cleaning and Recodes####

#Rename Wave 5 columns so code still works (early data did not have suffixes, but latest data does)
colnames.5<-colnames(dat.5)
colnames.5.stripped <- gsub("_W5", "", colnames.5)
colnames(dat.5)<-colnames.5.stripped

# Set up values for NA
na.values.2<-c(77, 98, 99)
na.values.3<- c(777, 998, 999)

# Indices for therms
therms.4<-grep("THERM", colnames(dat.4), fixed=T)
therms.5<-grep("THERM", colnames(dat.5), fixed=T)

# Vector of columns with 2-digit NAs
clean.length2_4<-colnames(dat.4[c(-1,-therms.4)])
clean.length2_5<-colnames(dat.5[c(-1,-therms.5)])

# Vector of columns with 3-digit NAs
clean.length3_4<- colnames(dat.4[therms.4])
clean.length3_5<- colnames(dat.5[therms.5])

# Remove NAs
dat.4 <- dat.4 %>%
  replace_with_na_at(.vars=clean.length2_4, condition = ~.x %in% na.values.2)
dat.4 <- dat.4 %>%
  replace_with_na_at(.vars=clean.length3_4, condition = ~.x %in% na.values.3)

dat.5 <- dat.5 %>%
  replace_with_na_at(.vars=clean.length2_5, condition = ~.x %in% na.values.2)
dat.5 <- dat.5 %>%
  replace_with_na_at(.vars=clean.length3_5, condition = ~.x %in% na.values.3)

#Demographics (wave 5 only)
dat.5$white<-ifelse(dat.5$RACETHNICITY==1, 1, 0)
dat.5$male<-ifelse(dat.5$GENDER==1, 1, 0)
dat.5$party3<-ifelse(dat.5$P_PARTYID7<=3, -1, 
                     ifelse(dat.5$P_PARTYID7>=5, 1, 0))

#Political Tolerance

##Wave 4
dat.4$tol1 <- recode(dat.4$POL_T2_1_W4, "5=0; 4=1; 3=2; 2=3; 1=4; else=NA")
dat.4$tol2 <- recode(dat.4$POL_T2_2_W4, "5=4; 4=3; 3=2; 2=1; 1=0; else=NA")
dat.4$tol3 <- recode(dat.4$POL_T2_3_W4, "5=4; 4=3; 3=2; 2=1; 1=0; else=NA")

###Index and reliability
index.tol4 <- grep("tol", colnames(dat.4))
dat.4$poltol3_W4 <- rowMeans(dat.4[,index.tol4])
alpha.tol4<-alpha(dat.4[,index.tol4])

##Wave 5
dat.5$tol1 <- recode(dat.5$POL_T2_1, "5=0; 4=1; 3=2; 2=3; 1=4; else=NA")
dat.5$tol2 <- recode(dat.5$POL_T2_2, "5=4; 4=3; 3=2; 2=1; 1=0; else=NA")
dat.5$tol3 <- recode(dat.5$POL_T2_3, "5=4; 4=3; 3=2; 2=1; 1=0; else=NA")
dat.5$tol4 <- recode(dat.5$POL_T2_4, "5=4; 4=3; 3=2; 2=1; 1=0; else=NA")
dat.5$tol5 <- recode(dat.5$POL_T2_5, "5=0; 4=1; 3=2; 2=3; 1=4; else=NA")
dat.5$tol6 <- recode(dat.5$POL_T2_6, "5=0; 4=1; 3=2; 2=3; 1=4; else=NA")

###Index and reliability
index.tol5.3 <- grep("tol", colnames(dat.5))[1:3]
index.tol5.6 <- grep("tol", colnames(dat.5))[1:6]
dat.5$poltol3 <- rowMeans(dat.5[,index.tol5.3])
alpha.tol5.3<-alpha(dat.5[,index.tol5.3])
dat.5$poltol6 <- rowMeans(dat.5[,index.tol5.6])
alpha.tol5.6<-alpha(dat.5[,index.tol5.6])

# Social Media use

##Wave 4
na.values.2<-c(77, 98, 99)
dat.4 <- dat.4 %>%
  replace_with_na_at(.vars=c("SOCIAL1_1_W4", "SOCIAL1_2_W4", "SOCIAL1_3_W4", "SOCIAL1_4_W4"), 
                     condition = ~.x %in% na.values.2)

##Wave 5
dat.5 <- dat.5 %>%
  replace_with_na_at(.vars=c("SOCIAL1_1", "SOCIAL1_2", "SOCIAL1_3", "SOCIAL1_4"), 
                     condition = ~.x %in% na.values.2)

# Longitudinal SM use (0=never; 1=about once a week or less; 2=few times weekly; 3=daily; 4= many times daily)

## Wave 4
dat.4$fb.use5_W4 <- car::recode(dat.4$SOCIAL1_1_W4, "1=0; 2=1; 3=1; 4=2; 5=3; 6=4")
dat.4$yt.use5_W4 <- car::recode(dat.4$SOCIAL1_2_W4, "1=0; 2=1; 3=1; 4=2; 5=3; 6=4")
dat.4$tw.use5_W4 <- car::recode(dat.4$SOCIAL1_3_W4, "1=0; 2=1; 3=1; 4=2; 5=3; 6=4")
dat.4$ig.use5_W4 <- car::recode(dat.4$SOCIAL1_4_W4, "1=0; 2=1; 3=1; 4=2; 5=3; 6=4")

## Wave 5
dat.5$fb.use5 <- car::recode(dat.5$SOCIAL1_1, "1=0; 2=1; 3=1; 4=1; 5=2; 6=3; 7=4")
dat.5$yt.use5 <- car::recode(dat.5$SOCIAL1_2, "1=0; 2=1; 3=1; 4=1; 5=2; 6=3; 7=4")
dat.5$tw.use5 <- car::recode(dat.5$SOCIAL1_3, "1=0; 2=1; 3=1; 4=1; 5=2; 6=3; 7=4")
dat.5$ig.use5 <- car::recode(dat.5$SOCIAL1_4, "1=0; 2=1; 3=1; 4=1; 5=2; 6=3; 7=4")

# Dummies for SM use

##dummy for social media platform use in wave 4
dat.4$fb.use.dummy_W4<-ifelse(dat.4$SOCIAL1_1_W4>1, 1, 0)
dat.4$yt.use.dummy_W4<-ifelse(dat.4$SOCIAL1_2_W4>1, 1, 0)
dat.4$tw.use.dummy_W4<-ifelse(dat.4$SOCIAL1_3_W4>1, 1, 0)
dat.4$ig.use.dummy_W4<-ifelse(dat.4$SOCIAL1_4_W4>1, 1, 0) 

##dummy for social media platform use in wave 5
dat.5$fb.use.dummy<-ifelse(dat.5$SOCIAL1_1>1, 1, 0)
dat.5$yt.use.dummy<-ifelse(dat.5$SOCIAL1_2>1, 1, 0)
dat.5$tw.use.dummy<-ifelse(dat.5$SOCIAL1_3>1, 1, 0)
dat.5$ig.use.dummy<-ifelse(dat.5$SOCIAL1_4>1, 1, 0) 

# Perceived Polarization (0= Perceive no polarization; 3=A Lot of polarization)

dat.4$ppol1<-dat.4$PP1_W4
dat.4$ppol2<-dat.4$PP2_W4
dat.4$ppol3<-recode(dat.4$PP3_W4, "1=4; 2=3; 3=2; 4=1")
dat.4$ppol4<-recode(dat.4$PP4_W4, "1=4; 2=3; 3=2; 4=1")

dat.5$ppol1<-dat.5$PP1
dat.5$ppol2<-dat.5$PP2
dat.5$ppol3<-recode(dat.5$PP3, "1=4; 2=3; 3=2; 4=1")
dat.5$ppol4<-recode(dat.5$PP4, "1=4; 2=3; 3=2; 4=1")

##Single index and reliability

ppol.index_4<-which(colnames(dat.4) %in% c("ppol1", "ppol2", "ppol3", "ppol4"))
dat.4$ppol.ind_W4<-rowMeans(dat.4[,ppol.index_4])-1
alpha.ppol_4<-psych::alpha(dat.4[,ppol.index_4])

ppol.index_5<-which(colnames(dat.5) %in% c("ppol1", "ppol2", "ppol3", "ppol4"))
dat.5$ppol.ind <-rowMeans(dat.5[,ppol.index_5])-1
alpha.ppol_5<-psych::alpha(dat.5[,ppol.index_5])

#Civility Index; (0,1), high=civil

dat.4$civil.1<-dat.4$CIVIL1_W4
dat.4$civil.2<-recode(dat.4$CIVIL2_W4, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat.4$civil.3<-dat.4$CIVIL3_W4
dat.4$civil.4<-recode(dat.4$CIVIL4_W4, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat.4$civil.5<-dat.4$CIVIL5_W4

dat.5$civil.1<-dat.5$CIVIL1
dat.5$civil.2<-recode(dat.5$CIVIL2, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat.5$civil.3<-dat.5$CIVIL3
dat.5$civil.4<-recode(dat.5$CIVIL4, "1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1")
dat.5$civil.5<-dat.5$CIVIL5

##Create scale, standardized (0,1), and alpha

civil.index_4<-which(colnames(dat.4) %in% c("civil.1", "civil.2", "civil.3", "civil.4", "civil.5"))
dat.4$civil.ind_W4 <- (rowMeans(dat.4[,civil.index_4])-1)/9
alpha.civil_4<-psych::alpha(dat.4[civil.index_4])

civil.index_5<-which(colnames(dat.5) %in% c("civil.1", "civil.2", "civil.3", "civil.4", "civil.5"))
dat.5$civil.ind <- (rowMeans(dat.5[,civil.index_5])-1)/9
alpha.civil_5<-psych::alpha(dat.5[civil.index_5])

#Interest (0= no interest; 3=very interested)
dat.4$interest_W4<-4-dat.4$Q16_W4
dat.5$interest <- 4-dat.5$Q16

#Create measure of passive political info exposure per platform; [0,3] 3= high exposure
dat.4 <- dat.4 %>%
  mutate(fb.pass.exp_W4=SOCIAL3_1_W4-1, yt.pass.exp_W4=SOCIAL3_2_W4-1, tw.pass.exp_W4=SOCIAL3_3_W4-1,
         ig.pass.exp_W4=SOCIAL3_4_W4-1)

dat.5 <- dat.5 %>%
  mutate(fb.pass.exp=SOCIAL3_1-1, yt.pass.exp=SOCIAL3_2-1, tw.pass.exp=SOCIAL3_3-1,
         ig.pass.exp=SOCIAL3_4-1)

#Create index of Social Media Use
dat.4 <- dat.4 %>%
  mutate(sm.ind_W4=fb.use5_W4+tw.use5_W4+ig.use5_W4+yt.use5_W4)

dat.5 <- dat.5 %>%
  mutate(sm.ind=fb.use5+tw.use5+ig.use5+yt.use5)

# Separate sample halves by date
dat.5$end.date <- ymd_hms(dat.5$ENDDT)

dat.5$second.half <- ifelse(dat.5$end.date<ymd("2020-03-04"), 0, 
                            ifelse(dat.5$end.date>ymd("2020-03-04"), 1, NA))

# Tolerance online versus offline (Wave 5 only)

dat.5$poltol.off<-rowMeans(dat.5[,which(colnames(dat.5) %in% c("tol1", "tol4", "tol5", "tol6"))])
dat.5$poltol.on<-rowMeans(dat.5[,which(colnames(dat.5) %in% c("tol2", "tol3"))])

dat.5 <- dat.5 %>%
  mutate(poltol.diff=poltol.off-poltol.on)


#####################################################
#### Variable Endings manipulation ####
dat.4 <- dat.4 %>%
  rename_at(vars(ends_with("_W4")), funs(str_replace(., "_W4$", "_W1")))

cols.5<-colnames(dat.5)
cols.5<-c(cols.5[1], paste(cols.5[-1], "_W2", sep=""))
colnames(dat.5)<-cols.5

#####################################################
####Analyses####

# Panel Analyses
##Merge, melt, and select variables

dat.p <- dat.4 %>%
  full_join(dat.5, by="CaseId") %>%
  remove_labels() %>%
  pivot_longer(-CaseId, names_to = c(".value", "wave"), names_sep="_W") %>%
  select(CaseId, wave, poltol3, fb.use5, yt.use5, tw.use5, ig.use5, civil.ind, ppol.ind, interest,
         fb.pass.exp, yt.pass.exp, tw.pass.exp, ig.pass.exp, fb.use.dummy, yt.use.dummy, tw.use.dummy,
         ig.use.dummy, sm.ind)

dat.p<-dat.p[dat.p$wave %in% c(1:2),]

##Write out to stata
dat.p.out<-dat.p
colnames(dat.p.out) <- gsub("\\.", "_", colnames(dat.p.out))
foreign::write.dta(dat.p.out, file="Panel Data Export to Stata.dta")

##Run the analyses

m.face<-plm(formula = poltol3~fb.use5+interest+fb.pass.exp, data=dat.p, model='within', index='CaseId', effect='individual')
m.yt<-plm(formula = poltol3~yt.use5+interest+yt.pass.exp, data=dat.p, model='within', index='CaseId', type='individual')
m.twit<-plm(formula = poltol3~tw.use5+interest+tw.pass.exp, data=dat.p, model='within', index='CaseId', type='individual')
m.insta<-plm(formula = poltol3~ig.use5+interest+ig.pass.exp, data=dat.p, model='within', index='CaseId', type='individual')
m.all<-plm(formula = poltol3~sm.ind+interest+fb.pass.exp+ig.pass.exp+tw.pass.exp+yt.pass.exp, data=dat.p, model='within', index='CaseId', type='individual')

m.face.select <- plm(formula = fb.use5~poltol3+interest+fb.pass.exp, data=dat.p, model='within', index='CaseId', effect='individual')

m.all.select<-plm(formula = sm.ind~poltol3+interest+fb.pass.exp+ig.pass.exp+tw.pass.exp+yt.pass.exp, data=dat.p, model='within', index='CaseId', type='individual')


##Make a figure
###Get the data
dat.panel<-data.frame(matrix(NA, nrow=5, ncol=4))
dat.panel[1,1]<-"Facebook"
dat.panel[1,2]<-coef(m.face)[1]
dat.panel[1,3]<-confint(m.face)[1,1]
dat.panel[1,4]<-confint(m.face)[1,2]
dat.panel[2,1]<-"Instagram"
dat.panel[2,2]<-coef(m.insta)[1]
dat.panel[2,3]<-confint(m.insta)[1,1]
dat.panel[2,4]<-confint(m.insta)[1,2]
dat.panel[3,1]<-"Twitter"
dat.panel[3,2]<-coef(m.twit)[1]
dat.panel[3,3]<-confint(m.twit)[1,1]
dat.panel[3,4]<-confint(m.twit)[1,2]
dat.panel[4,1]<-"YouTube"
dat.panel[4,2]<-coef(m.yt)[1]
dat.panel[4,3]<-confint(m.yt)[1,1]
dat.panel[4,4]<-confint(m.yt)[1,2]
dat.panel[5,1]<-"Combined"
dat.panel[5,2]<-coef(m.all)[1]
dat.panel[5,3]<-confint(m.all)[1,1]
dat.panel[5,4]<-confint(m.all)[1,2]

colnames(dat.panel)<-c("Platform", "Estimate", "lwr", "upr")
dat.panel$Platform<-factor(dat.panel$Platform, levels=c("Facebook", "Instagram", "Twitter", "YouTube", "Combined"))

###Make the Figure
ggplot(aes(x=Platform), data=dat.panel)+
  geom_point(aes(y=Estimate))+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.3)+
  geom_hline(yintercept=0, color='red')+
  theme_bw()+
  theme(plot.title = element_text(size=24, hjust=0.5),
        plot.caption = element_text(size=10, hjust = .5), 
        axis.title = element_text(size=12), 
        axis.text = element_text(size=12))

# Political Tolerance Online vs Offline

m.tol1<-lm(poltol6_W2~fb.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+fb.pass.exp_W2, data=dat.5)
m.tol2<-lm(poltol6_W2~ig.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+ig.pass.exp_W2, data=dat.5)
m.tol3<-lm(poltol6_W2~tw.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+tw.pass.exp_W2, data=dat.5)
m.tol4<-lm(poltol6_W2~yt.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+yt.pass.exp_W2, data=dat.5)
m.tol5<-lm(poltol6_W2~sm.ind_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+fb.pass.exp_W2+ig.pass.exp_W2+tw.pass.exp_W2+yt.pass.exp_W2, data=dat.5)


m.poltol.on.1<-lm(poltol.on_W2~fb.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+fb.pass.exp_W2, data=dat.5)
m.poltol.on.2<-lm(poltol.on_W2~ig.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+ig.pass.exp_W2, data=dat.5)
m.poltol.on.3<-lm(poltol.on_W2~tw.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+tw.pass.exp_W2, data=dat.5)
m.poltol.on.4<-lm(poltol.on_W2~yt.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+yt.pass.exp_W2, data=dat.5)
m.poltol.on.5<-lm(poltol.on_W2~sm.ind_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+fb.pass.exp_W2+ig.pass.exp_W2+tw.pass.exp_W2+yt.pass.exp_W2, data=dat.5)


m.poltol.off.1<-lm(poltol.off_W2~fb.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+fb.pass.exp_W2, data=dat.5)
m.poltol.off.2<-lm(poltol.off_W2~ig.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+ig.pass.exp_W2, data=dat.5)
m.poltol.off.3<-lm(poltol.off_W2~tw.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+tw.pass.exp_W2, data=dat.5)
m.poltol.off.4<-lm(poltol.off_W2~yt.use5_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+yt.pass.exp_W2, data=dat.5)
m.poltol.off.5<-lm(poltol.off_W2~sm.ind_W2+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2+second.half_W2+interest_W2+fb.pass.exp_W2+ig.pass.exp_W2+tw.pass.exp_W2+yt.pass.exp_W2, data=dat.5)

##Get the data
dat.cross<-data.frame(matrix(NA, nrow=15, ncol=5))
dat.cross[1,1] <-"Facebook"
dat.cross[1,2] <- "Combined"
dat.cross[1,3] <- coef(m.tol1)[2]
dat.cross[1,4] <- confint(m.tol1)[2,1]
dat.cross[1,5] <- confint(m.tol1)[2,2]
dat.cross[2,1] <-"Facebook"
dat.cross[2,2] <- "Online"
dat.cross[2,3] <- coef(m.poltol.on.1)[2]
dat.cross[2,4] <- confint(m.poltol.on.1)[2,1]
dat.cross[2,5] <- confint(m.poltol.on.1)[2,2]
dat.cross[3,1] <-"Facebook"
dat.cross[3,2] <- "Offline"
dat.cross[3,3] <- coef(m.poltol.off.1)[2]
dat.cross[3,4] <- confint(m.poltol.off.1)[2,1]
dat.cross[3,5] <- confint(m.poltol.off.1)[2,2]

dat.cross[4,1] <-"Instagram"
dat.cross[4,2] <- "Combined"
dat.cross[4,3] <- coef(m.tol2)[2]
dat.cross[4,4] <- confint(m.tol2)[2,1]
dat.cross[4,5] <- confint(m.tol2)[2,2]
dat.cross[5,1] <-"Instagram"
dat.cross[5,2] <- "Online"
dat.cross[5,3] <- coef(m.poltol.on.2)[2]
dat.cross[5,4] <- confint(m.poltol.on.2)[2,1]
dat.cross[5,5] <- confint(m.poltol.on.2)[2,2]
dat.cross[6,1] <-"Instagram"
dat.cross[6,2] <- "Offline"
dat.cross[6,3] <- coef(m.poltol.off.2)[2]
dat.cross[6,4] <- confint(m.poltol.off.2)[2,1]
dat.cross[6,5] <- confint(m.poltol.off.2)[2,2]

dat.cross[7,1] <-"Twitter"
dat.cross[7,2] <- "Combined"
dat.cross[7,3] <- coef(m.tol3)[2]
dat.cross[7,4] <- confint(m.tol3)[2,1]
dat.cross[7,5] <- confint(m.tol3)[2,2]
dat.cross[8,1] <-"Twitter"
dat.cross[8,2] <- "Online"
dat.cross[8,3] <- coef(m.poltol.on.3)[2]
dat.cross[8,4] <- confint(m.poltol.on.3)[2,1]
dat.cross[8,5] <- confint(m.poltol.on.3)[2,2]
dat.cross[9,1] <-"Twitter"
dat.cross[9,2] <- "Offline"
dat.cross[9,3] <- coef(m.poltol.off.3)[2]
dat.cross[9,4] <- confint(m.poltol.off.3)[2,1]
dat.cross[9,5] <- confint(m.poltol.off.3)[2,2]

dat.cross[10,1] <-"YouTube"
dat.cross[10,2] <- "Combined"
dat.cross[10,3] <- coef(m.tol4)[2]
dat.cross[10,4] <- confint(m.tol4)[2,1]
dat.cross[10,5] <- confint(m.tol4)[2,2]
dat.cross[11,1] <-"YouTube"
dat.cross[11,2] <- "Online"
dat.cross[11,3] <- coef(m.poltol.on.4)[2]
dat.cross[11,4] <- confint(m.poltol.on.4)[2,1]
dat.cross[11,5] <- confint(m.poltol.on.4)[2,2]
dat.cross[12,1] <-"YouTube"
dat.cross[12,2] <- "Offline"
dat.cross[12,3] <- coef(m.poltol.off.4)[2]
dat.cross[12,4] <- confint(m.poltol.off.4)[2,1]
dat.cross[12,5] <- confint(m.poltol.off.4)[2,2]

dat.cross[13,1] <-"Combined"
dat.cross[13,2] <- "Combined"
dat.cross[13,3] <- coef(m.tol5)[2]
dat.cross[13,4] <- confint(m.tol5)[2,1]
dat.cross[13,5] <- confint(m.tol5)[2,2]
dat.cross[14,1] <-"Combined"
dat.cross[14,2] <- "Online"
dat.cross[14,3] <- coef(m.poltol.on.5)[2]
dat.cross[14,4] <- confint(m.poltol.on.5)[2,1]
dat.cross[14,5] <- confint(m.poltol.on.5)[2,2]
dat.cross[15,1] <-"Combined"
dat.cross[15,2] <- "Offline"
dat.cross[15,3] <- coef(m.poltol.off.5)[2]
dat.cross[15,4] <- confint(m.poltol.off.5)[2,1]
dat.cross[15,5] <- confint(m.poltol.off.5)[2,2]

colnames(dat.cross)<-c("Platform", "Tolerance", "Estimate", "lwr", "upr")
dat.cross$Platform<-factor(dat.cross$Platform, levels=c("Facebook", "Instagram", "Twitter", "YouTube", "Combined"))

###Make the Figure
ggplot(aes(x=Platform, shape=Tolerance), data=dat.cross)+
  geom_point(aes(y=Estimate), position=position_dodge(width=.7))+
  geom_errorbar(aes(ymin=lwr, ymax=upr), position=position_dodge(width=.7), width=.3)+
  geom_hline(yintercept=0, color='red')+
  theme_bw()+
  theme(plot.title = element_text(size=24, hjust=0.5),
        plot.caption = element_text(size=10, hjust = .5), 
        axis.title = element_text(size=12), 
        axis.text = element_text(size=12))

#Descriptives on tolerance by platform
dat.tol<-as_tibble(c("Facebook", "YouTube", "Twitter", "Instagram"))
dat.tol$Tolerance <- as.numeric(c(mean(dat.5$poltol6_W2[dat.5$fb.use5_W2>0], na.rm=T), 
                                  mean(dat.5$poltol6_W2[dat.5$yt.use5_W2>0], na.rm=T),
                                  mean(dat.5$poltol6_W2[dat.5$tw.use5_W2>0], na.rm=T), 
                                  mean(dat.5$poltol6_W2[dat.5$ig.use5_W2>0], na.rm=T)))

dat.tol$upr <- c(mean(dat.5$poltol6_W2[dat.5$fb.use5_W2>0], na.rm=T)+
                   (1.96*sd(dat.5$poltol6_W2[dat.5$fb.use5_W2>0], na.rm=T))/
                   sqrt(length(dat.5$poltol6_W2[dat.5$fb.use5_W2>0])), 
                 mean(dat.5$poltol6_W2[dat.5$yt.use5_W2>0], na.rm=T)+
                   (1.96*sd(dat.5$poltol6_W2[dat.5$yt.use5_W2>0], na.rm=T))/
                   sqrt(length(dat.5$poltol6_W2[dat.5$yt.use5_W2>0])), 
                 mean(dat.5$poltol6_W2[dat.5$tw.use5_W2>0], na.rm=T)+
                   (1.96*sd(dat.5$poltol6_W2[dat.5$tw.use5_W2>0], na.rm=T))/
                   sqrt(length(dat.5$poltol6_W2[dat.5$tw.use5_W2>0])), 
                 mean(dat.5$poltol6_W2[dat.5$ig.use5_W2>0], na.rm=T)+
                   (1.96*sd(dat.5$poltol6_W2[dat.5$ig.use5_W2>0], na.rm=T))/
                   sqrt(length(dat.5$poltol6_W2[dat.5$ig.use5_W2>0])))

dat.tol$lwr <- c(mean(dat.5$poltol6_W2[dat.5$fb.use5_W2>0], na.rm=T)-
                   (1.96*sd(dat.5$poltol6_W2[dat.5$fb.use5_W2>0], na.rm=T))/
                   sqrt(length(dat.5$poltol6_W2[dat.5$fb.use5_W2>0])), 
                 mean(dat.5$poltol6_W2[dat.5$yt.use5_W2>0], na.rm=T)-
                   (1.96*sd(dat.5$poltol6_W2[dat.5$yt.use5_W2>0], na.rm=T))/
                   sqrt(length(dat.5$poltol6_W2[dat.5$yt.use5_W2>0])), 
                 mean(dat.5$poltol6_W2[dat.5$tw.use5_W2>0], na.rm=T)-
                   (1.96*sd(dat.5$poltol6_W2[dat.5$tw.use5_W2>0], na.rm=T))/
                   sqrt(length(dat.5$poltol6_W2[dat.5$tw.use5_W2>0])), 
                 mean(dat.5$poltol6_W2[dat.5$ig.use5_W2>0], na.rm=T)-
                   (1.96*sd(dat.5$poltol6_W2[dat.5$ig.use5_W2>0], na.rm=T))/
                   sqrt(length(dat.5$poltol6_W2[dat.5$ig.use5_W2>0])))

colnames(dat.tol) <- c("SM", "Tolerance", "upr", "lwr")

ggplot(aes(x=SM), data=dat.tol)+
  geom_point(aes(y=Tolerance))+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.3)

t.test(dat.5$poltol6_W2[dat.5$fb.use5_W2>0], dat.5$poltol6_W2[dat.5$tw.use5_W2>0])
t.test(dat.5$poltol6_W2[dat.5$fb.use5_W2>0], dat.5$poltol6_W2[dat.5$yt.use5_W2>0])
t.test(dat.5$poltol6_W2[dat.5$fb.use5_W2>0], dat.5$poltol6_W2[dat.5$ig.use5_W2>0])

t.test(dat.5$poltol6_W2[dat.5$tw.use5_W2>0], dat.5$poltol6_W2[dat.5$yt.use5_W2>0])
t.test(dat.5$poltol6_W2[dat.5$tw.use5_W2>0], dat.5$poltol6_W2[dat.5$ig.use5_W2>0])

t.test(dat.5$poltol6_W2[dat.5$ig.use5_W2>0], dat.5$poltol6_W2[dat.5$yt.use5_W2>0])

# Look at SM Increasers
dat.w <- dat.4 %>%
  full_join(dat.5, by='CaseId')

dat.w <- dat.w %>%
  mutate(poltol3.diff=poltol3_W2-poltol3_W1, fb.diff=fb.use5_W2-fb.use5_W1, ig.diff=ig.use5_W2-ig.use5_W1,
         tw.diff=tw.use5_W2-tw.use5_W1, yt.diff=yt.use5_W2-yt.use5_W1)

dat.w$fb.inc<-ifelse(dat.w$fb.diff>0, dat.w$fb.diff, 0)
dat.w$fb.dec<-ifelse(dat.w$fb.diff<0, abs(dat.w$fb.diff), 0)
dat.w$ig.inc<-ifelse(dat.w$ig.diff>0, dat.w$ig.diff, 0)
dat.w$ig.dec<-ifelse(dat.w$ig.diff<0, abs(dat.w$ig.diff), 0)
dat.w$tw.inc<-ifelse(dat.w$tw.diff>0, dat.w$tw.diff, 0)
dat.w$tw.dec<-ifelse(dat.w$tw.diff<0, abs(dat.w$tw.diff), 0)
dat.w$yt.inc<-ifelse(dat.w$yt.diff>0, dat.w$yt.diff, 0)
dat.w$yt.dec<-ifelse(dat.w$yt.diff<0, abs(dat.w$yt.diff), 0)

lm.fb.inc<-lm(poltol3.diff~fb.inc+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2, data=dat.w)
lm.ig.inc<-lm(poltol3.diff~ig.inc+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2, data=dat.w)
lm.tw.inc<-lm(poltol3.diff~tw.inc+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2, data=dat.w)
lm.yt.inc<-lm(poltol3.diff~yt.inc+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2, data=dat.w)

lm.fb.dec<-lm(poltol3.diff~fb.dec+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2, data=dat.w)
lm.ig.dec<-lm(poltol3.diff~ig.dec+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2, data=dat.w)
lm.tw.dec<-lm(poltol3.diff~tw.dec+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2, data=dat.w)
lm.yt.dec<-lm(poltol3.diff~yt.dec+white_W2+male_W2+AGE7_W2+EDUC4_W2+INCOME_W2+party3_W2, data=dat.w)




