### DATA ANALYSIS FOR PAS EXPERIMENT

library(ggplot2)
library(tidyr)
library(rstatix)
detach(package:plyr)    
library(dplyr)
#install.packages("ggdist")
library(ggdist)
library (data.table)
library(gtools)
#install.packages("plotrix")                           # Install plotrix R package
library("plotrix") 
library(lme4)
library(lmerTest)
library(qpcR)

#sub_ID = "16_1"


### for just one participant
#dataPath = paste('C:/Users/marvi/Documents/MATLAB/Masterarbeit/data/SUB_', sub_ID, sep='')
#setwd(dataPath)

#experiment_df <- read.csv(file=paste(dataPath, '/rawData.csv', sep=''))
#timing_df <- read.csv(file=paste(dataPath, '/timingLog.csv', sep=''))

#experiment_df$ID <- sub_ID


#experimental data for several participants
dataPath = paste('C:/Users/marvi/Documents/MATLAB/Masterarbeit/data/')
setwd(dataPath)
filenames <- list.files(pattern = 'rawData.csv', recursive = TRUE)
filenames <- mixedsort(filenames) #sort dataframe by participant number in ascending order

experiment_df <- purrr::map_df(filenames, read.csv, .id = 'id')

#timing data for several participants
dataPath = paste('C:/Users/marvi/Documents/MATLAB/Masterarbeit/data/')
setwd(dataPath)
filenames <- list.files(pattern = 'timingLog.csv', recursive = TRUE)
filenames <- mixedsort(filenames) #sort dataframe by participant number in ascending order

timing_df <- purrr::map_df(filenames, read.csv, .id = 'id')


#exclude test trials from analysis
experiment_df <- subset(experiment_df, trial != 1:20)
timing_df <- subset(timing_df, Trial != 1 & Trial!= 2& Trial!= 3& Trial!= 4& Trial!= 5& Trial!= 6
                     & Trial!= 7& Trial!= 8& Trial!= 9& Trial!= 10& Trial!= 11& Trial!= 12& Trial!= 13
                     & Trial!= 14& Trial!= 15& Trial!= 16& Trial!= 17& Trial!= 18& Trial!= 19& Trial!= 20)


#one row per trial
#dur_per_trial =  data.frame(trial_dur=double())
dur_per_trial = timing_df[seq(1, nrow(timing_df), 4), ]

unique_dur <- dur_per_trial$manipDuration
unique_dur <- as.data.frame(unique_dur)
unique_dur <- unique_dur*1000

#add column so that df shows whether trial was correct or not
for(i in 1:nrow(experiment_df)){
  if(experiment_df$corrStim[i] == experiment_df$respStim[i]){
    experiment_df$is_correct[i] <- 1}
  else{experiment_df$is_correct[i] <-0}
  }

#combine duration and iscorrect dfs    
correct_per_dur <- cbind(unique_dur, experiment_df$is_correct, experiment_df$condition)
colnames(correct_per_dur) <- c("duration", "iscorrect", "condition")



#combine subject and iscorrect and duration
sub_dur_cor <- cbind(experiment_df$SUB_ID, experiment_df$is_correct, experiment_df$condition, unique_dur, experiment_df$PASresp)
colnames(sub_dur_cor) <- c("ID", "iscorrect", "condition", "duration", "PASresp")
#sub_dur_cor$ID = as.numeric(sub_dur_cor$ID)
acc_per_sub = as.data.frame(sub_dur_cor)

#combine duration and PAS
PAS_per_dur <- cbind(unique_dur, experiment_df$PASresp, experiment_df$condition)
colnames(PAS_per_dur) <- c("duration", "PASrating", "condition")


#accuracy per duration
acc = correct_per_dur %>% dplyr::group_by(condition, duration) %>%
  dplyr::summarize(Acc = sum(iscorrect)/length(iscorrect), Accsd = sd(iscorrect), Acc_SE = std.error(iscorrect))
acc$condition=as.factor(acc$condition)
#acc$ID = sub_ID

#accuracy per subject 
acc_sub_dur = sub_dur_cor %>% dplyr::group_by(ID, duration, condition) %>%
  dplyr::summarize(Acc = sum(iscorrect)/length(iscorrect), Accsd = sd(iscorrect), Acc_SE =std.error(iscorrect), PAS = mean(PASresp))
acc_sub_dur$condition = as.factor(acc_sub_dur$condition)
#acc_sub_dur$duration = as.factor(acc_sub_dur$duration)

#PAS per duration
PAS = PAS_per_dur %>% dplyr::group_by(duration, condition) %>%
  dplyr::summarize(PASmean = sum(PASrating)/length(PASrating), PASsd = sd(PASrating), PAS_SE = std.error(PASrating))
PAS$condition=as.factor(PAS$condition)

#PAS$ID = sub_ID

#plot accuracy per subject per duration
nrsub <- max(acc_sub_dur$ID)
condition.labs <- c("narrow", "wide")
names(condition.labs) <- c("0", "1")
d <- ggplot(acc_sub_dur, aes(x = ID, y =Acc, fill=duration))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = Acc-1.96*Acc_SE, ymax = Acc+1.96*Acc_SE),position = "dodge")+
  facet_grid(rows=vars(condition), labeller = labeller(condition = condition.labs))+
  geom_hline(yintercept=0.25, linetype="dashed", color = "red")+
  scale_x_continuous(breaks = seq(from = 1, to = nrsub, by = 1))+
  theme_light()+
  labs(title="Accuracy per Duration for each Subject",
       x ="Subject", y = "Accuracy", fill = "Duration (in Milliseconds)",
       caption = "(error bars based on 95% confidence interval)")
d

#plot accuracy based on duration
#acc$duration <- as.factor(acc$duration)
acc$duration=as.numeric(acc$duration)
acc$Acc=as.numeric(acc$Acc)
g <- ggplot(acc, aes(x=duration, y=Acc, ymin = Acc-1.96*Acc_SE, ymax = Acc+1.96*Acc_SE, group = condition, color = condition))+
  geom_point(size=1.5)+
  geom_line(size=1)+
  expand_limits(x = 0, y = 0)+
  geom_errorbar(position =position_dodge(0.25), width = 3,size=1) +
  scale_x_continuous(breaks = seq(0, 144, by=16)) +
  scale_y_continuous(breaks = seq(0,1, by = 0.25),expand = c(0, 0), limits = c(0,1.1)) +
  geom_hline(yintercept=0.25, linetype="dashed", color = "red")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"),
                     breaks=c(0,1),
                     labels=c("Narrow", "Wide")) +
  
  theme_light()+
  labs(x ="Stimulus Duration [ms]", y = "Accuracy", fill = "Condition", color="Condition")
  
g


#plot PAS
#PAS$duration <- as.factor(PAS$duration)
b <- ggplot(PAS, aes(x=duration, y=PASmean, group = condition, color = condition))+
  geom_point(size = 1.5)+
  geom_line(size=1)+
  expand_limits(x = 0, y = 0)+
  scale_y_continuous(breaks = seq(from = 1, to = 4, by = 1), limits = c(1, 4.1)) +
  scale_x_continuous(breaks = seq(0, 144, by=16)) +
  geom_errorbar( aes(ymin = PASmean-1.96*PAS_SE, ymax = PASmean+1.96*PAS_SE), width = 3, size =1) +
  #scale_x_continuous(breaks = seq(from = 0, to = 0.32, by = 0.016)) +
  #scale_y_continuous(name="PAS mean", limits=c(1, 4))
  theme_light()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"),
                     breaks=c(0,1),
                     labels=c("Narrow", "Wide")) + 
  labs(x ="Stimulus Duration [ms]", y = "PAS mean rating", fill = "Condition", color = "Condition")
b

#plot histograms for PAS rating per duration
#PAS_per_dur$PASrating <- as.factor(PAS_per_dur$PASrating)

PAS_per_dur$condition <- as.factor(PAS_per_dur$condition)
h <- ggplot(PAS_per_dur, aes(x=PASrating, color = condition, fill = condition))+
  geom_histogram(binwidth = 0.5,position = "dodge")+
  facet_wrap(~duration)+
  theme_light()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"),
                     breaks=c(0,1),
                     labels=c("Narrow", "Wide"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800"),
                     breaks=c(0,1),
                     labels=c("Narrow", "Wide"))+
  labs(x = "PAS rating", fill = "Condition", color = "Condition")

h

##analysis of counts
hist_48 <- subset(acc_per_sub, duration == 48)
table(hist_48$PASresp)

hist_144 <- subset(acc_per_sub, duration == 144)
table(hist_144$PASresp)


ordered_acc = newset %>% dplyr::group_by(ID, condition) %>% 
  dplyr::summarize(Acc = sum(iscorrect)/length(iscorrect), Accsd = sd(iscorrect), Acc_SE =std.error(iscorrect)) 
ordered_acc = ordered_acc[order(ordered_acc$Acc),]
##plot it
c <- ggplot(ordered_acc, aes(x = ID, y = Acc, fill=condition))+
  geom_col(position = "dodge")
c



#analyze timing
timing_df$difference = timing_df$TimeExpected - timing_df$TimeRealFlip
timing = timing_df %>% dplyr::group_by(manipDuration) %>%
  dplyr::summarize(mean = mean(difference, na.rm = TRUE))
max_diff = max(timing_df$difference, na.rm = TRUE)



write.csv(PAS_per_dur,"C:/Users/marvi/Documents/MATLAB/Masterarbeit/data/combined_df.csv", row.names = FALSE)
write.csv(acc_sub_dur,"C:/Users/marvi/Documents/MATLAB/Masterarbeit/data/acc_sub_dur.csv", row.names = FALSE)

  

###statistical analysis
## Logistic regression
#look at data
newset <- subset(acc_per_sub, duration == 80)
newsetwide <- subset(newset, condition == 1)
newsetnarrow <- subset(newset, condition == 0)

t.test(newsetnarrow$PASresp, newsetwide$PASresp, var.equal = TRUE)
t.test(newsetnarrow$iscorrect, newsetwide$iscorrect, var.equal = TRUE)

table(experiment_df$PASresp, experiment_df$is_correct)
table(sub_dur_cor$duration, sub_dur_cor$iscorrect, sub_dur_cor$condition)

### mixed effect model
##Andy Field
#check whether multilevel modelling is necessary by comparing two models - 
#with and without random effect
library(nlme)

interceptOnly <- gls(PASresp ~ 1, data = acc_per_sub, method = "ML")
summary(interceptOnly)
anova(interceptOnly)
randomInterceptOnly <- lme(PASresp ~ 1, data = acc_per_sub, random = ~1|ID, method = "ML")
summary(randomInterceptOnly)
anova(randomInterceptOnly)
anova(interceptOnly, randomInterceptOnly)
randomInterceptCondition <- lme(PASresp ~ condition, data = acc_per_sub, random = ~1|ID, method = "ML")
summary(randomInterceptCondition)
anova(randomInterceptCondition)
anova(interceptOnly, randomInterceptOnly, randomInterceptCondition)

### --> significant p value, so modelling the intercepts is useful!!

randomInterceptConditionDur <- lme(PASresp ~ condition + duration, data = acc_per_sub, random = ~1|ID, method = "ML")
summary(randomInterceptConditionDur)
anova(randomInterceptConditionDur)
anova(interceptOnly, randomInterceptOnly, randomInterceptCondition, randomInterceptConditionDur)

finalModel <- lme(PASresp ~ condition + duration, data = acc_per_sub, random = ~duration|ID, method = "ML")
summary(finalModel)
intervals(finalModel)
anova(finalModel)

# +interaction term
finalModel_2 <- lme(PASresp ~ condition + duration + duration:condition, data = acc_per_sub, random = ~duration|ID, method = "ML")
summary(finalModel_2)
anova(finalModel_2)
anova(interceptOnly,randomInterceptOnly,randomInterceptCondition, randomInterceptConditionDur, finalModel, finalModel_2)
intervals(finalModel_2, 0.95)

#final model of only 80ms
finalModel_80 <- lme(PASresp ~ condition, data = newset, random = ~1|ID, method = "ML")
summary(finalModel_80)
anova(finalModel_80)
#finalModel_80 <- gls(PASresp ~ condition, data = newset, method = "ML")
intervals(finalModel_80, 0.95)

summary(finalModel_80)

#level 2 analysis of means (participant means?)
acc_sub_dur2 <- subset(acc_sub_dur, duration == 16 | duration == 80)
lvl2analysis <- lme(PAS ~ Acc , data = acc_sub_dur2, random = ~1|ID, method = "ML")
summary(lvl2analysis)
anova(lvl2analysis)
intervals(lvl2analysis, 0.95)

#guessing criterion
guess_cri <- subset(acc_sub_dur, duration == 80)


### interpretation of the model: as condition increases (from narrow to wide) PAS ratings increase
### as duration increases, PAS rating significantly increases as well

## breaking down interaction effects
fua_wide <- subset(acc_per_sub, condition == 1)
fua_narrow <- subset(acc_per_sub, condition == 0)

finalModel_narrow <- lme(PASresp ~ duration , data = fua_narrow, random = ~duration|ID, method = "ML")
summary(finalModel_narrow)
anova(finalModel_narrow)
intervals(finalModel_narrow, 0.95)

finalModel_wide <- lme(PASresp ~ duration , data = fua_wide, random = ~duration|ID, method = "ML")
summary(finalModel_wide)
anova(finalModel_wide)
intervals(finalModel_wide, 0.95)

anova(finalModel_narrow)
anova(finalModel_wide)
anova(finalModel_wide, finalModel_narrow)

##accuracy
accCon <- glm(iscorrect ~ condition, data = newset1, family = "binomial")
accCon1 <- glmer(iscorrect ~ condition + duration + duration:condition+(duration|ID), data = acc_per_sub, family = "binomial")
summary(accCon1)
anova(accCon1)

anova(interceptOnly, randomInterceptOnly, finalModel)



log.model <- lmer(PASresp ~ 1+ condition+ duration + (1+duration|ID), data = acc_per_sub)
log.model <- glm(iscorrect ~ 1+ condition+ duration + PASresp + (1|ID), data = acc_per_sub, family ='binomial')

summary(log.model)
summary(log.model)$coefficients
exp(confint.default(log.model)[2:3,])
predict(log.model, newdata = list(PASresp = c(1,2,3,4)), type = "response")




### only look at 80ms condition
table(newset$PASresp, newset$condition)
log.model <- glm(PASresp ~ iscorrect + duration + condition, data = newset)
summary(log.model)

log.model_acc80 <- glmer(iscorrect ~ condition + (1|ID), data = newset, family = "binomial")
summary(log.model_acc80)

log.model_acc80_ <- glm(iscorrect ~ condition, data = newset, family = "binomial")
summary(log.model_acc80_)

### only look at 16ms and 80ms condition to check whether accuracy is same
newset1 <- subset(acc_per_sub, duration == 80 | duration == 16)
table(newset1$iscorrect, newset1$condition)

log.model1 <- glm(iscorrect ~ condition, data = newset1, family = "binomial")
summary(log.model1)

log.model <- glmer(iscorrect ~ condition + (1|ID), data = newset1, family = "binomial")
summary(log.model)

acc_model <- lme(Acc ~ condition, data = acc_sub_dur2,random = ~1|ID)



intervals(log.model, 0.95)


##trying mixed effects logistic regression
require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)


### grid search for exploratory data analysis:
### remove participants with too high accuracy in wide condition and 
### participants with too low accuracy in narrow condition by 
### 1. seperating data in two sets (one for each condition) - only including 16 and 80 ms 
### 2. calculate the mean accuracy of each group
### 3. define thresholds
### 3. calculate difference between means
### 5. take the two thresholds that minimize this difference

###Exploratory analysis 
newsetwide_expl_an <- subset(newsetwide, ID != 14 & ID != 16)
newsetnarrow_expl_an <- subset(newsetnarrow, ID != 13 & ID!= 21)
expl_set <- subset(newset, ID != 14 & ID != 16 & ID != 13 & ID!= 21)

explModel_80 <- lme(PASresp ~ condition, data = expl_set, random = ~1|ID, method = "ML")
summary(explModel_80)
anova(explModel_80)

explModel_80_acc <- lme(iscorrect ~ condition, data = expl_set, random = ~1|ID, method = "ML")
summary(explModel_80_acc)

log.model3 <- glm(iscorrect ~ condition, data = expl_set, family = "binomial")
summary(log.model3)

log.model4 <- glmer(iscorrect ~ condition + (1|ID), data = expl_set, family = "binomial")
summary(log.model4)

t.test(newsetnarrow_expl_an$PASresp, newsetwide_expl_an$PASresp, var.equal = TRUE)
t.test(newsetnarrow_expl_an$iscorrect, newsetwide_expl_an$iscorrect, var.equal = TRUE)

PAS_expl_an_narrow = newsetnarrow_expl_an %>% 
  dplyr::summarize(PASmean = sum(PASresp)/length(PASresp), PASsd = sd(PASresp), PAS_SE = std.error(PASresp))

PAS_expl_an_wide = newsetwide_expl_an %>% 
  dplyr::summarize(PASmean = sum(PASresp)/length(PASresp), PASsd = sd(PASresp), PAS_SE = std.error(PASresp))


### descriptive statistics
##accuracy at 16 ms different from 0?
descr_stat <- subset(acc_per_sub, duration == 16)
acc_16_all <- mean(descr_stat$iscorrect)
descr_stat_1 <- subset(descr_stat, condition == 1)
acc_16_1 <- mean(descr_stat_1$iscorrect)
#how many correct?
corr1 <- sum(descr_stat_1$iscorrect == 1)
#how many wrong?
fals1 <- sum(descr_stat_1$iscorrect == 0)
#chi square test against chance
corr_fals_1 <- c(corr1, fals1)
res1 <- chisq.test(corr_fals_1, p = c(1/4, 3/4))
res1


descr_stat_0 <- subset(descr_stat, condition == 0)
acc_16_0 <- mean(descr_stat_0$iscorrect)

#how many correct?
corr0 <- sum(descr_stat_0$iscorrect == 1)
#how many wrong?
fals0 <- sum(descr_stat_0$iscorrect == 0)
corr_fals_0 <- c(corr0, fals0)

res0 <- chisq.test(corr_fals_0, p = c(1/4, 3/4))
res0

##48ms
descr_stat48 <- subset(acc_per_sub, duration == 48)
acc_48 <- mean(descr_stat48$iscorrect)

##80ms
descr_stat80 <- subset(acc_per_sub, duration == 80)
acc_80_all <- mean(descr_stat80$iscorrect)
descr_stat80_0 <- subset(descr_stat80, condition == 0)
acc_80_0 <- mean(descr_stat80_0$iscorrect)
descr_stat80_1 <- subset(descr_stat80, condition == 1)
acc_80_1 <- mean(descr_stat80_1$iscorrect)

##144ms
descr_stat144 <- subset(acc_per_sub, duration == 144)
acc_144 <- mean(descr_stat144$iscorrect)

##median
med_80 <- subset(acc_sub_dur, duration == 80)
med_80_0 <- subset(med_80, condition == 0)
median(med_80_0$Acc)

med_80_1 <- subset(med_80, condition == 1)
median(med_80_1$Acc)

###PAS
med_16 <- subset(PAS_per_dur, duration == 16)
med_16_1 <- subset(med_16, condition == 1)
median(med_16_1$PASrating)
med_16_0 <- subset(med_16, condition == 0)
median(med_16_0$PASrating)

med_80 <- subset(PAS_per_dur, duration == 80)
sd(med_80$PASrating)
##guessing criterion
guess <- subset(acc_per_sub, PASresp == 1)
mean(guess$iscorrect)
corr_gall <- sum(guess$iscorrect == 1)
fals_gall <- sum(guess$iscorrect == 0)
corr_fals_gall <- c(corr_gall, fals_gall)
res_all <- chisq.test(corr_fals_gall, p = c(1/4, 3/4))
res_all
#wide
guess1 <- subset(guess, condition == 1)
mean(guess1$iscorrect)
corr_g1 <- sum(guess1$iscorrect == 1)
fals_g1 <- sum(guess1$iscorrect == 0)
corr_fals_g1 <- c(corr_g1, fals_g1)
res_g1 <- chisq.test(corr_fals_g1, p = c(1/4, 3/4))
res_g1

#narrow
guess0 <- subset(guess, condition == 0)
mean(guess0$iscorrect)
corr_g0 <- sum(guess0$iscorrect == 1)
fals_g0 <- sum(guess0$iscorrect == 0)
corr_fals_g0 <- c(corr_g0, fals_g0)
res_g0 <- chisq.test(corr_fals_g0, p = c(1/4, 3/4))
res_g0

#comparing accuracy distributions across conditions
res_g_comp <- chisq.test()

#how many correct?
corr0 <- sum(descr_stat_0$iscorrect == 1)
#how many wrong?
fals0 <- sum(descr_stat_0$iscorrect == 0)
corr_fals_0 <- c(corr0, fals0)

res0 <- chisq.test(corr_fals_0, p = c(1/4, 3/4))
res0

##zero correlation criterion
# subset data into the different stimulus durations and correlate the corresponding
# accuracies with each other in order to see whether they are different

##anova
anova(descr_stat$iscorrect, descr_stat48$iscorrect, descr_stat80$iscorrect, descr_stat144$iscorrect)




