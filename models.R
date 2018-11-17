#okay, so. this is the file to work with every day 
library(pacman)
pacman::p_load(beepr,metafor,lme4,brms,tidyverse,readbitmap,eyetrackingR,readr,groupdata2,ggplot2,tidyverse,lmerTest,lme4,MuMIn,data.table,jpeg,grid,tidyr,rethinking,gridExtra,readxl,effsize,metafor,pastecs)
write.csv(df, file = 'FixationsV888.csv')
setwd("~/Denmark/Study/Semester V/big T/")
df = read.csv('final.csv')
df[,1] = NULL

#i.	Pupil dilation - Pupils are expected to be more constricted compared to subject's first round (focus )
df$Agreement = as.factor(df$Agreement)
df$SecondRating = as.factor(df$SecondRating)
df$Session = as.factor(df$Session)
df$ID = as.factor(df$ID)
df$Trial = as.factor(df$Trial)
df$Picture = as.factor(df$Picture)
df$Duration = as.numeric(df$Duration)
df$FixationN = as.numeric(df$FixationN)
df$Group = as.factor(df$Group)
df$CurrentRating = as.factor(df$CurrentRating)
df$CurrentGroupRating = as.factor(df$CurrentGroupRating)
df$PreviousGroupRating = as.factor(df$PreviousGroupRating)
df$PreviousRating = as.factor(df$PreviousRating)
df$Agreement_BI = as.factor(df$Agreement_BI)

H1 = lmer(PupilSize.s ~ relevel(Event, ref = "NormRating") * relevel(Agreement_BI, ref = "1") + (1|ID) + (1|Picture),  df)
summary(H1)

H1.2 = lmer(PupilSize.s ~ Event * Agreement_BI * Group + (1|ID) + (1|Picture),  df)
summary(H1.2)
r.squaredGLMM(H1.2)

H1.3 = lmer(max_FixationN ~ Agreement_BI * Event + (1|ID) + (1|Picture), df)
summary(H1.3)
r.squaredGLMM(H1)
H1.4 = lmer(Duration ~ Agreement_BI * Event + (1|ID) + (1|Picture), df)
summary(H1.4)


#let's try bayesian


#
M1 <- brm(PupilSize.s ~ 1 + Event * Agreement_BI + (1|ID), 
          data = df,iter = 1000, cores = 2, chain = 2)
#1694 seconds
summary(M1)



#ii.	Fewer but longer fixations and fewer but longer saccades [HIGHER DWELL TIMES]  compared to subject's first round 

#N fixations R2 ~ N fixations R1 * T fixations R1 * T fixations R2 + Correctness + (1|Subject)


##Hypothesis II



#2.)	Reaction to prediction error signal associated with conflict trials (change in the above listed  behavioural measurements) predicts adjustment in conformity 
df$mean_PupilSize.s = scale(df$mean_PupilSize)
H2 = glmer(Agreement_BI ~ mean_PupilSize.s + max_FixationN + max_DurationN + mean_Duration + mean_Duration_sacc + (1|ID) + (1|Picture), family = binomial, df)
summary(H2)  
r.squaredGLMM(H2)
  


Hypothe

