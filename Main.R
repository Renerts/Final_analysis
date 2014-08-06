## This is the script for reproducing the behavioral analysis for 'Sleep study'. The necessary data can be downloaded at 'X'. Data has already been preprocessed from the raw Presentation(Neurobs) logfiles using Matlab. 

## This script has several parts. The 'Main' script is for the statistical tests and analyses. It sources the 'processing' script, which creates all the necessary variables and data frames. 'Graphs' is used to create visual materials from the data. 

source('processing.R')

## Memory performance

### T-tests

ttest.hit.sure <- t.test(hit_sure ~ Condition, data=sub.performance.long, paired=T)
ttest.hit.sure

ttest.hit.unsure <- t.test(hit_unsure ~ Condition, data=sub.performance.long, paired=T)
ttest.hit.unsure

ttest.dprime.sure <- t.test(dprime_sure ~ Condition, data=sub.performance.long, paired=T)
ttest.dprime.sure # significant

ttest.dprime.sure.wake <- t.test(dprime_sure ~ Condition, data=sub.performance.long, paired=T)
ttest.dprime.sure.wake # significant

ttest.dprime.sure.sleep <- t.test(dprime_sure ~ Condition, data=sub.performance.long[sub.performance.long$Group=='s', ], paired=T)
ttest.dprime.sure.sleep

ttest.dprime.unsure.wake <- t.test(dprime_unsure ~ Condition, data=sub.performance.long[sub.performance.long$Group=='w', ], paired=T)
ttest.dprime.unsure.wake

ttest.dprime.unsure.sleep <- t.test(dprime_unsure ~ Condition, data=sub.performance.long[sub.performance.long$Group=='s', ], paired=T)
ttest.dprime.unsure.sleep

ttest.dprime.sure.gr.high <- t.test(main.memory[main.memory$Group=='s', ]$dprime_sure_high, main.memory[main.memory$Group=='w', ]$dprime_sure_high,)
ttest.dprime.sure.gr.high

ttest.dprime.sure.gr.low <- t.test(main.memory[main.memory$Group=='s', ]$dprime_sure_low, main.memory[main.memory$Group=='w', ]$dprime_sure_low,)
ttest.dprime.sure.gr.low

ttest.FA.sure.group <- t.test(fa_sure ~ Group, data=main.memory)
ttest.FA.sure.group

ttest.FA.unsure.group <- t.test(fa_unsure ~ Group, data=main.memory)
ttest.FA.unsure.group

### ANOVA

aov.dprime.sure.gr.con <- (aov(dprime_sure~Group*Condition + Error(Scan/Condition), data=sub.performance.long))
summary(aov.dprime.sure.gr.con) # significant

aov.dprime.sure.gr.con.gen <- (aov(dprime_sure~Group*Condition*Gender + Error(Scan/Condition), data=sub.performance.long))
summary(aov.dprime.sure.gr.con.gen)

aov.dprime.unsure.gr.con <- (aov(dprime_unsure~Group*Condition + Error(Scan/Condition), data=sub.performance.long))
summary(aov.dprime.unsure.gr.con)

aov.dprime.unsure.gr.con.gen <- (aov(dprime_unsure~Group*Condition*Gender + Error(Scan/Condition), data=sub.performance.long))
summary(aov.dprime.unsure.gr.con.gen)

aov.hit.sure.gr.con <- (aov(hit_sure~Group*Condition + Error(Scan/Condition), data=sub.performance.long))
summary(aov.hit.sure.gr.con)

library(ez)
library(stargazer)
ez.dprime.sure <- ezANOVA(data=sub.performance.long,dv=dprime_sure, wid=Scan, within=Condition, between=Group, type=2)
ez.dprime.sure
stargazer(ez.dprime.sure, type='text', summary=F)
# capture.output(stargazer(ez.dprime.sure, type='text', summary=F), file=file.path("dprime.sure.doc"))

## Reaction times
