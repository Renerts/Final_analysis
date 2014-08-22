# This script processes the data from the 'Sleep study'. For statistical analyses see script 'Main'

## Processing of the memory performance data
main.memory <- read.csv('data_for_final/Reward_recognition_combined.csv')

### Age

Code <- as.character(main.memory$Name)
Code <- strsplit(Code, split='_')
Year <- matrix(0,length(Code),1)
for (i in 1:length(Code)) {
        Year[i] <- as.numeric(Code[[i]][[2]])
}
Age <- 113-Year
main.memory <- cbind(main.memory[ ,1:2],Age,main.memory[ ,3:ncol(main.memory)])

remove(Age, Year, Code)

### sum & hit rates
main.memory$low_stimuli <- (main.memory$R1_low + main.memory$R2_low + main.memory$F3_low + main.memory$F4_low)
main.memory$high_stimuli <- (main.memory$R1_high + main.memory$R2_high + main.memory$F3_high + main.memory$F4_high)
main.memory$dist_stimuli <- (main.memory$Dist_1 + main.memory$Dist_2 + main.memory$Dist_3 + main.memory$Dist_4)

main.memory$hit_sure_low <- main.memory$R1_low/main.memory$low_stimuli
main.memory$hit_unsure_low <- (main.memory$R1_low+main.memory$R2_low)/main.memory$low_stimuli

main.memory$hit_sure_high <- main.memory$R1_high/main.memory$high_stimuli
main.memory$hit_unsure_high <- (main.memory$R1_high+main.memory$R2_high)/main.memory$high_stimuli

main.memory$fa_sure <- main.memory$Dist_1/main.memory$dist_stimuli
main.memory$fa_unsure <- (main.memory$Dist_1+main.memory$Dist_2)/main.memory$dist_stimuli

### removing ones and zeros (Harold Stanislaw, 1999)

main.memory$hit_unsure_high[31] <- ((main.memory$high_stimuli[31]-0.5)/main.memory$high_stimuli[31]) # exception based

### calculating d primes (Harold Stanislaw, 1999)

main.memory$dprime_sure_low <- qnorm(main.memory$hit_sure_low) - qnorm(main.memory$fa_sure)
main.memory$dprime_sure_high <- qnorm(main.memory$hit_sure_high) - qnorm(main.memory$fa_sure)

main.memory$dprime_unsure_low <- qnorm(main.memory$hit_unsure_low) - qnorm(main.memory$fa_unsure)
main.memory$dprime_unsure_high <- qnorm(main.memory$hit_unsure_high) - qnorm(main.memory$fa_unsure)

### creating subsets in long format

sub.performance <- main.memory[c('Scan', 'Age', 'Gender', 'Group', 'hit_sure_low', 'hit_sure_high', 'hit_unsure_low', 'hit_unsure_high', 'fa_sure', 'fa_unsure', 'dprime_sure_low', 'dprime_sure_high', 'dprime_unsure_low', 'dprime_unsure_high')] 

sub.performance.long <- reshape(sub.performance, direction='long', idvar='Scan', varying=list(c('hit_sure_low', 'hit_sure_high'),c('hit_unsure_low', 'hit_unsure_high'),c('dprime_sure_low', 'dprime_sure_high'),c('dprime_unsure_low', 'dprime_unsure_high')), v.names=c('hit_sure','hit_unsure','dprime_sure','dprime_unsure'),timevar='Condition', times=c('low','high'))
sub.performance.long$Condition <- as.factor(sub.performance.long$Condition)

## Processing of the accuracy data

main.accuracy.A <- read.csv('data_for_final/accuracy_A_study.csv')

### single correct cue responses

main.accuracy.A$sc_cue_stimuli <- (main.accuracy.A$sc_cue_15+main.accuracy.A$sc_cue_50l+main.accuracy.A$sc_cue_50h+main.accuracy.A$sc_cue_100)       
main.accuracy.A$cue_stimuli <- (main.accuracy.A$sc_cue_stimuli+(main.accuracy.A$sw_cue_15+main.accuracy.A$sw_cue_50l+main.accuracy.A$sw_cue_50h+main.accuracy.A$sw_cue_100))

### single correct word stimuli responses

main.accuracy.A$sc_
## Processing of the reaction times

### A study
main.rt.A <- read.csv('data_for_final/Reaction_times_A_study.csv')

### B study
main.rt.B <- read.csv('data_for_final/Reaction_times_B_study.csv')


