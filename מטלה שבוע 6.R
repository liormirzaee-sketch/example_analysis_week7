# R course for beginners 
# Week 6
# assignment by: Lior Mirzaee ID: 315665737

#### PART 1 ####
# loading and combining files
files_stroop <- dir("stroop_data")
df_stroop <- data.frame()
for(f in files_stroop) {
  temp <-read.csv(paste0("stroop_data/",f))
  df_stroop <- rbind(df_stroop, temp)
}

View(df_stroop)
length(unique(df_stroop$subject))
length(unique(df_stroop$condition))

sum(is.na(df_stroop$rt))
sum(is.na(df_stroop$rt))/ nrow(df_stroop) *100

library(ggplot2)

ggplot(df_stroop[df_stroop$condition == "congruent",], aes(x= rt)) + geom_histogram() + xlab("Reaction Time") +ylab("Congruent")+ ggtitle("RT: Congruent")

ggplot(df_stroop[df_stroop$condition == "incongruent",], aes(x= rt)) +geom_histogram()+ xlab("Reaction Time") + ylab("Incongruent") + ggtitle("RT: Incongruent")

#### PART 2 ####
#data cleaning and outliers detection

rt <- df_stroop$rt
outliers_RT <- function(rt) {
  x <- mean(rt)
  y <- sd(rt)
  z <- rt >x +2*y
  return(z)
}
outliers_RT(rt) 

sub <- unique(df_stroop$subject)
sub_outlier <- c()       
for (s in sub) {
  rt_subj <- df_stroop$rt[df_stroop$subject == s]
  out_s <- outliers_RT(rt_subj)
  sub_outlier <- c(sub_outlier,out_s)
}
df_stroop <- cbind(df_stroop, sub_outlier)
View(df_stroop)

#### PART 3 ####

for(s in sub) {
  n_outlier <- sum(df_stroop$sub_outlier[df_stroop$subject == s])
  print(n_outlier)}

df_stroop <- df_stroop |> filter(!sub_outlier)
sum(df_stroop$sub_outlier)
 
ggplot(df_stroop[df_stroop$condition == "congruent",], aes(x= rt)) + geom_histogram() + xlab("Reaction Time") +ylab("Congruent")+ ggtitle("RT: Congruent")

ggplot(df_stroop[df_stroop$condition == "incongruent",], aes(x= rt)) +geom_histogram()+ xlab("Reaction Time") + ylab("Incongruent") + ggtitle("RT: Incongruent")

