# this useful function to load several packages simultaneously by Steven Worthington can be found in https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage of the ipack formula to load and call the packages needed
packages <- c("tidyverse", "caret", "randomForest", "gsheet", "patchwork", "rpart", "rpart.plot", "car", "nortest", "rattle")
# As I am sharing the dataset in googlesheets, I included a package "gsheet" for reading it easily. 
# more information about this useful package could be found in this link: https://github.com/maxconway/gsheet
ipak(packages)
hf <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1V2KWrOSKKoeEID9-mvwIxcDp83RWlWUq9F5LFJpZqnk/edit?usp=sharing")
#first analysis of the dataset
str(hf)
# changing to factor bolean features
hf <- hf %>% mutate(anaemia = as.factor(if_else(anaemia == 0, "no", "yes")),
              diabetes = as.factor(if_else(diabetes == 0, "no", "yes")), 
              high_blood_pressure = as.factor(if_else(high_blood_pressure == 0, "no", "yes")),
              sex = as.factor(if_else(sex == 0, "female", "male")),
              smoking = as.factor(if_else(smoking == 0, "no", "yes")),
              DEATH_EVENT = as.factor(if_else(DEATH_EVENT == 0, "no", "yes")))
# renaming long name features
hf <- rename(hf, cpk = creatinine_phosphokinase,
               eject_f = ejection_fraction,
               hbp = high_blood_pressure,
               serum_c = serum_creatinine,
               serum_s = serum_sodium,
               death_event = DEATH_EVENT)

# exploring the data set for NAs
sum(is.na(hf))
# move forward to have a general view of the information
summary(hf)
# General distribution graphs
# numerice features
# Age
age <-  hf %>%
  ggplot(aes(age)) +
  geom_histogram(bins = 10, color = "black") +
  ggtitle("age")
# Creatinine Phophokinase
creat_ph <-  hf %>%
  ggplot(aes(cpk)) +
  geom_histogram(bins = 10, color = "black") +
  ggtitle("Creatinine Phosphokinase")
# ejection fraction
eject_f <- hf %>%
  ggplot(aes(eject_f)) +
  geom_histogram(bins = 20, color = "black") +
  ggtitle("Ejection Fraction")
# Platelets
platelets <- hf %>%
  ggplot(aes(platelets)) +
  geom_histogram(bins = 20, color = "black") +
  ggtitle("Platelets")
# Serum Creatinine
serum_c <- hf %>%
  ggplot(aes(serum_c)) +
  geom_histogram(bins = 20, color = "black") +
  ggtitle("Serum Creatinine")
# Serum sodium
serum_s <- hf %>%
  ggplot(aes(serum_s)) +
  geom_histogram(bins = 20, color = "black") +
  ggtitle("Serum sodium")
# Time until death event
time_de <- hf %>%
  ggplot(aes(time)) +
  geom_histogram(bins = 20, color = "black") +
  ggtitle("Time until Death Event")
# combine all graphs in one graph
((age + creat_ph + eject_f + platelets) / (serum_c + serum_s + time_de)) +
  plot_annotation(title = "Distribution of the numeric features")
# graphics to see correlation between features and DEATH EVENT
#  Age by death event
graph_age <- ggplot(hf, aes(x=death_event, y=age, group=death_event)) +
  geom_boxplot(aes(fill=death_event))
# creatinine phosphokinase by death event
graph_cpk <- ggplot(hf, aes(x=death_event, y=cpk, group=death_event)) +
  geom_boxplot(aes(fill=death_event))
# ejection fraction by death event
graph_ef <- ggplot(hf, aes(x=death_event, y=eject_f, group=death_event)) +
  geom_boxplot(aes(fill=death_event))
# Platelets by death event
graph_plat <- ggplot(hf, aes(x=death_event, y=platelets, group=death_event)) +
  geom_boxplot(aes(fill=death_event))
# serum creatinine by death event
graph_sc <- ggplot(hf, aes(x=death_event, y=serum_c, group=death_event)) +
  geom_boxplot(aes(fill=death_event))
# serum sodium by death event
graph_ss <- ggplot(hf, aes(x=death_event, y=serum_s, group=death_event)) +
  geom_boxplot(aes(fill=death_event))
# time by death event
graph_time <- ggplot(hf, aes(x=death_event, y=time, group=death_event)) +
  geom_boxplot(aes(fill=death_event))
# combine all graphs in one graph
((graph_age + graph_cpk) / (graph_ef + graph_plat) / (graph_sc + graph_ss) / (graph_time)) +
  plot_annotation(title = "Boxplots of Numeric Features with Death Event")
# normal distribution test
age_bp <- ggplot(hf, aes(x=age)) + geom_boxplot()
cpk_bp <- ggplot(hf, aes(x=cpk)) + geom_boxplot()
ef_bp <- ggplot(hf, aes(x=eject_f)) + geom_boxplot()
plat_bp <- ggplot(hf, aes(x=platelets)) + geom_boxplot()
serumc_bp <- ggplot(hf, aes(x=serum_c)) + geom_boxplot()
serums_bp <- ggplot(hf, aes(x=serum_s)) + geom_boxplot()
time_bp <- ggplot(hf, aes(x=time)) + geom_boxplot()
((age_bp + cpk_bp + ef_bp) / (plat_bp + serumc_bp + serums_bp) / (time_bp)) +
  plot_annotation("Boxplots of Numeric Features to Show Outlayers")
# normality test without outlayers
age_ks <- lillie.test(hf$age)$p.value
cpk_out <- hf %>% filter(min(boxplot.stats(hf$cpk)$stats) <= hf$cpk & hf$cpk <= max(boxplot.stats(hf$cpk)$stats))
cpk_ks <- lillie.test(cpk_out$cpk)$p.value
ef_out <- hf %>% filter(min(boxplot.stats(hf$eject_f)$stats) <= hf$eject_f & hf$eject_f <= max(boxplot.stats(hf$eject_f)$stats))
ef_ks <- lillie.test(ef_out$eject_f)$p.value
plat_out <- hf %>% filter(min(boxplot.stats(hf$platelets)$stats) <= hf$platelets & hf$platelets <= max(boxplot.stats(hf$platelets)$stats))
plat_ks <- lillie.test(plat_out$platelets)$p.value
serum_c_out <- hf %>% filter(min(boxplot.stats(hf$serum_c)$stats) <= hf$serum_c & hf$serum_c <= max(boxplot.stats(hf$serum_c)$stats))
serumc_ks <- lillie.test(serum_c_out$serum_c)$p.value
serum_s_out <- hf %>% filter(min(boxplot.stats(hf$serum_s)$stats) <= hf$serum_s & hf$serum_s <= max(boxplot.stats(hf$serum_s)$stats))
serums_ks <- lillie.test(serum_s_out$serum_c)$p.value
time_ks <- lillie.test(hf$time)$p.value

normality_test <- data.frame(Features = names(numeric_features[2:8]),
                             p.value = c(age_ks, cpk_ks, ef_ks, plat_ks, serumc_ks, serums_ks, time_ks))  
print(normality_test)                                               
# homogeneity of variances in features without aoutlayers
age_lt <- round(leveneTest(hf$age ~ hf$death_event)$"Pr(>F)"[1],4)
cpk_lt <- round(leveneTest(cpk_out$cpk ~ cpk_out$death_event)$"Pr(>F)"[1],4)     
ef_lt <- round(leveneTest(ef_out$eject_f ~ ef_out$death_event)$"Pr(>F)"[1],4)
plat_lt <- round(leveneTest(plat_out$platelets ~ plat_out$death_event)$"Pr(>F)"[1],4)
serumc_lt <- round(leveneTest(serum_c_out$serum_c ~ serum_c_out$death_event)$"Pr(>F)"[1],4)
serums_lt <- round(leveneTest(serum_s_out$serum_s ~ serum_s_out$death_event)$"Pr(>F)"[1],4)
time_lt <- round(leveneTest(hf$time ~ hf$death_event)$"Pr(>F)"[1],4)

variances_test <- data.frame(Features = names(numeric_features[2:8]),
                             p.value = c(age_lt, cpk_lt, ef_lt, plat_lt, serumc_lt, serums_lt, time_lt))
print(variances_test)
# T-Test for numeric features by death event
age_tt <- round(t.test(hf$age ~ hf$death_event, var.eq=F)$p.value,4)
cpk_tt <- round(t.test(cpk_out$cpk ~ cpk_out$death_event, var.eq=T)$p.value,4)    
ef_tt <- round(t.test(ef_out$eject_f ~ ef_out$death_event, var.eq=T)$p.value,4)
plat_tt <- round(t.test(plat_out$platelets ~ plat_out$death_event, var.eq=T)$p.value,4)
serumc_tt <- round(t.test(serum_c_out$serum_c ~ serum_c_out$death_event, var.eq=F)$p.value,4)
serums_tt <- round(t.test(serum_s_out$serum_s ~ serum_s_out$death_event, var.eq=T)$p.value,4)
time_tt <- round(t.test(hf$time ~ hf$death_event, var.eq=F)$p.value,4)

ttest <- data.frame(Features = names(numeric_features[2:8]),
                             p.value = c(age_tt, cpk_tt, ef_tt, plat_tt, serumc_tt, serums_tt, time_tt))
print(ttest)
# chi squared test for cathegorical features
with(hf, chisq.test(anaemia, death_event))
with(hf, chisq.test(diabetes, death_event))
with(hf, chisq.test(hbp, death_event))
with(hf, chisq.test(sex, death_event))
with(hf, chisq.test(smoking, death_event))

hf_cat <- hf %>% select("anaemia", "diabetes", "hbp", "sex", "smoking", "death_event")
catheg_chiq <-  data.frame(X_squared = sapply(hf_cat[1:5], function(x) chisq.test(x, hf_cat$death_event)$statistic),
                            p_value = sapply(hf_cat[1:5], function(x) chisq.test(x, hf_cat$death_event)$p.value))
print(catheg_chiq)
# creating training and test sets
set.seed(1, sample.kind = "rounding")
test_index <-  createDataPartition(y =hf$death_event, times = 1, p = 0.5, list = FALSE)
train_set <- hf[-test_index, ]
test_set <- hf[test_index, ]
# Desition tree
set.seed(0, sample.kind = "Rounding")
tree_hf <- rpart(death_event ~.,
              data = train_set,
              method = "class")
fancyRpartPlot(tree_hf)
# Using desition tree for prediction
prediction_tree <- predict(tree_hf, test_set, type = "class")
# Save the solution to a dataframe with two columns: death event and prediction
solution <- data.frame(death_event = test_set$death_event, prediction = prediction_tree) 
solution <- solution %>% mutate(comparison = if_else(death_event == prediction, 1, 0))
accuracy <- sum(solution$comparison)/nrow(solution)*100
accuracy
# setting a random seed
set.seed(456, sample.kind = "Rounding")
# Random Forest Model
rf_model <- randomForest(death_event ~., data = train_set)
rf_model
# Show model error
plot(rf_model, ylim=c(0,0.5))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# Get importance
importance    <- rf_model$importance
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()








