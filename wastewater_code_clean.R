setwd("/Users/tuo/Dropbox/biostats/Waste water project")
options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

#options(java.parameters = "-Xmx8000m")
library(tidyverse)
library(readxl)
library(plotly)
library(xlsx)
library(rpart)
library(randomForest)
library(reprtree)
library(pROC)
library(rpart.plot)
library(dplyr)
library(WeightedROC)
library(caret)
library(hrbrthemes)
library(viridis)
library(data.table)
library(cowplot)
library(glmnet)
library(e1071)
library(nnet)

source("confusion_matrix.R")

# Manhole lookup data
manhole_notif = read_csv("waste water Junting/data/manhole_notification.csv") %>%
  filter(!duplicated(building_name)) %>%
  janitor::clean_names()

## testing on the new dataset
colnames <- read_excel("validation data/Wastewater sample pickup-111.xlsx", sheet = "Results_for_test", cell_rows(2:3))
colnames <- as.vector(t(colnames))

ww_char_new <- read_excel("validation data/Wastewater sample pickup-111.xlsx", sheet = "Results_for_test",
                          range = cell_limits(c(3,1), c(147,359)))

oldnames = colnames(ww_char_new)
newnames = as.character(format(colnames))

ww_char_new1 <- ww_char_new %>% 
  rename_at(vars(oldnames), ~ newnames) %>%
  dplyr::select(4:ncol(ww_char_new)) %>%
  mutate_if(is.character, as.double) %>%
  mutate(manhole_id = ww_char_new$ManholeID) %>% 
  filter(!manhole_id %in% c("C6M094", "C6M095", "C6M097", "C6M098")) %>%
  drop_na(manhole_id) %>%
  pivot_longer(cols = 1:(ncol(ww_char_new)-3), names_to = "date", values_to = "WW_results") %>% 
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d",origin = "1970-01-01 00:00:00",tz = "UTC")) %>% 
  janitor::clean_names() %>%
  mutate(ww_results = ifelse(ww_results %in% c(-1), 50, ww_results)) %>%
  mutate(ww_results = ifelse(ww_results %in% c(-2), NA, ww_results)) %>%
  mutate(ww_results = abs(ww_results)) 

dat_list = read_csv("validation data/Building Result Data Historical 2021-11-16.csv") %>%
  dplyr::rename(date = RESULT_DATE) %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%y",origin = "1970-01-01 00:00:00",tz = "UTC")) %>%
  janitor::clean_names()

isolation = read_csv("validation data/ResidentsIsolationQuarantineOnCampus 2020nov23 through 2021Nov16.csv") %>% 
  dplyr::rename(start = `Date Entered`) %>%
  dplyr::rename(end = `Student Released Date`) %>%
  dplyr::rename(building_name = `Building`) %>%
  dplyr::rename(iso = `Isolation?`) %>%
  filter(iso == 1) %>%
  mutate(start = as.POSIXct(start, format = "%m/%d/%y",origin = "1970-01-01 00:00:00",tz = "UTC")) %>%
  mutate(end = as.POSIXct(end, format = "%m/%d/%y",origin = "1970-01-01 00:00:00",tz = "UTC")) %>%
  janitor::clean_names() %>%
  left_join(manhole_notif, by = "building_name") %>%
  filter(!is.na(manhole_id))

data_full = dat_list %>% 
  left_join( manhole_notif,by = "building_name") %>%
  filter(!is.na(manhole_id)) %>% 
  dplyr::select(date, building_name,total_results, total_pos_students, manhole_id) %>% 
  mutate(date = as.POSIXct(date,origin = "1970-01-01 00:00:00")) %>% 
  group_by(manhole_id,date) %>% 
  dplyr::summarise(test_num = sum(total_results),
                   #ww_results = ww_results,
                   pos_result = sum(total_pos_students)) %>%
  right_join(ww_char_new1, by = c("manhole_id", "date")) %>% 
  mutate(pos_result = ifelse(is.na(pos_result), 0, pos_result)) %>%
  mutate(test_num = ifelse(is.na(test_num), 0, test_num)) %>%
  group_by(manhole_id) %>%
  mutate(test_result = ifelse(lag(pos_result, 1) == 0 & pos_result == 0 &
                                dplyr::lead(pos_result, 1) == 0, 0, 1)) %>%
  mutate(ww_bf1 = lag(ww_results, 1),
         ww_bf2 = lag(ww_results, 2),
         ww_bf3 = lag(ww_results, 3),
         ww_bf4 = lag(ww_results, 4),
         ww_bf5 = lag(ww_results, 5),
         ww_bf6 = lag(ww_results, 6),
         ww_bf7 = lag(ww_results, 7)) %>%
  mutate(ww_bf1_tf = ifelse(ww_bf1 == 50, 0, 1),
         ww_bf2_tf = ifelse(ww_bf2 == 50, 0, 1),
         ww_bf3_tf = ifelse(ww_bf3 == 50, 0, 1),
         ww_bf4_tf = ifelse(ww_bf4 == 50, 0, 1),
         ww_bf5_tf = ifelse(ww_bf5 == 50, 0, 1),
         ww_bf6_tf = ifelse(ww_bf6 == 50, 0, 1),
         ww_bf7_tf = ifelse(ww_bf7 == 50, 0, 1)) %>%
  na.omit %>%
  mutate(ww_min5 = min(c(ww_bf1, ww_bf2, ww_bf3, ww_bf4, ww_bf5))) %>%
  mutate(ww_3out5_tf = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf+ww_bf4_tf+ww_bf5_tf >= 3,
                              1, 0)) %>%
  mutate(ww_2out5_tf = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf+ww_bf4_tf+ww_bf5_tf >= 2,
                              1, 0)) %>%
  mutate(consec3 = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf == 3 | 
                            (ww_bf1_tf+ww_bf2_tf+ww_bf3_tf+ww_bf4_tf == 3 & is.na(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf) == T),
                          1, 0)) %>%
  mutate(ww_3out7_tf = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf+ww_bf4_tf+ww_bf5_tf+
                                ww_bf6_tf+ww_bf7_tf >= 3,
                              1, 0)) %>%
  mutate(ww_2out7_tf = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf+ww_bf4_tf+ww_bf5_tf+
                                ww_bf6_tf+ww_bf7_tf >= 2,
                              1, 0)) %>%
  mutate(ww_3out4_tf = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf+ww_bf4_tf >= 3,
                              1, 0)) %>%
  mutate(ww_threshold = ifelse(ww_results <= 27.3, 1, 0)) %>% 
  mutate(ww_1out5_tf = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf+ww_bf4_tf+ww_bf5_tf >= 1,
                              1, 0)) %>%
  mutate(ww_1out4_tf = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf+ww_bf4_tf >= 1,
                              1, 0)) %>%
  mutate(ww_1out3_tf = ifelse(ww_bf1_tf+ww_bf2_tf+ww_bf3_tf >= 1,
                              1, 0))

## remove isolation in apartment
for(i in 1:nrow(isolation)){
  data_full = data_full %>%
    group_by(manhole_id) %>%
    mutate(test_result = ifelse(manhole_id == isolation$manhole_id[i] & date <= isolation$end[i] & 
                                  date >= isolation$start[i], 1, test_result))
}

## training
data_train = data_full %>%
  filter(date <= "2021-04-30")

n_neg = sum(data_train$test_result == 0)
n_pos = sum(data_train$test_result == 1)
data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, 2*1/n_pos)
fit_full = rpart(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                   ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                   ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                 data = data_train, weight = weight,
                 control = rpart.control(cp = 0.02))
plotcp(fit_full)
rpart.plot(fit_full)

split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub("ww_3out7 = 0", "\"+\" < 3 in last 7 days", labs)
  labs <- gsub("ww_1out5 = 0", "\"+\" < 1 in last 5 days", labs)
  labs <- gsub("ww_2out7 = 0", "\"+\" < 2 in last 7 days", labs)
  labs <- gsub("ww_bf1_t = 1", "previous day \"+\"", labs)
  labs }
prp(fit_full,
    type = 1,                # left and right split labels (see Figure 2)
    extra = "auto",
    fallen.leaves = TRUE,
    box.palette = "auto",  # color of the boxes
    branch = .3,
    round = 0,
    leaf.round = 9,
    branch.col = "gray",
    branch.lwd = 2,
    split.fun = split.fun)

yhat = as.numeric(as.character(predict(fit_full, type = "c")))
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
(w = weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight))
table(data_train$test_result, yhat)

## Cross validation tunning for cp
ID = c(0, floor(quantile(1:nrow(data_train), c(0.2, 0.4, 0.6, 0.8, 1))))
cpp = as.vector(outer(c(0.1, 0.01, 0.001, 0.0001), 1:9))
pred_cv = rep(NA, length(cpp))
for(j in 1:length(cpp)){
  pred = rep(NA, 5)
  for(i in 1:5){
    data_cv_train = data_train[(ID[i]+1):ID[i+1],]
    data_cv_valid = setdiff(data_train, data_cv_train)
    fit_full = rpart(as.factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                       ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                       ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                     data = data_train, weight = weight,
                     control = rpart.control(cp = cpp[j]))
    yhat = as.numeric(as.character(predict(fit_full, newdata = data_cv_valid, type = "c")))
    pred[i] = sum(data_cv_valid$weight*(yhat == data_cv_valid$test_result))/sum(data_cv_valid$weight)
  }
  pred_cv[j] = mean(pred[pred != 0])
}

cpp[which.max(pred_cv)]


## adjust the complexity and draw the ROC curve
cpp = c(0.0005, 0.0007, 0.0009, 0.001, 0.005, 0.01)
sscp = matrix(rep(0,2*length(cpp)), ncol = 2)
accuracy = rep(NA, length(cpp))

for(i in 1:length(cpp)){
  fit_full = rpart(test_result ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                     ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf,
                   data = data_train, weight = weight,
                   control = rpart.control(cp = cpp[i]))
  
  yhat = as.numeric(predict(fit_full)[,2] > 0.5)
  accuracy[i] = sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
  TPR = weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)[1,1]/
    sum(weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)[1,])
  FPR = weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)[2,1]/
    sum(weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)[2,])
  sscp[i,] = c(TPR, FPR)
  
}

sscp = data.frame(sscp)
colnames(sscp) = c("TPR", "FPR")
sscp$color = paste0("cp", cpp)

p = ggplot(data = sscp, aes(x = FPR, y = TPR)) +
  geom_point(aes(color = color), data = sscp) +
  scale_fill_continuous(name = "Dose", labels = 2:7) +
  geom_path(aes(FPR, TPR), data=sscp) + 
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  coord_equal()
p

## adjust the weight and draw the ROC curve
ss = matrix(rep(0,2*7), ncol = 2) ## sensitivity & specificity
var_imp = list()

w = c(0.2, 0.5, 1, 1.5, 2, 3, 4)

for(i in 1:length(w)){
  data_train$weight1 = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  fit_full = rpart(as.factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                     ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                     ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                   data = data_train, weight = weight1,
                   control = rpart.control(cp = 0.02))
  var_imp[[i]] = fit_full$variable.importance
  yhat = as.numeric(as.character(predict(fit_full, type = "c")))
  sum(data_train$weight1*(yhat == data_train$test_result))/sum(data_train$weight1)
  TPR = weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight1)[1,1]/
    sum(weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight1)[1,])
  FPR = weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight1)[2,1]/
    sum(weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight1)[2,])
  ss[i,] = c(TPR, FPR)
}

ss = data.frame(ss)
colnames(ss) = c("TPR", "FPR")
ss$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                       "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))

p = ggplot(data = ss, aes(x = FPR, y = TPR)) +
  geom_point(aes(color = color), data = ss) +
  scale_fill_continuous(name = "Dose", labels = 2:7) +
  geom_path(aes(FPR, TPR), data=ss) + 
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  xlab("1-specificity") + 
  ylab("sensitivity") +
  coord_equal() + 
  theme(legend.position = "none")
p

## validation
data_valid = data_full %>%
  filter(date >= "2021-06-30" )

# data_valid = data_full %>%
#   filter(date > "2021-04-10")

# data_valid = data_full %>%
#   filter(date <= "2021-08-20" & date > "2021-06-20")

n_neg = sum(data_valid$test_result == 0)
n_pos = sum(data_valid$test_result == 1)
data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, 2*1/n_pos)
fit_valid = rpart(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                    ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                    ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                  data = data_valid, weight = weight,
                  control = rpart.control(cp = 0.02))
plotcp(fit_valid)
rpart.plot(fit_valid)

split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub("ww_bf1_t = 0", "previous day \"-\"", labs)
  labs <- gsub("ww_1out5 = 0", "\"+\" < 1 in last 5 days", labs)
  labs <- gsub("ww_2out7 = 0", "\"+\" < 2 in last 7 days", labs)
  labs <- gsub("ww_bf3_t = 1", "3 days before \"+\"", labs)
  labs }
prp(fit_valid,
    type = 1,                # left and right split labels (see Figure 2)
    extra = "auto",
    fallen.leaves = TRUE,
    box.palette = "auto",  # color of the boxes
    branch = .3,
    round = 0,
    leaf.round = 9,
    branch.col = "gray",
    branch.lwd = 2,
    split.fun = split.fun,
    cex = 0.9)


yhat = as.numeric(as.character(predict(fit_valid, type = "c")))
sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)

## test training model on validation set
yhat = as.numeric(as.character(predict(fit_full, newdata = data_valid, type = "c")))
sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)

## test validation model on training set (reload the training data)
yhat = as.numeric(predict(fit_valid, newdata = data_train) > 0.5)
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)

## test training model on validation set with different weights
ss = matrix(rep(0,2*7), ncol = 2) ## sensitivity & specificity
var_imp = list()

w = c(0.2, 0.5, 1, 1.5, 2, 3, 4)

for(i in 1:length(w)){
  data_train$weight1 = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  fit_full = rpart(as.factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                     ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                     ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                   data = data_train, weight = weight1,
                   control = rpart.control(cp = 0.02))
  var_imp[[i]] = fit_full$variable.importance
  yhat = as.numeric(as.character(predict(fit_full, newdata = data_valid, type = "c")))
  wcm = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)
  ss[i,] = c(wcm[1,1], wcm[2,1])
}

ss = data.frame(ss)
colnames(ss) = c("TPR", "FPR")
ss$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                       "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))
levels(ss$color) = c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                     "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1")

p2 = ggplot(data = ss, aes(x = FPR, y = TPR)) +
  geom_point(aes(color = color), data = ss) +
  scale_fill_continuous(name = "Dose", labels = 2:7) +
  geom_path(aes(FPR, TPR), data=ss) + 
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  xlab("1-specificity") + 
  ylab("sensitivity") +
  coord_equal() 
p2

plot_grid(p, p2, labels = "AUTO", rel_widths = c(0.3, 0.43))

## random forest
n_neg = sum(data_train$test_result == 0)
n_pos = sum(data_train$test_result == 1)

ss = matrix(rep(0,2*7), ncol = 2) ## sensitivity & specificity

w = c(0.2, 0.5, 1, 1.5, 2, 3, 4)

for(i in 1:length(w)){
  data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  rf_fit = randomForest(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf +
                          ww_bf5_tf + ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + 
                          ww_3out4_tf, classwt = c(1,w[i]), data = data_train,
                        ntree=1000)
  yhat = as.numeric(as.character(predict(rf_fit, type = "c")))
  
  wcm = weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)
  TPR = wcm[1,1]
  FPR = wcm[2,1]
  ss[i,] = c(TPR, FPR)
}

## testing results
ss1 = matrix(rep(0,2*7), ncol = 2) ## sensitivity & specificity
ss2 = matrix(rep(0,2*7), ncol = 2) ## sensitivity & specificity

for(i in 1:length(w)){
  n_neg = sum(data_train$test_result == 0)
  n_pos = sum(data_train$test_result == 1)
  data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  rf_fit = randomForest(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf +
                          ww_bf5_tf + ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + 
                          ww_3out4_tf, classwt = c(1,w[i]), data = data_train,
                        ntree=1000)
  fit_full = rpart(as.factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                     ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                     ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                   data = data_train, weight = weight,
                   control = rpart.control(cp = 0.02))
  n_neg = sum(data_valid$test_result == 0)
  n_pos = sum(data_valid$test_result == 1)
  data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  yhat1 = as.numeric(as.character(predict(rf_fit, newdata = data_valid, type = "c")))
  yhat2 = as.numeric(as.character(predict(fit_full, newdata = data_valid, type = "c")))
  
  wcm1 = weighted_confusion_matrix(data_valid$test_result, yhat1, data_valid$weight)
  wcm2 = weighted_confusion_matrix(data_valid$test_result, yhat2, data_valid$weight)
  TPR1 = wcm1[1,1]
  FPR1 = wcm1[2,1]
  TPR2 = wcm2[1,1]
  FPR2 = wcm2[2,1]
  ss1[i,] = c(TPR1, FPR1)
  ss2[i,] = c(TPR2, FPR2)
}

ss1 = data.frame(ss1); ss2 = data.frame(ss2)
colnames(ss1) = c("TPR", "FPR"); colnames(ss2) = c("TPR", "FPR")
ss1$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                       "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))
levels(ss1$color) = c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                     "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1")
ss2$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                        "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))
levels(ss2$color) = c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                     "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1")

p = ggplot(data = ss1, aes(x = FPR, y = TPR)) +
  geom_point(aes(color = color), data = ss1) +
  scale_fill_continuous(name = "Dose", labels = 2:7) +
  geom_path(aes(FPR, TPR), data=ss1) + 
  xlim(c(0,1)) + 
  ylim(c(0,1)) +
  xlab("1-specificity") + 
  ylab("sensitivity") +
  coord_equal() +
  geom_point(aes(color = color), data = ss2) + 
  geom_path(aes(FPR, TPR), data=ss2, color = "red")
p

set.seed(2021)
rf_fit = randomForest(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf +
                        ww_bf5_tf + ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + 
                        ww_3out4_tf, classwt = c(1,3), data = data_train,
               ntree=1000, do.trace=100)

yhat = as.numeric(as.character(predict(rf_fit, type = "c")))
data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, 3*1/n_pos)
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
(w2 = weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight))
table(data_train$test_result, yhat)
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)

yhat = as.numeric(as.character(predict(rf_fit, newdata = data_valid, type = "c")))
data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, 3*1/n_pos)
(w2 = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight))
sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)

## logistic regression
fit = glm(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
            ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
            ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf, weight = weight, data = data_train, family = "binomial")
yhat = as.numeric(as.character(predict(fit, type = "response"))>0.5)
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)

yhat = as.numeric(as.character(predict.glm(fit, newdata = data_valid, type = "response"))>0.5)
sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)

XX = model.matrix(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                    ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                    ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf, data = data_train)
cv.lasso <- cv.glmnet(XX[,-1], data_train$test_result, family = 'binomial', weights = data_train$weight,
                      type.measure = "class")
c = coef(cv.lasso, cv.lasso$lambda.1se) #set a bigger log(lambda)
inds = which(c!=0)
row.names(c)[inds][-1]

fit = glm(factor(test_result) ~ ww_3out7_tf + ww_1out5_tf + ww_1out3_tf,
          weight = weight, data = data_train, family = "binomial")
yhat = as.numeric(as.character(predict(fit, type = "response"))>0.5)
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)

yhat = as.numeric(as.character(predict.glm(fit, newdata = data_valid, type = "response"))>0.5)
sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)

## SVM
wts = 1/table(data_train$test_result)
wts[2] = 2*wts[2]
svm_model <- svm(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                   ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                   ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf, 
                   data = data_train, type="C-classification", kernel="linear",
                 class.weights = wts)
yhat = predict(svm_model, data_train)
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)

yhat = predict(svm_model, data_valid)
sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)

## Neural Network
nn_model = nnet(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                  ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                  ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf, 
                data = data_train, weights = data_train$weight, size = 10)
yhat = as.numeric(predict(nn_model, data_train) > 0.5)
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight)

yhat = as.numeric(predict(nn_model, data_valid) > 0.5)
sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)

## NPV PPV calculation
p = rep(0, 7)
for(i in 1:7){
  dat_1day = dat_list %>% 
    filter(date > (as.Date("2021-09-12")+i-1) & date <= (as.Date("2021-09-13")+i-1))
  total_pos = sum(dat_1day$total_pos_students)
  total = sum(dat_1day$total_results)
  p[i] = total_pos/total
}
phat = mean(p)


## Read-in the data matched the building name
occupant = read.csv("draft/PPV_NPV/occupant.csv")
occupant$building_name[1:41] = paste0(occupant$building_name[1:41], " Apartments")

building_match = read.csv("draft/PPV_NPV/building_name.csv")

residentN = occupant %>% 
  right_join(building_match, by = "building_name") %>%
  group_by(manhole_id) %>%
  dplyr::summarise(number = sum(count_of_pid))

p_c = 1-(1-phat)^residentN$number
ppv = function(sen, spe, prev){
  ppv1 = sen*prev/(sen*prev + (1-spe)*(1-prev))
  return(ppv1)
}

# w the confusion matrix of the model we train
PPV = ppv(w[1,1], w[2,2], p_c)

npv = function(sen, spe, prev){
  npv1 = spe*(1-prev)/(spe*(1-prev)+(1-sen)*prev)
  return(npv1)
}
NPV = npv(w[1,1], w[2,2], p_c)

residentN = cbind(residentN, PPV, NPV)
write.csv(residentN, file = "draft/PPV_NPV/FA21_ppv_npv.csv", row.names = F)

long <- melt(setDT(residentN), id.vars = c("manhole_id","number"), variable.name = "PPV_NPV")

long %>%
  ggplot( aes(x=PPV_NPV, y=value, fill=PPV_NPV)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("PPV and NPV") +
  xlab("")

residentN_PPV = residentN[order(residentN$PPV),]
residentN_NPV = residentN[order(residentN$NPV),]

plot(PPV~number, data=residentN_PPV, type="b", bty="l", 
     xlab="Resident Number", ylab="Value of PPV or NPV", col=rgb(0.2,0.4,0.1,0.7),
     lwd=3, pch=17, ylim=c(0,1), xlim=c(0,1000), main="PPV and NPV curve")
lines(NPV~number, data = residentN_NPV, col=rgb(0.8,0.4,0.1,0.7), lwd=3, pch=19, type="b")

# Add a legend
legend("bottomright", 
       legend = c("PPV", "NPV"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


## summary for test vs pos
data_train_agg = data_train %>% 
  group_by(manhole_id) %>%
  summarise(test_total = sum(test_num), pos_total = sum(pos_result))

data_train_agg_nonzero = data_train_agg %>%
  filter(pos_total != 0)

ggplot(data_train_agg, aes(x=test_total, y=pos_total)) + 
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

ggplot(data_train_agg_nonzero, aes(x=test_total, y=pos_total)) + 
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)


## More analysis for paper revision
# Impact of vaccination
## training
data_train = data_full %>%
  filter(date < "2020-12-15")
n_neg = sum(data_train$test_result == 0)
n_pos = sum(data_train$test_result == 1)
data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, 2*1/n_pos)
fit_full = rpart(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                   ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                   ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                 data = data_train, weight = weight,
                 control = rpart.control(cp = 0.02))

plotcp(fit_full)
rpart.plot(fit_full)

yhat = as.numeric(as.character(predict(fit_full, type = "c")))
sum(data_train$weight*(yhat == data_train$test_result))/sum(data_train$weight)
(w = weighted_confusion_matrix(data_train$test_result, yhat, data_train$weight))
table(data_train$test_result, yhat)

data_valid = data_full %>%
  filter(date >= "2021-06-30" )

n_neg = sum(data_valid$test_result == 0)
n_pos = sum(data_valid$test_result == 1)
data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, 2*1/n_pos)
fit_valid = rpart(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                    ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                    ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                  data = data_valid, weight = weight,
                  control = rpart.control(cp = 0.02))
plotcp(fit_valid)
rpart.plot(fit_valid)

## test training model on validation set
yhat = as.numeric(as.character(predict(fit_full, newdata = data_valid, type = "c")))
sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)

## sampling the data
data_train = data_full %>%
  filter(date <= "2021-04-30")
min_date = min(data_train$date)
max_date = max(data_train$date)
#sampled_date <- seq(min_date, max_date, by = "3 days")
sampled_date <- seq(min_date, max_date, by = "2 days")
sampled_dat_train <- data_train %>%
  mutate(date = as.Date(date)) %>%
  filter(date %in% as.Date(sampled_date))

n_neg = sum(sampled_dat_train$test_result == 0)
n_pos = sum(sampled_dat_train$test_result == 1)
sampled_dat_train$weight = ifelse(sampled_dat_train$test_result == 0, 1/n_neg, 2*1/n_pos)
sampled_dat_train = sampled_dat_train %>% 
  mutate(ww_2out4_tf = ifelse(ww_bf2_tf+ww_bf4_tf == 2, 1, 0)) %>%
  mutate(ww_3out6_tf = ifelse(ww_bf2_tf+ww_bf4_tf+ww_bf6_tf == 3, 1, 0))
fit_full = rpart(factor(test_result) ~ ww_bf2_tf + ww_bf4_tf + ww_bf6_tf +
                   ww_2out4_tf + ww_3out6_tf,
                 data = sampled_dat_train, weight = weight,
                 control = rpart.control(cp = 0.02))

plotcp(fit_full)
rpart.plot(fit_full)

yhat = as.numeric(as.character(predict(fit_full, type = "c")))
sum(sampled_dat_train$weight*(yhat == sampled_dat_train$test_result))/sum(sampled_dat_train$weight)
(w = weighted_confusion_matrix(sampled_dat_train$test_result, yhat, sampled_dat_train$weight))
table(sampled_dat_train$test_result, yhat)

data_valid = data_full %>%
  filter(date >= "2021-06-30" )
min_date = min(data_valid$date)
max_date = max(data_valid$date)
#sampled_date <- seq(min_date, max_date, by = "3 days")
sampled_date <- seq(min_date, max_date, by = "2 days")
sampled_dat_valid <- data_valid %>%
  mutate(date = as.Date(date)) %>%
  filter(date %in% as.Date(sampled_date))
sampled_dat_valid <- sampled_dat_valid %>%
  mutate(ww_2out4_tf = ifelse(ww_bf2_tf+ww_bf4_tf == 2, 1, 0)) %>%
  mutate(ww_3out6_tf = ifelse(ww_bf2_tf+ww_bf4_tf+ww_bf6_tf == 3, 1, 0))

n_neg = sum(sampled_dat_valid$test_result == 0)
n_pos = sum(sampled_dat_valid$test_result == 1)
sampled_dat_valid$weight = ifelse(sampled_dat_valid$test_result == 0, 1/n_neg, 2*1/n_pos)

yhat = as.numeric(as.character(predict(fit_full, newdata = sampled_dat_valid, type = "c")))
sum(sampled_dat_valid$weight*(yhat == sampled_dat_valid$test_result))/sum(sampled_dat_valid$weight)
weighted_confusion_matrix(sampled_dat_valid$test_result, yhat, sampled_dat_valid$weight)

## adjust the weight and draw the ROC curve for all other methods
ss = matrix(rep(0,2*7), ncol = 2) ## sensitivity & specificity
var_imp = list()

w = c(0.2, 0.5, 1, 1.5, 2, 3, 4)
data_train = data_full %>%
  filter(date <= "2021-04-30")

data_valid = data_full %>%
  filter(date >= "2021-06-30" )

# classification tree
ss = matrix(rep(0,2*7), ncol = 2) ## sensitivity & specificity
ac = rep(0,7)
var_imp = list()

w = c(0.2, 0.5, 1, 1.5, 2, 3, 4)

for(i in 1:length(w)){
  n_neg = sum(data_train$test_result == 0)
  n_pos = sum(data_train$test_result == 1)
  data_train$weight1 = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  fit_full = rpart(as.factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                     ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                     ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
                   data = data_train, weight = weight1,
                   control = rpart.control(cp = 0.02))
  var_imp[[i]] = fit_full$variable.importance
  n_neg = sum(data_valid$test_result == 0)
  n_pos = sum(data_valid$test_result == 1)
  data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  yhat = as.numeric(as.character(predict(fit_full, newdata = data_valid, type = "c")))
  TPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,])
  FPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,])
  ss[i,] = c(TPR, FPR)
  ac[i] = sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
}

ss = data.frame(ss)
colnames(ss) = c("TPR", "FPR")
ss$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                       "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))
levels(ss$color) = c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                     "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1")

# logistic regression
ss1 = matrix(rep(0,2*7), ncol = 2) ## sensitivity & specificity
ac1 = rep(0,7)
for(i in 1:length(w)){
  n_neg = sum(data_train$test_result == 0)
  n_pos = sum(data_train$test_result == 1)
  data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  fit = glm(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
              ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
              ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf,
            weight = weight, data = data_train, family = "binomial")

  n_neg = sum(data_valid$test_result == 0)
  n_pos = sum(data_valid$test_result == 1)
  data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  yhat = as.numeric(as.character(predict(fit, newdata = data_valid, type = "response"))>0.5)
  
  TPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,])
  FPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,])
  ss1[i,] = c(TPR, FPR)
  ac1[i] = sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
}

ss1 = data.frame(ss1)
colnames(ss1) = c("TPR", "FPR")
ss1$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                       "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))

# logistic regression with LASSO
ss2 = matrix(rep(0,2*7), ncol = 2)
ac2 = rep(0,7)
for(i in 1:length(w)){
  n_neg = sum(data_train$test_result == 0)
  n_pos = sum(data_train$test_result == 1)
  
  data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  XX = model.matrix(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                      ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                      ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf, data = data_train)
  cv.lasso <- cv.glmnet(XX[,-1], data_train$test_result, family = 'binomial', weights = data_train$weight,
                        type.measure = "class")
  c = coef(cv.lasso, cv.lasso$lambda.1se) #set a bigger log(lambda)
  inds = which(c!=0)
  
  if(length(inds) == 1){
    fml = as.formula(paste("factor(test_result) ~ ", 1))
  }else{
  fml = as.formula(paste("factor(test_result) ~ ", paste(row.names(c)[inds][-1], collapse = "+")))}
  
  fit = glm(fml, weight = weight, data = data_train, family = "binomial")
  
  n_neg = sum(data_valid$test_result == 0)
  n_pos = sum(data_valid$test_result == 1)
  data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  yhat = as.numeric(as.character(predict(fit, newdata = data_valid, type = "response"))>0.5)
  
  TPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,])
  FPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,])
  ss2[i,] = c(TPR, FPR)
  ac2[i] = sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
}

ss2 = data.frame(ss2)
colnames(ss2) = c("TPR", "FPR")
ss2$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                       "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))

# SVM
ss3 = matrix(rep(0,2*7), ncol = 2)
ac3 = rep(0,7)
for(i in 1:length(w)){
  n_neg = sum(data_train$test_result == 0)
  n_pos = sum(data_train$test_result == 1)
  
  data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  wts = 1/table(data_train$test_result)
  wts[2] = w[i]*wts[2]
  svm_model <- svm(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                     ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                     ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf, 
                   data = data_train, type="C-classification", kernel="linear",
                   class.weights = wts)
  
  n_neg = sum(data_valid$test_result == 0)
  n_pos = sum(data_valid$test_result == 1)
  data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  yhat = predict(svm_model, data_valid)
  
  TPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,])
  FPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,])
  ss3[i,] = c(TPR, FPR)
  ac3[i] = sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
}

ss3 = data.frame(ss3)
colnames(ss3) = c("TPR", "FPR")
ss3$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                        "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))

# Neural Network
ss4 = matrix(rep(0,2*7), ncol = 2)
ac4 = rep(0,7)
for(i in 1:length(w)){
  n_neg = sum(data_train$test_result == 0)
  n_pos = sum(data_train$test_result == 1)
  
  data_train$weight = ifelse(data_train$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  nn_model = nnet(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                    ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                    ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf, 
                  data = data_train, weights = data_train$weight, size = 10)
  
  n_neg = sum(data_valid$test_result == 0)
  n_pos = sum(data_valid$test_result == 1)
  data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  yhat = as.numeric(predict(nn_model, data_valid) > 0.5)
  
  TPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,])
  FPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,])
  ss4[i,] = c(TPR, FPR)
  ac4[i] = sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
}

ss4 = data.frame(ss4)
colnames(ss4) = c("TPR", "FPR")
ss4$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                        "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))

# Random Forest
ss5 = matrix(rep(0,2*7), ncol = 2)
ac5 = rep(0,7)
for(i in 1:length(w)){
  n_neg = sum(data_train$test_result == 0)
  n_pos = sum(data_train$test_result == 1)
  
  rf_fit = randomForest(factor(test_result) ~ ww_bf1_tf + ww_bf2_tf + ww_bf3_tf + ww_bf4_tf + ww_bf5_tf + 
                          ww_3out5_tf + ww_2out5_tf + consec3 + ww_3out7_tf + ww_3out4_tf + 
                          ww_1out5_tf + ww_1out3_tf + ww_1out4_tf + ww_2out7_tf, classwt = c(1,w[i]), data = data_train,
                        ntree=1000)
  n_neg = sum(data_valid$test_result == 0)
  n_pos = sum(data_valid$test_result == 1)
  data_valid$weight = ifelse(data_valid$test_result == 0, 1/n_neg, w[i]*1/n_pos)
  yhat = as.numeric(as.character(predict(rf_fit, newdata = data_valid, type = "c")))
  
  TPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[1,])
  FPR = weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,1]/
    sum(weighted_confusion_matrix(data_valid$test_result, yhat, data_valid$weight)[2,])
  ss5[i,] = c(TPR, FPR)
  ac5[i] = sum(data_valid$weight*(yhat == data_valid$test_result))/sum(data_valid$weight)
}
ss5 = data.frame(ss5)
colnames(ss5) = c("TPR", "FPR")
ss5$color = as.factor(c("weight 0.2:1", "weight 0.5:1", "weight 1:1",
                        "weight 1.5:1", "weight 2:1", "weight 3:1", "weight 4:1"))

ss_tot = rbind(ss, ss1, ss2, ss3, ss4, ss5)
ss_tot <- ss_tot %>% 
  rename(shape = color) %>%
  mutate(color = rep(c("classification tree", "logistic regression",
                     "logistic regression (with LASSO)", "SVM", "Neural Network", "Random Forest"), each = 7))
