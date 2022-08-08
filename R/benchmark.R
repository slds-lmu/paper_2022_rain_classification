library(BBmisc)
library(mlr)
library(data.table)
library(stringi)
library(e1071)
library(batchtools)
library(checkmate)
library(RSNNS)
library(partykit)
library(party)
library(healthcareai)
library(dplyr)
library(kernlab)
library(FSelectorRcpp)
library(irace)


# DATA

# read data
data = readRDS("data/df_newdataprep2.rds")

# impute missing values of individual lower height features with their mean value
data = mlr::impute(data, cols = list(Z_n100 = imputeMean(), Z_n200 = imputeMean(),Z_n300 = imputeMean()))$data

# domain defined test set
test = which(data$En %in% c(186, 191, 196, 204))
train = setdiff(1:nrow(data), test)
# split rest of events randomly in training and test set
# train - test split: 95 percent for training and evaluation (event-based splitting)
#set.seed(123)
#data.split = split_train_test(data[-ids_ev,], RTB_Br09, grouping_col = En, percent_train = 0.95)
#train      = which(data$En %in% data.split$train$En)
#test       = which(data$En %in% data.split$test$En)


# TASK 

# definition with Event as block variable for CV within training
blocking = (data[train,] %>% dplyr::group_by(En) %>% dplyr::summarise(n = n()))
task     = makeClassifTask(id = "normal", data = data[train,][,-which(colnames(data)%in%c("En","Date"))], target = "RTB_Br09", blocking = factor(rep(blocking$En, times = blocking$n)))


# TUNING

# evaluation measure within tuning/optimization process
measure = auc


# Define learners and their parameter sets

# SVM
srange <- log(sigest(RTB_Br09 ~.,data = getTaskData(task)))/log(2) # range definition for sigma tuning
ps = makeParamSet(
  makeNumericParam("C", lower = -2, upper = 10, trafo = function(x) 2^x),
  makeDiscreteParam("kernel", values = c("rbfdot")),
  makeNumericParam("sigma", lower = as.numeric(srange[1]), upper = as.numeric(srange[3]), trafo = function(x) 2^x,
                   requires = quote(kernel == "rbfdot"))
)
ctrl = makeTuneControlIrace(maxExperiments = 180L)
inner = makeResampleDesc("Holdout", blocking.cv = TRUE)
lrn1 = makeTuneWrapper(makeLearner("classif.ksvm", predict.type = "prob"),
                       resampling = inner, par.set = ps, control = ctrl, measures = measure,
                       show.info = TRUE)


# smote learners
lrn1.2 = makeTuneWrapper(makeSMOTEWrapper(makeLearner("classif.ksvm", predict.type = "prob"), sw.rate = 8, sw.alt.logic = TRUE),
                       resampling = inner, par.set = ps, control = ctrl, measures = measure,
                       show.info = TRUE)



# KNN
ps = makeParamSet(makeDiscreteParam("k", 3:15))
ctrl = makeTuneControlGrid()
inner = makeResampleDesc("Holdout", blocking.cv = TRUE)
lrn2 = makeTuneWrapper(makeLearner("classif.kknn", predict.type = "prob"),
                       resampling = inner, par.set = ps, control = ctrl,measures =measure,
                       show.info = TRUE)

# smote wrapper
lrn2.2 = makeTuneWrapper(makeSMOTEWrapper(makeLearner("classif.kknn", predict.type = "prob"), sw.rate = 8, sw.alt.logic = TRUE),
                       resampling = inner, par.set = ps, control = ctrl,measures = measure,
                       show.info = TRUE)

# XGBOOST
ps <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 500),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .05, upper = .3),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)
ctrl = makeTuneControlIrace(maxExperiments = 180L)
inner = makeResampleDesc("Holdout", blocking.cv = TRUE)
lrn3 = makeTuneWrapper(makeLearner("classif.xgboost", predict.type = "prob"),
                       resampling = inner, par.set = ps, control = ctrl, measures = measure,
                       show.info = TRUE)

# smote wrapper
lrn3.2 = makeTuneWrapper(makeSMOTEWrapper(makeLearner("classif.xgboost", predict.type = "prob"), sw.rate = 8, sw.alt.logic = TRUE),
                         resampling = inner, par.set = ps, control = ctrl,measures = measure,
                         show.info = TRUE)


# RANGER 
ps = makeParamSet(
  makeDiscreteParam("num.trees", values = c(400,600,800,1000)),
  makeDiscreteParam("mtry", values = c(4:6))
)
ctrl = makeTuneControlGrid()
inner = makeResampleDesc("Holdout", blocking.cv = TRUE)
lrn4 = makeTuneWrapper(makeLearner("classif.ranger", predict.type = "prob"),
                       resampling = inner, par.set = ps, control = ctrl, measures = measure,
                       show.info = TRUE)

# smote wrapper
lrn4.2 = makeTuneWrapper(makeSMOTEWrapper(makeLearner("classif.ranger", predict.type = "prob"), sw.rate = 8, sw.alt.logic = TRUE),
                         resampling = inner, par.set = ps, control = ctrl,measures = measure,
                         show.info = TRUE)




# Learners of models that are not tuned: ctree, naive bayes and logistic regression
lrn5 = makeLearner("classif.ctree", predict.type = "prob")
lrn5.2 = makeSMOTEWrapper(makeLearner("classif.ctree", predict.type = "prob"), sw.rate = 8, sw.alt.logic = TRUE)
lrn6 = makeLearner("classif.naiveBayes", predict.type = "prob")
lrn6.2 = makeSMOTEWrapper(makeLearner("classif.naiveBayes", predict.type = "prob"), sw.rate = 8, sw.alt.logic = TRUE)
lrn7 = makeLearner("classif.logreg", predict.type = "prob")
lrn7.2 = makeSMOTEWrapper(makeLearner("classif.logreg", predict.type = "prob"), sw.rate = 8, sw.alt.logic = TRUE)


lrns = list(lrn1, lrn2, lrn2.2, lrn3, lrn3.2, lrn4, lrn4.2, lrn5, lrn5.2, lrn6, lrn6.2, lrn7, lrn7.2)

lrns = list(lrn1, lrn2, lrn3, lrn4, lrn5, lrn6, lrn7)

# DEFINE OUTER RESAMPLING STRATEGY
outer = list(makeResampleDesc("CV", iters = 5, blocking.cv = TRUE))


# BENCHMARKING
time = Sys.time()
set.seed(1234)
res = benchmark(lrns, task, outer,
                measures = list(mlr::auc, mlr::f1, mlr::tpr, mlr::fnr, mlr::fpr, mlr::bac, mlr::acc, mlr::ppv, mlr::brier), show.info = FALSE,
                keep.extract = TRUE, models = TRUE)
Sys.time()


# SAVE RESULTS
#save(res, task, train, test, file = "data/benchmark_results_auc_smoteWrapped2.RData")
save(res, task, train, test, file = "data/benchmark_results_auc_3.RData")



