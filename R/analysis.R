library(mlr)
library(iml)
library(dplyr)
library(healthcareai)
library(ggplot2)
library(plyr)
library(featureImportance)
library(gridExtra)

# load data and results of benchmarking
load("data/benchmark_results_auc_3.RData")
data = readRDS("data/df_newdataprep2.rds")

# impute missing values of individual variables
data = mlr::impute(data, cols = list(Z_n100 = imputeMean(), Z_n200 = imputeMean(),Z_n300 = imputeMean()))$data


# find best model (without smote) regarding test performance
results = data.frame(res)
results_wo_smote = results[results$learner.id %in% c("classif.ksvm.tuned","classif.kknn.tuned","classif.xgboost.tuned",
                                                     "classif.ranger.tuned","classif.ctree","classif.naiveBayes",
                                                     "classif.logreg"),]
results_wo_smote$learner.id = substr(results_wo_smote$learner.id,9,(nchar(results_wo_smote$learner.id)))
colnames(results_wo_smote)[2] = "learner"
results_wo_smote$learner = factor(results_wo_smote$learner, levels = c("naiveBayes","logreg","ctree","kknn.tuned",
                                                                       "ksvm.tuned","ranger.tuned","xgboost.tuned"))
results_wo_smote$learner = mapvalues(results_wo_smote$learner, 
                                     from = c("naiveBayes","logreg","ctree","kknn.tuned","ksvm.tuned","ranger.tuned",
                                              "xgboost.tuned"), 
                                     to = c("naive Bayes","log reg","tree","knn","svm","rf","xgboost"))

# boxplot with performance of outer folds
p_perf = ggplot(results_wo_smote, aes(x=learner, y=auc)) + geom_boxplot(aes(fill = learner)) + theme_bw() +
  theme(legend.position = "none") + ylab("AUC")

ggsave("figures/performance.png", p_perf, width = 7, height = 4)

# table with average performance measures
results_wo_smote %>% dplyr::group_by(learner) %>% dplyr::summarise("auc" = mean(auc), "bac" = mean(bac), 
                                                                   "f1" = mean(f1), "tpr" = mean(tpr),  "ppv" = mean(ppv))


####################################################################################################
# MODEL INTERPRETATION

# choose xgboost model for further analysis
data_mod = res$results$normal[["classif.xgboost.tuned"]]



# calculate feature importance for individual features and feature groups

data_analysis = data[train,-(1:2)]

  for(i in 1:length(data_mod$models)){
    model = data_mod$models[[i]]
    # calculate individual and grouped feature importance
    imp_ind = featureImportance(model,data_analysis[data_mod$pred$instance$test.inds[[i]],], n.feat.perm = 10, measures = list(mlr::auc))
    imp_group = featureImportance(model,data_analysis[data_mod$pred$instance$test.inds[[i]],], 
                      features = list(c("Z_n100","Z_n200","Z_n300","Z_lower"),
                                      c("Z_upper","Z_all"), 
                                      c("Z_lower_sd","Z_upper_sd","Z_all_sd"),
                                      c("W_lower","W_upper","W_all"), 
                                      c("W_lower_sd","W_upper_sd","W_all_sd"), 
                                      c("SW_lower","SW_upper","SW_all"), 
                                      c("SW_lower_sd","SW_upper_sd","SW_all_sd"), 
                                      "SL"), 
                      n.feat.perm = 10, measures = list(mlr::auc))
    imp_group2 = featureImportance(model,data_analysis[data_mod$pred$instance$test.inds[[i]],], 
                                  features = list(c("Z_n100","Z_n200","Z_n300","Z_lower", "W_lower","SW_lower"),
                                                  c("Z_upper","SW_upper","W_upper"), 
                                                  c("Z_lower_sd","W_lower_sd", "SW_lower_sd" ),
                                                  c("W_all","Z_all","SW_all"), 
                                                  c("Z_upper_sd","SW_upper_sd","W_upper_sd"), 
                                                  c("Z_all_sd","SW_all_sd","W_all_sd"), 
                                                  "SL"), 
                                  n.feat.perm = 10, measures = list(mlr::auc))
    if(i==1){
      imp_ind_all = imp_ind$importance
      imp_ind_all$fold = i
      imp_group_all = imp_group$importance
      imp_group_all$fold = i
      imp_group_all2 = imp_group2$importance
      imp_group_all2$fold = i
    }
    else{
      imp_ind_all = rbind(imp_ind_all, cbind(imp_ind$importance, "fold" = i))
      imp_group_all = rbind(imp_group_all, cbind(imp_group$importance, "fold" = i))
      imp_group_all2 = rbind(imp_group_all2, cbind(imp_group2$importance, "fold" = i))
    } 
  }

# visualize importance for individual features
imp_ind_aggr = imp_ind_all %>% dplyr::group_by(features, fold) %>% dplyr::summarise("importance" = -mean(auc))
imp_feat_aggr = imp_ind_all%>% dplyr::group_by(features) %>% dplyr::summarise("importance" = -mean(auc))
imp_feat_aggr = arrange(imp_feat_aggr, importance, decreasing = TRUE)
p_imp_ind = ggplot(imp_ind_aggr, aes(y = factor(features, levels = imp_feat_aggr$features), x = importance)) + 
  geom_boxplot(fill = "gray") + theme_bw() + ylab("features")

# visualize importance for defined groups
imp_group_aggr = imp_group_all %>% dplyr::group_by(features, fold) %>% dplyr::summarise("importance" = -mean(auc))
imp_featgroup_aggr = imp_group_all%>% dplyr::group_by(features) %>% dplyr::summarise("importance" = -mean(auc))
imp_featgroup_aggr = arrange(imp_featgroup_aggr, importance, decreasing = TRUE)
p_imp_group = ggplot(imp_group_aggr, aes(y = factor(features, levels = imp_featgroup_aggr$features), x = importance)) + geom_boxplot(fill = "gray") + theme_bw() +
  ylab("feature groups")

# visualize importance for defined groups 2
imp_group_aggr2 = imp_group_all2 %>% dplyr::group_by(features, fold) %>% dplyr::summarise("importance" = -mean(auc))
imp_featgroup_aggr2 = imp_group_all2%>% dplyr::group_by(features) %>% dplyr::summarise("importance" = -mean(auc))
imp_featgroup_aggr2 = arrange(imp_featgroup_aggr2, importance, decreasing = TRUE)
p_imp_group2 = ggplot(imp_group_aggr2, aes(y = factor(features, levels = imp_featgroup_aggr2$features), x = importance)) + 
  geom_boxplot(fill = "gray") + theme_bw() + ylab("feature groups")

p = cowplot::plot_grid(p_imp_group, p_imp_group2, align = "v", ncol = 2)

# save plots
ggsave("figures/single_importance.png",p_imp_ind, width = 4, height = 5)
ggsave("figures/group_importance.png",p, width = 12, height = 5)


# calculate Feature Effects

# train model on all data (tuning on all training data using 5 fold CV and train model again with best hyperparameters on all data)
blocking = (data[train,] %>% dplyr::group_by(En) %>% dplyr::summarise(n = n()))
task     = makeClassifTask(id = "normal", data = task$env$data, target = "RTB_Br09", 
                           blocking = factor(rep(blocking$En, times = blocking$n)))

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
rdesc = makeResampleDesc("CV", iters = 5, blocking.cv = TRUE)
lrn = makeLearner("classif.xgboost", predict.type = "prob")
set.seed(1234)
res = tuneParams(lrn, task = task, resampling = rdesc,
                 par.set = ps, control = ctrl, measures = mlr::auc)

# set hyperparameters to best found ones and train on all data
lrn = setHyperPars(makeLearner("classif.xgboost", predict.type = "prob"), nrounds = res$x$nrounds, max_depth = res$x$max_depth,
                   eta = res$x$eta, lambda = res$x$lambda)

# train model on all training data
m = mlr::train(lrn, task)

# predict data for testset
testset = data[test,]
predictions = predict(m, newdata = testset[,colnames(task$env$data)])
testset$pred_prob_C = predictions$data$prob.C
testset$pred_label = predictions$data$response
write.csv(testset, file = "data/testset_predictions.csv")

# define predictor and calculate feature effects 
pred = Predictor$new(m, data=task$env$data, y = "RTB_Br09", type = "prob", class = "C")

# PDP and ICE curves for all parameters
for(i in pred$model$features){
  effect = FeatureEffect$new(pred, feature = i, method = "pdp+ice")
  p = plot(effect) + theme_bw() + ylab("Predicted Convective Probabilities")
  # save plots
  ggsave(paste0("figures/effects_",i,".png"), p, width = 4.5, height = 4)
}




####################################################################################################
# local/regional explanations on testset using Shapley values


testset = data[test,]
#ind = which(testset$En == 90)

# performance for testset
df = predict(m, newdata = testset[,-which(colnames(data)%in%c("En","Date"))])
r = calculateROCMeasures(df)

#indices of wrong classifications
ind.misclass.C = which((df$data$truth == "C") & (df$data$response == "S"))
ind.misclass.S = which((df$data$truth == "S") & (df$data$response == "C"))

# ind correct classified
ind.corclass.C = which((df$data$truth == "C") & (df$data$response == "C"))
ind.corclass.S = which((df$data$truth == "S") & (df$data$response == "S"))

# convective wrongly classified as stratiform with high/low prob for stratiform
ind.misclass.C.high = df[(df$truth == "C") & (df$response == "S") & (df$prob.C < 0.1), "id"]
ind.misclass.C.low = df[(df$truth == "C") & (df$response == "S") & (df$prob.C > 0.3), "id"]



# Shapley values for individual and subset explanations

# calculate shapley values for testset
pred = Predictor$new(m, data=task$env$data[,-which(colnames(task$env$data)=="RTB_Br09")], type = "prob")
X = testset[,-which(colnames(data)%in%c("En","Date","RTB_Br09"))]
for(i in 1:nrow(testset)){
  if(i == 1){
    shap = Shapley$new(pred, x.interest = X[i,])
    shap.res = shap$results
    shap.res$id = i
  }
  else {
    shap.new = Shapley$new(pred, x.interest = X[i,])
    shap.res = rbind(shap.res, cbind(shap.new$results, "id" = i))
  }
}

# create dataset with all shapley values
shap.res.C = shap.res[(shap.res$class=="C"),]
shap.res.C =join(shap.res.C, data.frame("id"=1:nrow(df$data),"response"=df$data$response), by = "id")
shap.res.C =join(shap.res.C, data.frame(cbind("id"=1:nrow(testset),"En"=testset$En)), by = "id")
shap.res.C =join(shap.res.C, data.frame(cbind("id"=1:nrow(testset),"RTB_Br09"=testset$RTB_Br09)), by = "id")
saveRDS(shap.res.C, "data/shap_values.rds")


# calculate groupwise shap for each feature

# misclassified examples of testset
shap.res.grouped.1 = shap.res[(shap.res$class=="C") & (shap.res$id %in% ind.misclass.C),] %>% group_by(feature) %>% dplyr::summarize(mean = mean(phi))
shap.res.grouped.2 = shap.res[(shap.res$class=="C") & (shap.res$id %in% ind.misclass.S),] %>% group_by(feature) %>% dplyr::summarize(mean = mean(phi))

# # plot influence of each feature for two misclassified subsets
# p1 = ggplot(shap.res.grouped.1) + geom_bar(aes(x = feature, y = mean), stat = "identity") + ylim(-0.15,0.8) +
#   theme(axis.text.x=element_text(angle =45, size = 7, vjust = 0.5)) + theme_bw() + ylab("average Shapley value Convective")
# p2 = ggplot(shap.res.grouped.2) + geom_bar(aes(x = feature, y = mean), stat = "identity") + ylim(-0.15,0.8) +
#   theme(axis.text.x=element_text(angle =45, size = 7, vjust = 0.5)) + theme_bw()+ ylab("average Shapley value Stratiform")
# 
# ggsave(paste0("figures/expl_misclassified_shap.png"), gridExtra::grid.arrange(p1,p2, nrow = 1))

# # correct classified of testset
# shap.res.grouped.1 = shap.res[(shap.res$class=="C") & (shap.res$id %in% ind.corclass.C),] %>% group_by(feature) %>% dplyr::summarize(mean = mean(phi))
# shap.res.grouped.2 = shap.res[(shap.res$class=="C") & (shap.res$id %in% ind.corclass.S),] %>% group_by(feature) %>% dplyr::summarize(mean = mean(phi))
# 
# # plot influence of each feature for two misclassified subsets
# p1 = ggplot(shap.res.grouped.1) + geom_bar(aes(x = feature, y = mean), stat = "identity") +
#   theme(axis.text.x=element_text(angle =45, size = 7, vjust = 0.5)) + theme_bw() + ylab("average Shapley value Convective")
# p2 = ggplot(shap.res.grouped.2) + geom_bar(aes(x = feature, y = mean), stat = "identity") +
#   theme(axis.text.x=element_text(angle =45, size = 7, vjust = 0.5)) + theme_bw()+ ylab("average Shapley value Stratiform")
# gridExtra::grid.arrange(p1,p2, nrow = 1)


# explanations for each test event
for(i in 1:length(unique(testset$En))){
  shap.res.grouped.C = shap.res[(shap.res$class=="C")&(shap.res$id %in% (which(testset$En==unique(testset$En)[i] &(df$data$response == "C")))),] %>% group_by(feature) %>% dplyr::summarize(mean = mean(phi, na.rm = T))
  shap.res.grouped.S = shap.res[(shap.res$class=="C")&(shap.res$id %in% (which(testset$En==unique(testset$En)[i] &(df$data$response == "S")))),] %>% group_by(feature) %>% dplyr::summarize(mean = mean(phi, na.rm = T))
  p.C = ggplot(shap.res.grouped.C) + geom_bar(aes(x = feature, y = mean), stat = "identity") +  theme_bw() + ylab("average Shapley value Convective") +
    theme(axis.text.x=element_text(angle =45, size = 7, vjust = 0.5)) #+ ylim(min(shap.res.grouped.C$mean)-0.05,max(shap.res.grouped.C$mean)+0.05)
  p.S = ggplot(shap.res.grouped.S) + geom_bar(aes(x = feature, y = -mean), stat = "identity") +  theme_bw() + ylab("average Shapley value Stratiform") +
    theme(axis.text.x=element_text(angle =45, size = 7, vjust = 0.5)) #+ ylim(min(shap.res.grouped.S$mean)-0.05,max(shap.res.grouped.S$mean)+0.05)
  ggsave(paste0("figures/expl_Ev",unique(testset$En)[i],".png"), gridExtra::grid.arrange(p.C,p.S, nrow = 1))
}


