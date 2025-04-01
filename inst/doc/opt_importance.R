## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ranger)
library(optRF)
SNPdata[1:5,1:5]

## ----echo=FALSE, message=FALSE------------------------------------------------
load("opt_importance_vignette_initData.Rda")

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123) # Set a seed for reproducibility
# RF_model = ranger(y=SNPdata[,1], x=SNPdata[,-1], write.forest = TRUE, importance="permutation")
# D_VI = data.frame(variable = names(SNPdata)[-1], importance = RF_model$variable.importance)
# D_VI = D_VI[order(D_VI$importance, decreasing=TRUE),]

## -----------------------------------------------------------------------------
head(D_VI)

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(321) # Set a seed for reproducibility
# RF_model_2 = ranger(y=SNPdata[,1], x=SNPdata[,-1], write.forest = TRUE, importance="permutation")
# D_VI_2 = data.frame(variable = names(SNPdata)[-1], importance = RF_model_2$variable.importance)
# D_VI_2 = D_VI_2[order(D_VI_2$importance, decreasing=TRUE),]

## ----fig.width=6, fig.height=4.5, fig.align='center'--------------------------
M = merge(D_VI, D_VI_2, by="variable")
plot(M$importance.x, M$importance.y,
     xlab="Variable importances in the first run", ylab="Variable importances in the second run")
cor(M$importance.x, M$importance.y)

## ----eval=FALSE, message=FALSE------------------------------------------------
# num.trees_values = c(500, 2500, 5000, 10000, 15000, 20000)
# result = data.frame()
# for(i in num.trees_values){
# 
#   start.time = Sys.time()
# 
#   set.seed(123)
#   RF_model_1 = ranger(y=SNPdata[,1], x=SNPdata[,-1], num.trees=i,
#                       write.forest = TRUE, importance="permutation")
#   D_VI_1 = data.frame(variable = names(SNPdata)[-1], importance = RF_model_1$variable.importance)
# 
#   set.seed(321)
#   RF_model_2 = ranger(y=SNPdata[,1], x=SNPdata[,-1],  num.trees=i,
#                       write.forest = TRUE, importance="permutation")
#   D_VI_2 = data.frame(variable = names(SNPdata)[-1], importance = RF_model_2$variable.importance)
#   end.time = Sys.time()
# 
#   M = merge(D_VI_1, D_VI_2, by="variable")
#   result = rbind(result, data.frame(number_of_trees = i,
#                                     variable_importance_stability = cor(M$importance.x, M$importance.y),
#                                     computation_time = (end.time - start.time)/2))
# }

## -----------------------------------------------------------------------------
result

## ----fig.width=8, fig.height=4.5, fig.align='center'--------------------------
par(mfrow=c(1,2))
plot(variable_importance_stability ~ number_of_trees, data=result)
plot(computation_time ~ number_of_trees, data=result)
abline(lm(result$computation_time ~ result$number_of_trees), lwd=2, col="grey")

## ----echo=FALSE, message=FALSE------------------------------------------------
load("opt_importance_vignette_optData.Rda")

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
# set.seed(123)
# optRF_result = opt_importance(y=SNPdata[,1], X=SNPdata[,-1])

## ----message=FALSE------------------------------------------------------------
summary(optRF_result)

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123)
# RF_model_1 = ranger(y=SNPdata[,1], x=SNPdata[,-1], num.trees=optRF_result$recommendation,
#                    write.forest = TRUE, importance="permutation")
# D_VI_1 = data.frame(variable = names(SNPdata)[-1],
#                     importance = RF_model_1$variable.importance)
# 
# set.seed(321)
# RF_model_2 = ranger(y=SNPdata[,1], x=SNPdata[,-1], num.trees=optRF_result$recommendation,
#                    write.forest = TRUE, importance="permutation")
# D_VI_2 = data.frame(variable = names(SNPdata)[-1],
#                     importance = RF_model_2$variable.importance)
# 
# M = merge(D_VI_1, D_VI_2, by="variable")

## ----message=FALSE, fig.width=6, fig.height=4.5, fig.align='center'-----------
plot(M$importance.x, M$importance.y,
     xlab="Variable importances in the first run", ylab="Variable importances in the second run")
cor(M$importance.x, M$importance.y)

## ----eval=FALSE, message = FALSE, warning=FALSE-------------------------------
# set.seed(123)
# RF_model = ranger(y=SNPdata[,1], x=SNPdata[,-1], num.trees=optRF_result$recommendation,
#                  write.forest = TRUE, importance="permutation")

## ----message = FALSE, warning=FALSE, fig.width=6, fig.height=4.5, fig.align='center'----
hist(RF_model$variable.importance, xlim=c(-10,50),
     main="Histogram of variable importances", xlab="")

## ----fig.width=6, fig.height=4.5, fig.align='center'--------------------------
hist(RF_model$variable.importance, ylim=c(0,20), xlim=c(-10,50),
     main="Histogram of variable importances", xlab="")
abline(v=5, col="red", lwd=4)
selection_size = sum(RF_model$variable.importance>5)
selection_size

## ----eval=FALSE, message = FALSE, warning=FALSE-------------------------------
# set.seed(123)
# optRF_result_2 = opt_importance(y=SNPdata[,1], X=SNPdata[,-1],
#                               recommendation = "selection",
#                               alpha = selection_size)

## ----message=FALSE------------------------------------------------------------
summary(optRF_result_2)

## ----eval=FALSE, message = FALSE, warning=FALSE-------------------------------
# set.seed(123)
# RF_model_1 = ranger(y=SNPdata[,1], x=SNPdata[,-1], num.trees=optRF_result_2$recommendation,
#                    write.forest = TRUE, importance="permutation")
# D_VI_1 = data.frame(variable = names(SNPdata)[-1],
#                     importance = RF_model_1$variable.importance)
# D_VI_1 = D_VI_1[order(D_VI_1$importance, decreasing=TRUE),]
# D_VI_1$variable_ranking = c(1:nrow(D_VI_1))
# selected_variables_1 = D_VI_1[1:selection_size,1]
# 
# set.seed(321)
# RF_model_2 = ranger(y=SNPdata[,1], x=SNPdata[,-1], num.trees=optRF_result_2$recommendation,
#                    write.forest = TRUE, importance="permutation")
# D_VI_2 = data.frame(variable = names(SNPdata)[-1],
#                     importance = RF_model_2$variable.importance)
# D_VI_2 = D_VI_2[order(D_VI_2$importance, decreasing=TRUE),]
# D_VI_2$variable_ranking = c(1:nrow(D_VI_2))
# selected_variables_2 = D_VI_2[1:selection_size,1]

## -----------------------------------------------------------------------------
sum(selected_variables_1 %in% selected_variables_2)

## ----fig.width=6, fig.height=4.5, fig.align='center'--------------------------
plot_stability(optRF_result_2, measure="selection", 
               0, 100000, add_recommendation = FALSE, ylim=c(0, 1))
abline(h=1, col="red")

## ----message=FALSE------------------------------------------------------------
estimate_numtrees(optRF_result_2, measure="selection", for_stability = 0.9)
estimate_numtrees(optRF_result_2, measure="selection", for_stability = 0.95)
estimate_numtrees(optRF_result_2, measure="selection", for_stability = 0.99)
estimate_numtrees(optRF_result_2, measure="selection", for_stability = 0.999)

## ----message=FALSE------------------------------------------------------------
estimate_stability(optRF_result_2, with_num.trees=500000)

## ----echo=FALSE, message=FALSE------------------------------------------------
load("opt_importance_vignette_stabilityData.Rda")

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123)
# stability_imp_500 = measure_stability(y = Training[,1], X=Training[,-1], X_Test=Test, num.trees=500, method="importance")

## ----message=FALSE------------------------------------------------------------
stability_imp_500

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123)
# stability_imp_10000 = measure_stability(y = Training[,1], X=Training[,-1], X_Test=Test, num.trees=10000, method="importance")

## ----message=FALSE------------------------------------------------------------
stability_imp_10000

