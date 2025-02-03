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
load("opt_prediction_vignette_initData.Rda")

## ----message=FALSE------------------------------------------------------------
Training = SNPdata[1:200,] # Rows 1 to 200 as training data
Test = SNPdata[201:250,-1] # Rows 201 to 250 as test data, excluding the response column (column 1)

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123) # Set a seed for reproducibility
# RF_model = ranger(y=Training[,1], x=Training[,-1], write.forest = TRUE)

## ----message=FALSE------------------------------------------------------------
predictions = predict(RF_model, data=Test)$predictions
predicted_Test = data.frame(ID = row.names(Test), predicted_yield = predictions)
head(predicted_Test)

## ----message=FALSE------------------------------------------------------------
predicted_Test = predicted_Test[order(predicted_Test$predicted_yield, decreasing=TRUE),] 
selected_individuals = predicted_Test[1:5,1] 
selected_individuals

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(321) # Set a different seed for reproducibility
# RF_model_2 = ranger(y=Training[,1], x=Training[,-1], write.forest = TRUE)
# predictions_2 = predict(RF_model_2, data=Test)$predictions
# predicted_Test_2 = data.frame(ID = row.names(Test), predicted_yield = predictions_2)

## ----fig.width=6, fig.height=4.5, fig.align='center'--------------------------
M = merge(predicted_Test, predicted_Test_2, by="ID")
plot(M$predicted_yield.x, M$predicted_yield.y,
     xlab="Predicted yield in the first run", ylab="Predicted yield in the second run")
cor(M$predicted_yield.x, M$predicted_yield.y)

## ----message=FALSE------------------------------------------------------------
predicted_Test_2 = predicted_Test_2[order(predicted_Test_2$predicted_yield, decreasing=TRUE),]
selected_individuals_2 = predicted_Test_2[1:5,1]

## ----message=FALSE------------------------------------------------------------
selected_individuals
selected_individuals_2

## ----eval=FALSE, message=FALSE------------------------------------------------
# num.trees_values = c(500, 1000, 1500, 2000, 2500, 3000)
# result = data.frame()
# for(i in num.trees_values){
# 
#   start.time = Sys.time()
# 
#   set.seed(123)
#   RF_model_1 = ranger(y=Training[,1], x=Training[,-1], write.forest = TRUE, num.trees=i)
#   predictions_1 = predict(RF_model_1, data=Test)$predictions
#   predicted_Test_1 = data.frame(ID = row.names(Test), predicted_yield = predictions_1)
#   predicted_Test_1 = predicted_Test_1[order(predicted_Test_1$predicted_yield, decreasing=TRUE),]
#   selected_individuals_1 = predicted_Test_1[1:5,1]
# 
#   set.seed(321)
#   RF_model_2 = ranger(y=Training[,1], x=Training[,-1], write.forest = TRUE, num.trees=i)
#   predictions_2 = predict(RF_model_2, data=Test)$predictions
#   predicted_Test_2 = data.frame(ID = row.names(Test), predicted_yield = predictions_2)
#   predicted_Test_2 = predicted_Test_2[order(predicted_Test_2$predicted_yield, decreasing=TRUE),]
#   selected_individuals_2 = predicted_Test_2[1:5,1]
# 
#   end.time = Sys.time()
# 
#   M = merge(predicted_Test_1, predicted_Test_2, by="ID")
#   result = rbind(result, data.frame(number_of_trees = i,
#                                     prediction_stability = cor(M$predicted_yield.x, M$predicted_yield.y),
#                                     selection_stability = sum(selected_individuals_1 %in% selected_individuals_2)/5,
#                                     computation_time = end.time - start.time))
# }

## ----message=FALSE------------------------------------------------------------
result

## ----fig.width=8, fig.height=4.5, fig.align='center'--------------------------
par(mfrow=c(1,2))
plot(prediction_stability ~ number_of_trees, data=result)
plot(computation_time ~ number_of_trees, data=result)
abline(lm(result$computation_time ~ result$number_of_trees), lwd=2, col="grey")

## ----echo=FALSE, message=FALSE------------------------------------------------
load("opt_prediction_vignette_optData.Rda")

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123) # Set a seed for reproducibility
# optRF_result = opt_prediction(y=Training[,1], X=Training[,-1], X_Test=Test,
#                               alpha=0.1)

## ----message=FALSE------------------------------------------------------------
summary(optRF_result)

## ----eval=FALSE---------------------------------------------------------------
# set.seed(123)
# RF_model_1 = ranger(y=Training[,1], x=Training[,-1],
#                     write.forest = TRUE, num.trees=19000)
# predictions_1 = predict(RF_model_1, data=Test)$predictions
# predicted_Test_1 = data.frame(ID = row.names(Test), predicted_yield = predictions_1)
# predicted_Test_1 = predicted_Test_1[order(predicted_Test_1$predicted_yield, decreasing=TRUE),]
# selected_individuals_1 = predicted_Test_1[1:5,1]
# 
# set.seed(321)
# RF_model_2 = ranger(y=Training[,1], x=Training[,-1],
#                     write.forest = TRUE, num.trees=19000)
# predictions_2 = predict(RF_model_2, data=Test)$predictions
# predicted_Test_2 = data.frame(ID = row.names(Test), predicted_yield = predictions_2)
# predicted_Test_2 = predicted_Test_2[order(predicted_Test_2$predicted_yield, decreasing=TRUE),]
# selected_individuals_2 = predicted_Test_2[1:5,1]
# 
# M = merge(predicted_Test_1, predicted_Test_2, by="ID")

## ----fig.width=6, fig.height=4.5, fig.align='center'--------------------------
plot(M$predicted_yield.x, M$predicted_yield.y,
     xlab="Predicted yield in the first run", ylab="Predicted yield in the second run")
cor(M$predicted_yield.x, M$predicted_yield.y)

## ----message=FALSE------------------------------------------------------------
selected_individuals_1
selected_individuals_2

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123) # Set a seed for reproducibility
# optRF_result_2 = opt_prediction(y=Training[,1], X=Training[,-1], X_Test=Test,
#                                 alpha=0.1, recommendation="selection")

## ----message=FALSE------------------------------------------------------------
summary(optRF_result_2)

## ----fig.width=6, fig.height=4.5, fig.align='center'--------------------------
plot_stability(optRF_result_2, measure="selection", from=0, to=200000)

## ----message=FALSE------------------------------------------------------------
estimate_numtrees(optRF_result_2, measure="selection", for_stability=0.9)
estimate_numtrees(optRF_result_2, measure="selection", for_stability=0.95)
estimate_numtrees(optRF_result_2, measure="selection", for_stability=0.99)

## ----message=FALSE------------------------------------------------------------
estimate_stability(optRF_result_2, with_num.trees=250000)

