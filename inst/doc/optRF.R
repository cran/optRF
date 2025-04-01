## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ranger)
library(optRF)
SNPdata[1:5, 1:5]

## ----echo=FALSE, message=FALSE------------------------------------------------
load("optRF_vignette_predData.Rda")

## ----message=FALSE------------------------------------------------------------
Training = SNPdata[1:200,] # Rows 1 to 200 as training data
Test = SNPdata[201:250,-1] # Rows 201 to 250 as test data, excluding the response column (column 1)

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123) # Set a seed for reproducibility
# optRF_result = opt_prediction(y=Training[,1], X=Training[,-1],
#                               X_Test=Test, alpha=0.1)

## ----message=FALSE------------------------------------------------------------
summary(optRF_result)

## ----eval=FALSE, message=FALSE------------------------------------------------
# RF_model = ranger(y=Training[,1], x=Training[,-1],
#                   write.forest = TRUE, num.trees=optRF_result$recommendation)
# predictions = predict(RF_model, data=Test)$predictions
# predicted_Test_data = data.frame(ID = row.names(Test), predicted_response = predictions)

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123) # Set a seed for reproducibility
# optRF_result_2 = opt_prediction(y=Training[,1], X=Training[,-1], X_Test=Test,
#                                 alpha=0.1, recommendation="selection")

## ----message=FALSE------------------------------------------------------------
summary(optRF_result_2)

## ----echo=FALSE, message=FALSE------------------------------------------------
load("optRF_vignette_impData.Rda")

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123) # Set a seed for reproducibility
# optRF_result = opt_importance(y=SNPdata[,1], X=SNPdata[,-1])

## ----message=FALSE------------------------------------------------------------
summary(optRF_result)

## ----eval=FALSE, message=FALSE------------------------------------------------
# RF_model = ranger(y=SNPdata[,1], x=SNPdata[,-1], num.trees=optRF_result$recommendation,
#                   write.forest = TRUE, importance="permutation")
# D_VI = data.frame(variable = names(SNPdata)[-1],
#                   importance = RF_model$variable.importance)

## ----fig.width=6, fig.height=4.5, fig.align='center'--------------------------
hist(D_VI$importance, xlim=c(-10, 50), 
     main="Histogram of variable importances", xlab="")

## ----message=FALSE------------------------------------------------------------
selection_size = sum(RF_model$variable.importance>5)

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123) # Set a seed for reproducibility
# optRF_result_2 = opt_importance(y=SNPdata[,1], X=SNPdata[,-1],
#                                 recommendation = "selection",
#                                 alpha = selection_size)

## ----eval=FALSE, message=FALSE------------------------------------------------
# RF_model_2 = ranger(y=SNPdata[,1], x=SNPdata[,-1], num.trees=optRF_result_2$recommendation,
#                     write.forest = TRUE, importance="permutation")
# D_VI_2 = data.frame(variable = names(SNPdata)[-1],
#                     importance = RF_model_2$variable.importance)
# D_VI_2 = D_VI_2[order(D_VI_2$importance, decreasing=TRUE),]
# selected_variables = D_VI_2[1:selection_size,1]

## ----message=FALSE------------------------------------------------------------
summary(optRF_result_2)

## ----echo=FALSE, message=FALSE------------------------------------------------
load("optRF_vignette_stabilityData.Rda")

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123)
# stability_prediction = measure_stability(y = Training[,1], X=Training[,-1], X_Test=Test, num.trees=5000, method="prediction")

## ----message=FALSE------------------------------------------------------------
stability_prediction

## ----eval=FALSE, message=FALSE------------------------------------------------
# set.seed(123)
# stability_importance = measure_stability(y = Training[,1], X=Training[,-1], X_Test=Test, num.trees=5000, method="importance")

## ----message=FALSE------------------------------------------------------------
stability_importance

