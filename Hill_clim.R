library(dplyr)
library(gRbase)
library(RGraphics) 
library(gRain)
library(bnlearn)
library(igraph)
library(tidyr)
library(graph)
# Step 1: Learn the structure of the Bayesian Network using Hill-Climbing algorithm
fit_on_train_hill= hc(train_data)
# Step 2: Visualize the learned Bayesian Network structure
viewer(fit_on_train_hill,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Discrete Bayesian Network - ",
       bayesianNetwork.subtitle = "Monitoring of emergency care patients",
       bayesianNetwork.footer = "Fig. 1 - Layout with Sugiyama")

)
# Step 3: Fit the parameters to the learned structure using Bayesian estimation
# 'iss = 1' is the imaginary sample size (a smoothing parameter)
fit_params_hill <- bn.fit(fit_on_train_hill, train_data, method = "bayes", iss = 1)

# Step 4: Convert the fitted Bayesian Network to a gRain object for inference
# Internally, this also performs moralization, triangulation, and builds the junction tree
grain_hill <- as.grain(fit_params_hill)

# Step 5: Perform probability propagation (message passing) in the junction tree
grain_hill_prop <- propagate(grain_hill)

# Step 6: Predict the value of 'readmitted' for each instance in the test dataset
predic_hill=predict(grain_hill_prop, response = "readmitted", newdata = test_data)

# Step 7: Create a data frame for comparing predicted and actual values
datafram_for_confusion=data.frame(predic_hill$pred,test_data$readmitted)

table(datafram_for_confusion)
# Step 9: Evaluate prediction performance using a confusion matrix
library(caret)
confusionMatrix(as.factor(datafram_for_confusion$readmitted), as.factor(datafram_for_confusion$test_data.readmitted))

# Step 10: Save the fitted model to an RDS file for future use
saveRDS(fit_params_hill, file = "D:/project_esame_aml/fit_params_hill.rds")
