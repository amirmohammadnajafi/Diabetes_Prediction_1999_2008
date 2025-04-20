library(dplyr)
library(gRbase)
library(RGraphics) 
library(gRain)
library(bnlearn)
library(igraph)
library(tidyr)
library(graph)
library(bnstruct)
library(deal)
library(pcalg)
library(bnviewer)
"preprossesing"
"drop null value"
data=read.csv("E:/project_esame_aml/data.csv")
sum(is.na(data))
data_clean=data
data_clean[data_clean == "?"] <- NA
summary(data_clean)
sort(colSums(is.na(data_clean)))
"""readmitted                   diag_1 
                       0                       21 
                  diag_2                   diag_3 
                     358                     1423 
                    race               payer_code 
                    2273                    40256 
       medical_specialty                   weight 
                   49949                    98569 
so we are going to drop columns 
[weight,medical_specialt,payer_code.race]
and drop row with null value of the columns[ diag_2,diag_3,diag_1]
                   """

library(dplyr)
data_clean=select(data_clean, -c(weight, medical_specialty, race, payer_code))
data_clean=na.omit(data_clean)
colSums(is.na(data_clean))



sum(data_clean$gender == "Unknown/Invalid")
data_clean <- data_clean %>% filter(gender != "Unknown/Invalid")

sum(data$readmitted =="NO")
sum(data$readmitted ==">30")
sum(data$readmitted =="<30")

data=data_clean
fix(data)
data_senza_numbrecantinuos=data %>% select(-diag_3, -encounter_id,-num_lab_procedures,-num_medications,-diag_1
                                           ,-diag_2)



data=read.csv("D:\\project_esame_aml\\data_cleaned_and categurialed.csv")
data=subset(data,select = -c(patient_nbr,examide,citoglipton,glimepiride.pioglitazone,metformin.rosiglitazone))
data <- subset(data, gender != "Unknown/Invalid")
column=names(data)
data <- data.frame(lapply(data, as.factor))
str(data)
set.seed(1)
fix(data)
train <- sample(1:nrow(data),nrow(data)*0.65)
train_data=data[train,]
str(train_data)
test_data=data[-train,]

train_data <- droplevels(train_data)
# Grow-Shrink Algorithm: Learn the structure of the Bayesian Network
fit_gs = gs(train_data)


#Display the DAG structure

viewer(fit_gs,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Discrete Bayesian Network - ",
       bayesianNetwork.subtitle = "Monitoring of emergency care patients",
       bayesianNetwork.footer = "Fig. 1 - Layout with Sugiyama"
)

# ------------------------------------------
# Step 2: Extract the Markov Blanket for the target variable

mb_nodes = mb(fit_gs, node = "readmitted")

# Step 3: Add the target variable itself to the subgraph node list
mb_subgraph_nodes = c("readmitted", mb_nodes)

# ------------------------------------------
# Step 4: Extract the full adjacency matrix from the learned DAG
amat_full = amat(fit_gs)

# Filter the adjacency matrix to keep only Markov Blanket nodes
amat_mb = amat_full[mb_subgraph_nodes, mb_subgraph_nodes]

# ------------------------------------------
# 5. Create a Bayesian Network object using the submatrix
class(amat_mb) = "matrix"
mb_bn = empty.graph(nodes = mb_subgraph_nodes)
amat(mb_bn) = amat_mb

#Convert the partial graph into a fully directed acyclic graph (if necessary)
mb_bn_dag = cextend(mb_bn)

# ------------------------------------------
#Step 6: Moralization
#
mb_bn_ug = moral(mb_bn_dag)  

#Convert to an adjacency matrix for plotting or further processing
amat_moral = amat(mb_bn_ug)

#Plot the moralized graph using igraph
library(igraph)
g_moral = graph.adjacency(amat_moral, mode = "undirected")

plot(g_moral,
     vertex.label.cex = 0.8,
     vertex.size = 25,
     edge.color = "gray30",
     vertex.color = "lightblue",
     main = "Moralized Graph")


# ------------------------------------------
#step 7 :Triangulation is assumed to be done
"""
g_moral_undirected = ug(amat_moral)

triangulated_graph = triangulate(g_moral_undirected)
"""
# step 8 :Build the junction tree (clique tree)
junction_tree = rip(g_moral)

# ------------------------------------------
# Step 9: Fit the parameters of the subnetwork using training data
train_data_mb = train_data[, mb_subgraph_nodes]
fit_mb_bn = bn.fit(mb_bn_dag, train_data_mb)

# ------------------------------------------
# Step 10: Convert the fitted BN to a gRain object for inference
grain_mb = as.grain(fit_mb_bn)

# propagate برای استنتاج
grain_mb_propagated = propagate(grain_mb)

# ------------------------------------------
# Step 11: Display a summary of the propagated network
summary(grain_mb_propagated)
# Extract only Markov Blanket variables from test data
test_data_mb = test_data[, mb_subgraph_nodes]

#Predict 'readmitted' for each test sample
preds = predict(grain_mb_propagated, response = "readmitted", newdata = test_data_mb)

# Actual values
real = test_data_mb$readmitted 

# Combine real and predicted values into a comparison table
comparison = data.frame(real, preds)

# Evaluate performance using confusion matrix
library(caret)
confusionMatrix(as.factor(comparison$readmitted), as.factor(comparison$real))
