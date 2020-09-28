#Load the data of nanoparticles with 9 features
#make sure the data is in the form of matrix with rows as samples and columns as features
#Normalize the features of the loaded data (represented here as New_data) using the normalization methods described separately below
#Load the rds models of your choice

source("normalise.R")
source("AD.R")

new_data <- normalise(new_data)

#### load the models  
Logistic <- readRDS("model_log.rds")
RF <- readRDS("model_rf.rds")
SVM_linear <- readRDS("model_SVM_linear.rds")
NNet1 <- readRDS("model_neuralNet_1layer.rds")
MLP2 <- readRDS("model_neuralNet_2layer.rds")

### Check applicability domain for each instance and predict

for (i in 1:nrow(new_data)) {
  
  instance_applicability <- applicability(new_data[i,])
  
  if (!instance_applicability) {
    #stop("Instance exceeds Applicability domain. Halting predictions...")
    print(paste("Current instance exceeds Applicability domain: instance # ", i, ". Skipping to next instance..."))
    next
  }
  else {
    print(paste("Instance within Applicability domain: instance # ", i, ". Proceeding to prediction..."))
  }
  
  outcomes <- c(predict(RF,new_data[i,]), predict(NNet1,new_data[i,]))
  
  ens = ifelse(outcomes == 2 | outcomes == 0, 1, 0)

  if (ens) {
    print("The top two models agree in their predictions.")
    if (sum(outcomes>1)) print("The consensus prediction is Toxicity")
    else print("The consensus prediction is Absence of toxicity.")
  }
  
  else {
    print("There is no consensus among the top two models.")
    outcomes_full <- c(predict(Logistic, new_data[i,]), predict(RF,new_data[i,]), predict(SVM_linear, new_data[i,]), predict(NNet1,new_data[i,]), predict(MLP2, new_data[i,]))
    maj_pred = ifelse(sum(outcomes_full) > 2, 1, 0)
    
    if (maj_pred) {
      print(paste("There is majority prediction for Toxicity from", maj_pred, "classifiers"))
    }
    else {
      print(paste("There is majority prediction for Absence of toxicity from", 5-maj_pred, "classifiers"))
    }
  }
  
}
