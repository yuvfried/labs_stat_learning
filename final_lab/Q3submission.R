# Generate objects

# a
my_prediction_model <- function(test_data){
  # imports
  require(dplyr)
  require(xgboost)
  
  # functions for pre-processisng
  impute_mean <- function(data, cols_to_impute){
    vals_to_impute <- colMeans(data[cols_to_impute],na.rm=TRUE)
    for (col in cols_to_impute){
      data[col][is.na(data[col])] <- vals_to_impute[col]
    }
    return(data)
  }
  
  convert_to_dummies <- function(data, cat_name){
    one_hot <- model.matrix(~data[[cat_name]])[,-1]
    n_minus_one <- length(colnames(one_hot))
    colnames(one_hot) <- paste(rep(cat_name,n_minus_one),1:n_minus_one,sep="")
    data <- data[ , -which(names(data) %in% c(cat_name))]
    data <- cbind(one_hot, data)
    return(data)
  }
  
  zero_one_scaling <- function(vec){
    mini <- min(vec)
    maxi <- max(vec)
    return(sapply(vec, function(x) (x-mini)/(maxi-mini)))
  }
  
  raw_data <- read.csv("pisa_israel_2018_fortargil.csv")
  # deepcopy of raw_data
  data <- data.frame(raw_data)
  
  # apply preproccesing to training data
  cols_to_impute <- names(which(colSums(is.na(data))>0))
  data <- impute_mean(data,cols_to_impute)
  data$LANGN <- as.character(data$LANGN)
  data <- convert_to_dummies(data,"LANGN")
  data <- convert_to_dummies(data,"System")
  data[,13:length(colnames(data))] <- apply(
    data[,13:length(colnames(data))],2,zero_one_scaling)
  
  train_X <- as.matrix(select(data, -PVREAD))
  
  # train the model as discussed in the Rmd file
  params <- list(
    eta = 0.1,
    max_depth = 3,
    objective = 'reg:squarederror'
  )
  
  model <- xgboost(
    params = params,
    data = train_X,
    label = data$PVREAD,
    nrounds = 104,
    verbose = 0
  )
  
  # apply pre-processing for test data
  cols_to_impute <- names(which(colSums(is.na(test_data))>0))
  test_data <- impute_mean(test_data,cols_to_impute)
  test_data$LANGN <- as.character(test_data$LANGN)
  test_data <- convert_to_dummies(test_data,"LANGN")
  test_data <- convert_to_dummies(test_data,"System")
  test_data[,12:length(colnames(test_data))] <- apply(
    test_data[,12:length(colnames(test_data))],2,zero_one_scaling)
  
  # prediction
  test_X <- as.matrix(test_data)
  preds <- predict(model, test_X)
  
  return(preds)
}

# b
predicted_accuracy <- 80.92761


# c
my_id <- "204169320"

# save objects
save(my_prediction_model, predicted_accuracy, my_id, "Q3submission.RData")