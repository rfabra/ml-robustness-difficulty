library(caret)
library(RWeka)

learn_model <- function(model_name, 
                        train, 
                        model_path = NULL, 
                        mylogger = NULL) {
  
  model <- load_object(model_path, 
                       sprintf("%s model", model_name), 
                       mylogger)
  if (!is.null(model)) {
    return(model)
  }
  
  log_info_message(sprintf("Learning model %s...", model_name), mylogger)
  
  if (model_name=="C5.0") {
    load_install("C50")
    preProc <- preProcess(train, method=c("center", "scale"))
    train <- predict(preProc, train)
    model <- C5.0(Class ~., data = train)
    
    tryCatch(detach("package:C50", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "RF") {
    load_install("randomForest")
    model <- caret::train(Class ~ ., 
                          data = train, 
                          preProc = c("center", "scale"), 
                          method = "rf",
                          # tuneGrid=data.frame(mtry = 64),
                          trControl = trainControl(method="none"))    
    
    tryCatch(detach("package:randomForest", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "1-NN") { 
    model <- caret::train(Class ~ ., 
                          data = train, 
                          preProc = c("center", "scale"), 
                          method = "knn",
                          tuneGrid=data.frame(k = 1),  
                          trControl = trainControl(method="none"))
  }
  else if (model_name == "knn_k3") { 
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "knn",tuneGrid=data.frame(k = 3),  
                          trControl = trainControl(method="none"))
  }
  else if (model_name=="JRip"){
    load_install("RWeka")
    # preProc <- preProcess(train, method=c("center", "scale"))
    # train <- predict(preProc, train)
    # model <-JRip(Class ~., data = train)
    model <- caret::train(Class ~ ., data = train, 
                          preProc = c("center", "scale"), 
                          method = "JRip", 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:RWeka", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "SVM") {
    load_install("kernlab")
    model <- caret::train(Class ~ ., 
                          data = train, 
                          preProc = c("center", "scale"), 
                          method = "svmRadialCost", 
                          # tuneGrid=data.frame(C=2^5), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:kernlab", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "NB") {
    load_install("naivebayes")
    
    preProc <- preProcess(train, method=c("center", "scale"))
    train <- predict(preProc, train)
    model <- naive_bayes(train[,-ncol(train)], train[,ncol(train)])
    
    tryCatch(detach("package:naivebayes", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "FDA_prune9") {
    load_install("mda")
    model <- caret::train(Class ~ ., data = train, 
                          preProc = c("center", "scale"), 
                          method = "fda", 
                          tuneGrid=data.frame(nprune = 9, degree = 1), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:mda", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "NNET") {
    load_install("nnet")
    
    model <- caret::train(Class~ .,
                          data=train,
                          method="nnet")
    # metric="Rsquared",
    # trControl=ctrl,
    # tuneGrid=t.grid)
    
    tryCatch(detach("package:nnet", unload=TRUE), error = function(e){print(e)})
    
  }
  else if (model_name == "ADA") {
    load_install("fastAdaboost")
    model <- caret::train(Class~ .,
                          data=train,
                          method="adaboost")
    
    tryCatch(detach("package:fastAdaboost", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "RPART") {
    load_install("rpart")
    preProc <- preProcess(train, method=c("center", "scale"))
    train <- predict(preProc, train)
    
    model <- rpart(Class ~., data = train, method = "class")
    
    tryCatch(detach("package:rpart", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "simpls_ncomp3") { 
    load_install("pls")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "simpls",
                          tuneGrid=data.frame(ncomp = 3),  
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:pls", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "mlpWeightDecay_1e05") {
    load_install("RSNNS")
    model <- caret::train(
      Class ~ ., data = train, 
      preProc = c("center", "scale"), 
      method = "mlpWeightDecay", 
      tuneGrid=data.frame(size=5, decay = 1e-05))
    
    tryCatch(detach("package:RSNNS", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "mlp_7") {
    load_install("RSNNS")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), method = "mlp", tuneGrid=data.frame(size=7), trControl = trainControl(method="none"))
    
    tryCatch(detach("package:RSNNS", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "multinom") {
    load_install("nnet")
    preProc <- preProcess(train, method=c("center", "scale"))
    train <- predict(preProc, train)
    
    # model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), method = "multinom", 
    #                       trControl = trainControl(method="none", MaxNWts = 3000))
    
    model <- multinom(Class ~ ., data = train, MaxNWts = 30000)
    # model <- multinom(Class ~ ., data = train)
    tryCatch(detach("package:nnet", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "fda_prune17") {
    load_install("mda")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "fda", 
                          tuneGrid=data.frame(nprune = 17, degree = 1), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:mda", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rda") {
    load_install("klaR")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), method = "rda")
    
    tryCatch(detach("package:klaR", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "lvq_3") {
    load_install("class")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "lvq", tuneGrid=data.frame(size=50, k = 3), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:class", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "svmPoly_d_2_s_0.1") {
    load_install("kernlab")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "svmPoly", tuneGrid=data.frame(degree=2, scale= 0.1, C = 1), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:kernlab", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "gbm_3_50") {
    load_install("gbm")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "gbm", 
                          tuneGrid=data.frame(n.trees = 50, interaction.depth = 3, 
                                              shrinkage = 0.1 , n.minobsinnode = 2), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:gbm", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "ctree_c0.05"){
    load_install("party")
    
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "ctree", 
                          tuneGrid=data.frame(mincriterion=0.5), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:party", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "PART"){
    load_install("RWeka")
    preProc <- preProcess(train, method=c("center", "scale"))
    # train <- predict(preProc, train)
    # model <- PART(Class ~., data = train)
    model <- caret::train(Class ~ ., data = train, 
                          preProc = c("center", "scale"), 
                          method = "PART", 
                          trControl = trainControl(method="none"))
    tryCatch(detach("package:RWeka", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rf_mtry64") {
    load_install("randomForest")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "rf",tuneGrid=data.frame(mtry = 64),  
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:randomForest", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rfRules_mtry64") {
    load_install("inTrees")
    load_install("randomForest")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "rfRules", tuneGrid=data.frame(mtry = 64, maxdepth=2),  
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:randomForest", unload = TRUE), error = function(e){print(e)})
    tryCatch(detach("package:inTrees", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rbf") {
    load_install("RSNNS")
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"), 
                          method = "rbfDDA", tuneGrid=data.frame(negativeThreshold=0.001), 
                          trControl = trainControl(method="none"))
    
    tryCatch(detach("package:RSNNS", unload=TRUE), error = function(e){print(e)})
  }
  
  
  if (is.null(model)) {
    log_error_message(sprintf("The model %s could not be trained. Possible reasons:\n 
                              \t(1) You mistyped the model_name parameter (%s)", 
                              model_name, model_name), 
                      mylogger)
  } 
  else {
    save_object(model, model_path, sprintf("%s model", model_name), mylogger)
  }
  
  return(model)
}

train_models_datasets <- function(datasets_data, models, paths, mylogger = NULL) {
  log_info_message("STAGE: Training models", mylogger)
  log_info_message("----------------------", mylogger)
  # Iterate datasets
  for (dataset_name in names(datasets_data)) {
    log_info_message(paste("Training models: Processing dataset", 
                           paste("<", dataset_name, ">", sep="")), mylogger)
    # Load dataset
    dataset_path <- file_path_dataset[["data"]](paths, dataset_name)
    dataset <- load_dataset(dataset_name, datasets_data[[task_id]],
                            dataset_path = dataset_path)
    # Start 5-cross validation
    set.seed(123)
    folds <- create_folds(dataset$Class, k=5)
    i_fold <- 0
    # Iterate folds
    for(raw_train_fold in folds) {
      i_fold <- i_fold + 1
      # Train-test splits
      train_fold <- rownames(dataset[raw_train_fold,])
      test_fold <- rownames(dataset[-raw_train_fold,])
      data_train <- dataset[train_fold,]
      data_test <- dataset[test_fold,]
      # Save instances id      
      train_row_id <- data_train$row_id
      test_row_id <- data_test$row_id
      # Perform split
      data_train <- dplyr::select(data_train, -row_id)
      data_test <- dplyr::select(data_test, -row_id)
      # Iterate models
      for (model_name in names(models)) {
        log_info_message(sprintf("Model %s", model_name), mylogger)
        # Remove factor levels without instances in the training set
        data_train$Class <- factor(data_train$Class, 
                                   levels=unique(data_train$Class))
        # Remove 0 variance features
        data_train <- remove_constant_columns(data_train)
        model_path <- file_path_model[["data"]](paths, dataset_name, model_name,
                                                i_fold)
        # Lock process
        path_lock <- file_path_locks(paths, dataset_name, 
                                     paste("models", i_fold, model_name, sep=""))
        # Learn model
        if (lock_process(path_lock, mylogger)) {
          tryCatch(
            model <- learn_model(model_name, data_train,
                                 model_path = model_path, mylogger = mylogger),
            
            error = function(e) {
              log_info_message(paste("Error when learning models for dataset", 
                                     paste("<", dataset_name, ">:", sep = "")), 
                               mylogger);
              log_info_message(e, mylogger)
            })
          unlock_process(path_lock, mylogger)
        }
      }
    }
    log_info_message(paste("IRT: end dataset", 
                           paste("<", dataset_name, ">", sep = "")), mylogger)
  }
  return(TRUE)
}