library(caret)
library(RWeka)

predict_test <- function(model,
                         model_name,
                         test, 
                         train,
                         pred_levels = levels(train$Class),
                         predictions_path = NULL, 
                         mylogger = NULL) {
  
  predictions <- load_object(predictions_path, 
                             sprintf("%s predictions", model_name), 
                             mylogger)
  if (!is.null(predictions)) {
    return(predictions)
  }
  
  log_info_message(sprintf("Predicting with model %s...", model_name), mylogger)
  
  if (model_name=="C5.0") {
    load_install("C50")
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:C50", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "RF") {
    load_install("randomForest")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:randomForest", unload=TRUE), error = function(e){print(e)})
  } 
  else if (model_name == "1-NN") {
    predictions <- predict(model, newdata = test)
  }
  else if (model_name == "knn_k3") { 
    predictions <- predict(model, newdata = test)
  }
  else if (model_name == "JRip"){
    load_install("RWeka")
    # preProc <- preProcess(train, method=c("center", "scale"))
    # test <- predict(preProc, test)
    # 
    # # NOTE: This is if fails to load the trained model 
    model <- learn_model(model_name = model_name,
                         train = train,
                         mylogger = mylogger)
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:RWeka", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "SVM") {
    load_install("kernlab")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:kernlab", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "NB") {
    load_install("naivebayes")
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:naivebayes", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "FDA_prune9") {
    load_install("mda")
    
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:mda", unload=TRUE), error = function(e){print(e)})
  } 
  else if (model_name == "NNET") {
    load_install("nnet")
    
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:nnet", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "ADA") {
    load_install("fastAdaboost")
    
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:fastAdaboost", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "RPART") {
    load_install("rpart")
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    
    raw_predictions <- predict(model, newdata = test)
    predictions <-  factor(as.vector(apply(raw_predictions, 1, 
                                           function (x) {
                                             colnames(raw_predictions)[which.max(x)]
                                           })), 
                           levels=levels(as.factor(train$Class)))
    
    tryCatch(detach("package:rpart", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "simpls_ncomp3") { 
    load_install("pls")
    predictions <- predict(model, newdata = test); "OK"
    tryCatch(detach("package:pls", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "mlp_7") {
    load_install("RSNNS")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:RSNNS", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "multinom") {
    load_install("nnet")
    # model <- learn_model(model_name, train)
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:nnet", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "fda_prune17") {
    load_install("mda")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:mda", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rda") {
    load_install("klaR")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:klaR", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "lvq_3") {
    load_install("class")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:class", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "svmPoly_d_2_s_0.1") {
    load_install("kernlab")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:kernlab", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "gbm_3_50") {
    load_install("gbm")
    # test$Class <- factor(test$Class, levels=unique(c(levels(train$Class), levels(test$Class))))
    model <- caret::train(Class ~ ., data = train, preProc = c("center", "scale"),
                          method = "gbm",
                          tuneGrid=data.frame(n.trees = 50, interaction.depth = 3,
                                              shrinkage = 0.1 , n.minobsinnode = 2),
                          trControl = trainControl(method="none"))
    # train_back <- train
    # train$Class <- as.numeric(train$Class)
    # train$Class <- as.character(train$Class)
    # model <- gbm(train$Class ~.,
    #                 data = train,
    #                 n.trees = 50, 
    #                 interaction.depth = 3, 
    #                 shrinkage = 0.1 , 
    #                 n.minobsinnode = 2)
    # test$Class <- as.numeric(test$Class)
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:gbm", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "ctree_c0.05"){
    load_install("party")
    # NOTE: This is if fails to load the trained model 
    model <- learn_model(model_name = model_name,
                         train = train,
                         mylogger = mylogger)
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:party", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "PART"){
    load_install("RWeka")
    preProc <- preProcess(train, method=c("center", "scale"))
    test <- predict(preProc, test)
    model <- learn_model(model_name = model_name,
                         train = train,
                         mylogger = mylogger)
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:RWeka", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rf_mtry64") {
    load_install("randomForest")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:randomForest", unload=TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rfRules_mtry64") {
    load_install("inTrees")
    load_install("randomForest")
    predictions <- predict(model, newdata = test)
    
    tryCatch(detach("package:randomForest", unload = TRUE), error = function(e){print(e)})
    tryCatch(detach("package:inTrees", unload = TRUE), error = function(e){print(e)})
  }
  else if (model_name == "rbf") {
    load_install("RSNNS")
    predictions <- predict(model, newdata = test)
    tryCatch(detach("package:RSNNS", unload=TRUE), error = function(e){print(e)})
  }
  
  if (is.null(predictions)) {
    log_error_message(sprintf("The model %s could not be used to predict. Possible reasons:\n 
                              \t(1) You mistyped the model_name parameter (%s)", 
                              model_name, model_name), 
                      mylogger)
  }
  else {
    predictions <- factor(predictions, levels = pred_levels)
    save_object(predictions, predictions_path, sprintf("%s predictions", model_name), mylogger)
  }
  
  
  
  return(predictions)
  
}

predict_datasets <- function(datasets_data, models, paths, noise_magnitude, mylogger = NULL) {
  log_info_message("STAGE: Predictions", mylogger)
  log_info_message("----------------------", mylogger)
  for (dataset_name in names(datasets_data)) {
    log_info_message(paste("Predictions: Processing dataset", 
                           paste("<", dataset_name, ">", sep="")), mylogger)
    # Load dataset
    dataset_path <- file_path_dataset[["data"]](paths, dataset_name)
    dataset <- load_dataset(dataset_name, datasets_data[[dataset_name]],
                            dataset_path = dataset_path)
    # Start 5-cross validation
    set.seed(123)
    folds <- create_folds(dataset$Class, k=5)
    i_fold <- 0
    for(raw_train_fold in folds) {
      i_fold <- i_fold + 1
      # Create train-test folds
      train_fold <- rownames(dataset[raw_train_fold,])
      test_fold <- rownames(dataset[-raw_train_fold,])
      data_train <- dataset[train_fold,]
      data_test <- dataset[test_fold,]
      # Save row id
      train_row_id <- data_train$row_id
      test_row_id <- data_test$row_id
      # Select train-test examples
      data_train <- dplyr::select(data_train, -row_id)
      data_test <- dplyr::select(data_test, -row_id)
      # Remove 0 variance features
      data_train <- remove_constant_columns(data_train)
      # Load noisy test
      path_noisy_dataset <- file_path_noisy_dataset[["data"]](paths, dataset_name, i_fold, 
                                                              noise_magnitude, 1.0)
      data_test_noisy <- load_object(path_noisy_dataset,
                                     object="Noisy dataset", 
                                     mylogger = mylogger)
      for (model_name in names(models)) {
        log_info_message(sprintf("Model %s", model_name), mylogger)
        path_lock <- file_path_locks(paths, dataset_name, 
                                     paste("models", i_fold, model_name, sep="-"))
        if (!lock_process(path_lock, mylogger)) {
          next
        }
        # Remove factor levels without instances in the training set
        data_train$Class <- factor(data_train$Class, 
                                   levels=unique(data_train$Class))
        # Load model
        model_path <- file_path_model[["data"]](paths, dataset_name, model_name,
                                                i_fold)
        model <- load_object(model_path, paste("Model", model_name), mylogger)
        # Perform predictions
        preds_path <- file_path_predictions[["data"]](paths, dataset_name, 
                                                      model_name, 0, i_fold)
        tryCatch({
          predictions_original <- predict_test(model, model_name, data_test,
                                               data_train, 
                                               predictions_path = preds_path,
                                               mylogger = mylogger)
        }, error = function(e) {
          log_info_message(paste("Error when computing predictions for (original) dataset", 
                                 paste("<", dataset_name, ">:", sep = "")), 
                           mylogger);
          log_info_message(e, mylogger)
        })
        preds_path <- file_path_predictions[["data"]](paths, dataset_name, 
                                                      model_name, 1.0, i_fold)
        tryCatch({
          predictions_noisy <- predict_test(model, model_name, data_test_noisy,
                                            data_train, 
                                            predictions_path = preds_path,
                                            mylogger = mylogger)
        }, error = function(e) {
          log_info_message(paste("Error when computing predictions for (noisy) dataset", 
                                 paste("<", dataset_name, ">:", sep = "")), 
                           mylogger);
          log_info_message(e, mylogger)
        })
        unlock_process(path_lock, mylogger)
      }
    }
    log_info_message(paste("PREDICTIONS: end dataset", 
                           paste("<", dataset_name, ">", sep = "")), mylogger)
  }
  return(TRUE)
}