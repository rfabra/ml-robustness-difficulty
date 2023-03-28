check_option <- function(data, values) {
  if (!data %in% values){
    stop(paste("Values must be one of:\n", paste(values, collapse= ", "), collapse = " "))
  }
}

load_install <- function(lib_name)
{
  if ((lib_name %in% installed.packages()) == FALSE) 
  {
    install.packages(lib_name)
  }
  require(lib_name, character.only = TRUE)
}

remove_constant_columns <- function(in_data) {
  # Remove 0 variance features
  idx_constant_columns = apply(in_data, 2, FUN = function(x) {all(duplicated(x)[-1L])})
  return(in_data[,!idx_constant_columns])
}

normalize_instance_names <- function(current_names) {
  return(paste("Inst", current_names, sep="."))
}

normalize_model_names <- function(data, models_data) {
  return( 
    sapply(data, FUN = function(x) {
      return(models_data[[x]])
    }))
}

train_test_split <- function(dataset, dataset_split_percent, myseed=123) {
  set.seed(myseed)
  trainIndex <- createDataPartition(dataset$Class, p=dataset_split_percent, list=FALSE)
  data_train <- dataset[trainIndex,]
  data_test <- dataset[-trainIndex,]
  
  return(list("train"=data_train, "test"=data_test))
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

remove_0_var_feats <- function(remaining_feats) {
  # Remove 0 variance features
  var0_feats_num = apply(remaining_feats, 2, var, na.rm=TRUE) %in%c(0)
  remaining_feats <- remaining_feats[,!var0_feats_num]
  var0_feats_disc = apply(remaining_feats, 2, function(x){length(unique(x))==1})
  remaining_feats <- remaining_feats[,!var0_feats_disc]
  
  return(remaining_feats)  
get_class_distribution <- list(
  "Class" = function(dataset_labels) {
    class_distribution <- dataset_labels %>%
      dplyr::group_by(Class) %>%
      dplyr::summarise(
        Count = length(Class)
      )
    return(class_distribution)
  },
  "Real" = function(predictions_per_bin) {
    real_class_count <- predictions_per_bin %>%
      dplyr::group_by(Noisy.Instances, Model, Bin, Num.Bin, Real) %>%
      dplyr::summarise(
        Real.Count = length(Real)
      )
    return(real_class_count)
  }, # Esta crec que sobra
  "Predicted" = function(predictions_per_bin) {
    real_class_count <- predictions_per_bin %>%
      dplyr::group_by(Noisy.Instances, Model, Bin, Num.Bin, Predicted) %>%
      dplyr::summarise(
        Predicted.Count = length(Predicted)
      )
  },
  "Predicted.Noisy" = function(predictions_per_bin) {
    predicted_class_count <- predictions_per_bin %>%
      dplyr::group_by(Noisy.Instances, Model, Bin, Num.Bin, Predicted.Noisy) %>%
      dplyr::summarise(
        Predicted.Noisy.Count = length(Predicted.Noisy)
      )
    return(predicted_class_count)
  }
)
