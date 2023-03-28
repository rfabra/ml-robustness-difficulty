eval_metric <- list(
  "Kappa" = function(data, reference) {
    return(confusionMatrix(data=data,
                           reference = reference)$overall[["Kappa"]])
  },
  "Accuracy" = function(data, reference) {
    return(confusionMatrix(data=data,
                           reference = reference)$overall[["Accuracy"]])
  },
  "F1" = function(data, reference) {
    conf_mat <- confusionMatrix(data=data,
                                reference=reference, mode="everything")$byClass
    if (length(levels(reference)) > 2) {
      return(mean(conf_mat[,"F1"]))
    }
    else {
      return(conf_mat[["F1"]])
    }
  },
  "Global.Kappa" = function(data, reference, chance_prob) {
    return(my_kappa(data, reference, chance_prob))
  }
)