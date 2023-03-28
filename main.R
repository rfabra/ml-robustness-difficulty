setwd("~/Projects/family-robustness/")
source("src/startup/config_setup.R")

# Load config file
# config <- config_setup("Config file (for development)", "config/develop.json")
config <- config_setup("Config file", "config/config.json")
# config <- config_setup("Config file", "config/single_scc.json")
# config <- config_setup("Config file", "config/config_big_instances.json")
# config <- config_setup("Config file", "config/config_single_dataset.json")

# Experiments -------------------------------------------------------------

if (config$experiments$difficulty$response_matrix$active) {
  compute_datasets_response_matrix(
    config$datasets, config$paths, 
    config$experiments$difficulty$response_matrix$num_models, 
    mylogger = config$logger)
}
if (config$experiments$difficulty$irt$active) {
  compute_datasets_IRT(config$datasets, config$paths,
                       config$experiments$difficulty$response_matrix$num_models,
                       config$experiments$difficulty$irt,
                       mylogger = config$logger)
}
if (config$experiments$noisy_datasets$active) {
  generate_noisy_datasets(config$datasets, config$paths, 
                          config$experiments$noisy_datasets$magnitude,
                          mylogger = config$logger)
}
if (config$experiments$training$active) {
  train_models_datasets(config$datasets, config$models, config$paths, 
                        mylogger = config$logger)
}
if (config$experiments$predictions$active) {
  predict_datasets(config$datasets, config$models, config$paths, 
                   config$experiments$noisy_datasets$magnitude, 
                   mylogger = config$logger)
}
if (config$experiments$predictions_per_bin$active) {
  predict_per_bin_datasets(config$datasets, config$models, config$paths,
                           config$experiments$difficulty$irt, 
                           config$experiments$noisy_datasets, 
                           mylogger = config$logger)
}
if (config$experiments$process_predictions$active) {
  process_predictions(names(config$datasets), names(config$models), 
                      config$paths, config$experiments$noisy_datasets$magnitude,
                      config$experiments$process_predictions$instance_proportions,
                      config$logger)
}
if (config$experiments$taxonomy$aggregate_curves$active) {
  aggregate_curves(
    names(config$datasets), 
    config$models,
    config$paths,
    config$experiments$taxonomy$datasets_sizes, 
    config$experiments$taxonomy$instance_proportions,
    config$experiments$taxonomy$aggregate_curves$methods, 
    config$logger)
  }
if (config$experiments$taxonomy$clustering$active) {
  perform_clustering(
    names(config$datasets), 
    names(config$models),
    config$paths,
    config$experiments$taxonomy$datasets_sizes, 
    config$experiments$taxonomy$instance_proportions,
    config$experiments$taxonomy$aggregate_curves$methods,
    config$experiments$taxonomy$clustering$type,
    config$experiments$taxonomy$clustering$num_clusters,
    config$logger)
}
if (config$experiments$taxonomy$cluster_prototypes$active) {
  get_cluster_prototypes(
    names(config$datasets), 
    names(config$models),
    config$paths,
    config$experiments$taxonomy$datasets_sizes, 
    config$experiments$taxonomy$instance_proportions,
    config$experiments$taxonomy$aggregate_curves$methods,
    config$experiments$taxonomy$clustering$type,
    config$experiments$taxonomy$clustering$num_clusters,
    config$logger)
}
# Analytics ---------------------------------------------------------------

if (config$analytics$datasets$active) {
  compute_datasets_stats(names(config$datasets), 
                         config$paths, 
                         mylogger = config$logger)
  dataset_class_distribution_analytics(names(config$datasets), 
                                       config$paths, 
                                       config$analytics$datasets$plot_params)
}
if (config$analytics$irt$difficulty$active) {
  analytics_difficulty(names(config$datasets),
                       config$paths,
                       config$experiments$difficulty$irt,
                       config$analytics$irt$difficulty$plot_params)
}
if (config$analytics$evaluations$active) {
  analytics_datasets_evaluation(names(config$datasets), config$models, config$paths,
                                config$experiments$noisy_datasets$magnitude, 
                                mylogger = config$logger)
}
if (config$analytics$curves$SCC_Kappa$active) {
  # TODO: provide datasets_sizes as parameter, and call only once
  # cnae-9, NB
  # letter, GBM
  # nursery, 3NN
  # config$datasets <- list("cnae-9" = 9981)
  # config$models <- list("NB" = "NB")
  # plot_single_scc(names(config$datasets), 
  #                        config$models,
  #                        config$paths,
  #                        "SCC_Kappa",
  #                        config$analytics$curves$SCC_Kappa$plot_params,
  #                        mylogger = config$logger)
  # 
  # config$datasets <- list("letter" = 1886)
  # config$models <- list("gbm_3_50" = "GBM")
  # plot_single_scc(names(config$datasets), 
  #                 config$models,
  #                 config$paths,
  #                 "SCC_Kappa",
  #                 config$analytics$curves$SCC_Kappa$plot_params,
  #                 mylogger = config$logger)
  # 
  # config$datasets <- list("nursery" = 26)
  # config$models <- list("knn_k3" = "3NN")
  # plot_single_scc(names(config$datasets), 
  #                 config$models,
  #                 config$paths,
  #                 "SCC_Kappa",
  #                 config$analytics$curves$SCC_Kappa$plot_params,
  #                 mylogger = config$logger)
  # 
  # config$datasets <- list("gina_prior2" = 3894)
  # config$models <- list("svmPoly_d_2_s_0.1" = "SVM")
  # plot_single_scc(names(config$datasets), 
  #                 config$models,
  #                 config$paths,
  #                 "SCC_Kappa",
  #                 config$analytics$curves$SCC_Kappa$plot_params,
  #                 mylogger = config$logger)
  # 
  
  config$datasets <- list("optdigits" = 1792)
  config$models <- list("knn_k3" = "3NN")
  plot_single_scc(names(config$datasets),
                  config$models,
                  config$paths,
                  "SCC_Kappa",
                  config$analytics$curves$SCC_Kappa$plot_params,
                  mylogger = config$logger)
  
  plot_robustness_curves(names(config$datasets), 
                         config$models,
                         config$paths,
                         "SCC_Kappa",
                         config$experiments$taxonomy$datasets_sizes,
                         config$analytics$curves$SCC_Kappa$plot_params,
                         mylogger = config$logger)
}
if (config$analytics$curves$SCC_Kappa_by_cluster$active) {
  
  plot_robustness_curves_by_cluster(names(config$datasets), 
                                    names(config$models),
                                    config$paths,
                                    "SCC_Kappa",
                                    config$analytics$curves$SCC_Kappa_by_cluster$plot_params,
                                    config$experiments$taxonomy$datasets_sizes,
                                    config$experiments$taxonomy$aggregate_curves$methods,
                                    config$experiments$taxonomy$clustering$type,
                                    config$experiments$taxonomy$clustering$num_clusters,
                                    mylogger = config$logger)
}
# TODO: change condition after changing config
if (config$analytics$curves$SCC_Kappa_by_cluster$active) {
  
  plot_robustness_curves_by_prototype(names(config$datasets), 
                                    config$models,
                                    config$paths,
                                    "SCC_Kappa",
                                    config$analytics$curves$SCC_Kappa_by_prototype$plot_params,
                                    config$experiments$taxonomy$datasets_sizes,
                                    config$experiments$taxonomy$aggregate_curves$methods,
                                    config$experiments$taxonomy$clustering$type,
                                    config$experiments$taxonomy$clustering$num_clusters,
                                    mylogger = config$logger)
}

if (config$analytics$curves$SCC_Acc$active) {
  plot_robustness_curves(names(config$datasets), 
                         names(config$models),
                         config$paths,
                         "SCC_Acc",
                         config$analytics$curves$SCC_Acc$plot_params,
                         mylogger = config$logger)
  
}
if (config$analytics$curves$CCC_Kappa$active) {
  plot_robustness_curves(names(config$datasets), 
                         names(config$models),
                         config$paths,
                         "CCC_Kappa",
                         config$analytics$curves$CCC_Kappa$plot_params,
                         mylogger = config$logger)
  
}
if (config$analytics$curves$CCC_Acc$active) {
  plot_robustness_curves(names(config$datasets),
                         names(config$models),
                         config$paths,
                         "CCC_Acc",
                         config$analytics$curves$CCC_Acc$plot_params,
                         mylogger = config$logger)
  
}
if (config$analytics$curves$SCC_avg$active) {
  plot_model_curves(names(config$datasets), 
                    names(config$models),
                    config$paths,
                    config$analytics$curves$SCC_avg$plot_params,
                    mylogger = config$logger)
  
}
if (config$analytics$curves$CCC$active) {
  plot_CCC_per_model(names(config$datasets),
                     names(config$models),
                     config$paths,
                     config$analytics$curves$CCC$plot_params,
                     mylogger = config$logger)
}
if (config$analytics$class_distributions$real$active) {
  class_distribution_analytics(names(config$datasets),
                               config$paths,
                               names(config$models),
                               "real",
                               config$analytics$class_distributions$real$plot_params)
}
if (config$analytics$class_distributions$predicted$active) {
  class_distribution_analytics(names(config$datasets),
                               config$paths,
                               names(config$models),
                               "predicted",
                               config$analytics$class_distributions$predicted$plot_params)
  
}
if (config$analytics$taxonomy$clustering$active) {
  plot_taxonomy(
    config$paths,
    config$models,
    config$experiments$taxonomy$datasets_sizes, 
    config$experiments$taxonomy$aggregate_curves$methods,
    config$experiments$taxonomy$clustering$type,
    config$experiments$taxonomy$clustering$num_clusters,
    config$analytics$taxonomy$clustering$plot_params,
    config$logger)
  # plot_taxonomy(config$paths, config$analytics$taxonomy$hierarchical$plot_params,
  #                  "hierarchical")
}
if (config$analytics$taxonomy$cluster_quality$active) {
  plot_cluster_quality_multiple_clusters(config$paths,
                       config$experiments$taxonomy$datasets_sizes, 
                       config$experiments$taxonomy$aggregate_curves$methods,
                       config$experiments$taxonomy$clustering$type,
                       config$analytics$taxonomy$cluster_quality$num_clusters,
                       config$analytics$taxonomy$cluster_quality$plot_params,
                       config$logger)
  
  plot_cluster_quality(config$paths,
                       config$experiments$taxonomy$datasets_sizes, 
                       config$experiments$taxonomy$aggregate_curves$methods,
                       config$experiments$taxonomy$clustering$type,
                       config$experiments$taxonomy$clustering$num_clusters,
                       config$analytics$taxonomy$cluster_quality$plot_params,
                       config$logger)
}