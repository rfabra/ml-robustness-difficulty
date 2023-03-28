analytics_response_matrix <- function(datasets_names, 
                                      paths,
                                      num_models,
                                      plot_params,
                                      mylogger = NULL) {
  all_plots <- list()
  i_plot <- 0
  for (dataset_name in datasets_names) {
    i_plot <- i_plot + 1
    response_matrix_path <- file_path_response_matrix[["data"]](paths, dataset_name, num_models)
    response_matrix <- load_object(response_matrix_path)
    colnames(response_matrix) <- paste("item", colnames(response_matrix), sep=".")
    rownames(response_matrix) <- paste("response", rownames(response_matrix), sep=".")
    response_matrix_melted <- reshape::melt(as.matrix(response_matrix))
    colnames(response_matrix_melted) <- c("responses", "items", "answers")
    p <- ggplot(response_matrix_melted, aes(items, responses, fill= answers)) + 
      geom_tile() + 
      ggtitle(dataset_name) + 
      theme(plot.title = element_text(size = plot_params$title$size),
            axis.title = element_text(size = plot_params$axis$x$title$size),
            axis.text = element_text(size = plot_params$axis$x$text$size),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            strip.text = element_text(size = plot_params$subplots_title_size))
    
    all_plots[[i_plot]] <- p
    
  }
  
  response_matrix_analytics_path <- file_path_response_matrix[["analytics"]](
    paths, "all_datasets", num_models, extension = "pdf")
  
  save_plot[["multiple"]](all_plots, response_matrix_analytics_path, 
                          plot_params$doc, plot_params$num_cols)
}

analytics_difficulty <- function(datasets_names, 
                                 paths,
                                 irt,
                                 plot_params,
                                 mylogger = NULL) {
  all_difficulties <- data.frame(
    Dataset = NA,
    Difficulty = NA)
  all_difficulties <- all_difficulties[0,]
  
  for (dataset_name in datasets_names) {
    difficulty_path <- file_path_difficulty[["data"]](paths, dataset_name, irt)
    difficulty <- load_object(difficulty_path)
    dataset_difficulty <- data.frame(
      Dataset = dataset_name,
      Difficulty = difficulty$difficulty %>% dplyr::select(Difficulty))
    
    all_difficulties <- rbind(all_difficulties,
                              dataset_difficulty)
    
  }
  
  mean_diff <- all_difficulties %>% dplyr::group_by(Dataset) %>%
    dplyr::summarise(Mean.Difficulty = mean(Difficulty)) %>%
    dplyr::arrange(Mean.Difficulty)
  
  diff_order <- mean_diff$Dataset
  all_difficulties <- dplyr::mutate(all_difficulties,
                                    Dataset = factor(Dataset, levels = diff_order))
  
  p <- ggplot(all_difficulties, aes(x=Difficulty, y=Dataset, fill=stat(x))) + 
    theme_minimal() +
    geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Difficulty", option = "C") +
    ggtitle(plot_params$title$text) +
    set_plot_theme(plot_params)
  p  
  difficulty_analytics_path <- file_path_difficulty[["analytics"]](paths, "all-datasets", irt, "pdf")
  save_plot[["single"]](p, difficulty_analytics_path, plot_params$doc)
}

get_mean_difficulty_dataset <- function(dataset_name, 
                                        paths,
                                        irt,
                                        mylogger = NULL) {
  difficulty_path <- file_path_difficulty[["data"]](paths, dataset_name, irt)
  difficulty <- load_object(difficulty_path)
  dataset_difficulty <- data.frame(
    Dataset = dataset_name,
    Difficulty = difficulty$difficulty %>% dplyr::select(Difficulty))
  
  return(data.frame(Dataset = dataset_name,
                    Mean.Difficulty = mean(dataset_difficulty$Difficulty),
                    SD.Difficulty = sd(dataset_difficulty$Difficulty)))
  
}

get_mean_difficulty_datasets <- function(datasets_names, 
                                           paths,
                                           irt,
                                           plot_params,
                                           mylogger = NULL) {
  mean_difficulties <- data.frame(Dataset = NA,
                                  Mean.Difficulty = NA,
                                  SD.Difficulty = NA)
  mean_difficulties <- mean_difficulties[0,]
  
  for (dataset_name in datasets_names) {
    dataset_mean_difficulty <- get_mean_difficulty_dataset(dataset_name,
                                                           paths,
                                                           irt,
                                                           mylogger = mylogger)
    mean_difficulties <- rbind(mean_difficulties,
                               dataset_mean_difficulty)
  }
  mean_difficulties <- dplyr::arrange(mean_difficulties, desc(Mean.Difficulty))
  difficulty_means_path <- file_path_difficulty[["table_mean"]](paths, "all-datasets", irt, "rds")
  saveRDS(mean_difficulties, difficulty_means_path)
  
  return(mean_difficulties)
}