void_parser <- function(description) {
  parse_arguments = function(command_line) {
    argv <- parse_args(command_line)
    return(argv)
  }
  return(parse_arguments(arg_parser(description)))
}

command_line <- list(
  scc = function(description, mylogger = NULL) {
    add_command_line_arguments <- function(description) {
      command_line_parser <- arg_parser(description)
      command_line_parser <- add_argument(command_line_parser, 
                                          "--responseMatrix", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Compute response matrix to feed IRT.",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--irt", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Compute IRT model and instances difficulty.",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--noisyDatasets", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Generates noisy datasets.",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--predictionsPerBin", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Compute predictions per difficulty bin",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--scc", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Compute System Characteristic curves (SCC).",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--taxonomy", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Compute taxonomy of models by robustness.",
                                          flag=TRUE)
      
      return(command_line_parser)
    }
    
    parse_command_line_arguments <- function(command_line_parser, mylogger = NULL) {
      argv <- parse_args(command_line_parser)
      log_parsed_arguments(argv, mylogger)
      return(argv)
    }
    
    command_line_parser <- add_command_line_arguments(description)
    return(parse_command_line_arguments(command_line_parser, mylogger))
  },
  taxonomy = function(description, mylogger = NULL) {
    log_info_message("Process Taxonomy: No arguments to parse.")
    return(void_parser(description))
  },
  analytics = function(description, mylogger = NULL) {
    add_command_line_arguments <- function(description) {
      command_line_parser <- arg_parser(description)
      command_line_parser <- add_argument(command_line_parser, 
                                          "--datasets", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Analytics for datasets.",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--irt", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Analytics for IRT.",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--noisyDatasets", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Analytics for noisy datasets.",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--predictionsPerBin", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Analytics for predictions per bin difficulty.",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--scc", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Analytics for System Characteristic Curves (SCC).",
                                          flag=TRUE)
      
      command_line_parser <- add_argument(command_line_parser, 
                                          "--taxonomy", 
                                          type="logical", 
                                          #default=FALSE,
                                          help="Plot the dendrograms.",
                                          flag=TRUE)
      
      return(command_line_parser)
    }
    
    parse_command_line_arguments <- function(command_line_parser, mylogger = NULL) {
      argv <- parse_args(command_line_parser)
      log_parsed_arguments(argv, mylogger)
      return(argv)
    }
    
    command_line_parser <- add_command_line_arguments(description)
    return(parse_command_line_arguments(command_line_parser, mylogger))
  }
)
