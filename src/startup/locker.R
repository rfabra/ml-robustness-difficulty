lock_process <- function(lock_path_file, mylogger=NULL) {
  
  if (file.exists(lock_path_file)) {
    log_warn_message(paste("LOCKER: already locked -", lock_path_file), mylogger)
    ok <- FALSE
  } 
  else {
    log_info_message(paste("LOCKER: locking process -", lock_path_file), mylogger)
    file.create(lock_path_file)
    ok <- TRUE
  }
  
  return(ok)
}


unlock_process <- function(lock_path_file, mylogger=NULL) {
  
  if (file.exists(lock_path_file)) {
    log_info_message(paste("LOCKER: unlocking process -", lock_path_file), mylogger)
    file.remove(lock_path_file)
    ok <- TRUE
  } 
  else {
    log_warn_message(paste("LOCKER: process was not locked -", lock_path_file), mylogger)
    # file.create(lock_path_file)
    ok <- FALSE
  }
  
  return(ok)
}

get_oracle_lock_path <- function(locks_path, dataset_name, family, oracle_name) {
  return(path(locks_path, paste(dataset_name, family, oracle_name, "model.locked", sep="_")))
}
