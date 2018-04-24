clean_cached_folder <- function(f_path = normalizePath("./cached_images"), 
                                max_files = 100) {
  x <- dir_info(f_path)
  if (nrow(x) < max_files) {
    return(sprintf("No pruning. Only %i of %i spots currently taken.", 
                   nrow(x), max_files))
  } else {
    old_files_to_delete <- 
      dir_info("./results_explorer/cached_images/") %>% 
      arrange(access_time, birth_time, modification_time) %>% 
      slice((max_files + 1):n()) %>% 
      pull(path) %>% 
      normalizePath(.)
    
    unlink(old_files_to_delete)
  }
}

