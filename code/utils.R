#' Helper function that will clear out the cached images folder
#' 
#' Clears out cached images folder (created by Shiny app) by first sorting
#' through to make sure least used (or oldest) file gets deleted first.
#'
#' @param f_path path to cached images folder
#' @param max_files maximum number of files in there
#'
#' @return none
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
