# Load the required libraries
library(dplyr)
library(fs)
# Load the data.table library
library(data.table)
# Load the stringr library
library(stringr)

# Set the root directory
size_of_subfolders <- function(root_dir, max_depth = Inf, min_size = 0){
  # List all subdirectories and files in the root directory recursively
  get_size_of_subdirectories <- function(dir) {
    # Get the list of files and subdirectories in the current directory
    dir_list <- dir_info(dir) %>%
      select(path, type, size) %>%
      as.data.table()

    # Calculate the size of all files in the current directory
    size_of_files_here <- dir_list[type == "file"]
    if(nrow(size_of_files_here) == 0){
      size_of_files_here = data.table(path = dir, size = fs::as_fs_bytes(0))
    }else{
      size_of_files_here = size_of_files_here[, .(path = dir, size = sum(size))]
    }
    # Get a list of subdirectories in the current directory
    subdirs <- dir_list[type == "directory", path]

    # If there are no subdirectories, return the size of the files in the current directory
    if (length(subdirs) == 0) {
      size_of_files_here[, csize := size]
      return(size_of_files_here)
    }

    # If there are subdirectories, recursively calculate the sizes of the files and subdirectories in each subdirectory
    size_of_subdirs_here <- lapply(subdirs, get_size_of_subdirectories) %>%
      rbindlist(fill = TRUE)

    #Calculate the cumulative size of the subdirectories in the current directory
    cumulative_size <- size_of_subdirs_here[path %in% subdirs, sum(csize)]

    #Return the sizes of the files and subdirectories in the current directory
    rbind(size_of_files_here[, csize := size + cumulative_size], size_of_subdirs_here)
  }
  subdirs_files <- get_size_of_subdirectories(root_dir)

  depth_root <- str_count(root_dir, "/")
  subdirs_files[,depth := str_count(path, "/") - depth_root + 1]
  subdirs_files[1, depth := depth - 1]
  setkey(subdirs_files, depth)

  return(subdirs_files[depth <= max_depth & csize >= fs::as_fs_bytes(min_size)])
}

# Set the root directory
size_of_subfolders_with_largest_files <- function(root_dir, max_depth = Inf, min_size = 0, min_size_file = 20*(1e+6)){
  # List all subdirectories and files in the root directory recursively
  get_size_of_subdirectories <- function(dir) {
    # Get the list of files and subdirectories in the current directory
    dir_list <- dir_info(dir) %>%
      select(path, type, size) %>%
      as.data.table()

    # Calculate the size of all files in the current directory
    files_here <- dir_list[type == "file"]
    if(nrow(files_here) == 0){
      size_of_files_here = data.table(path = dir, size = fs::as_fs_bytes(0))
    }else{
      size_of_files_here = files_here[, .(path = dir, size = sum(size))]
    }
    # Get a list of subdirectories in the current directory
    subdirs <- dir_list[type == "directory", path]


    # If there are no subdirectories, return the size of the files in the current directory
    if (length(subdirs) == 0) {
      size_of_files_here[, csize := size][,type := "dir"]
      files_show = files_here[size > min_size_file][,.(path = as.character(path), size)][, csize := fs::as_fs_bytes(0)][,type := "file"]
      if(nrow(files_show) > 0) size_of_files_here = rbind(size_of_files_here, files_show)
      return(size_of_files_here)
    }

    # If there are subdirectories, recursively calculate the sizes of the files and subdirectories in each subdirectory
    size_of_subdirs_here <- lapply(subdirs, get_size_of_subdirectories) %>%
      rbindlist(fill = TRUE)

    #Calculate the cumulative size of the subdirectories in the current directory
    cumulative_size <- size_of_subdirs_here[path %in% subdirs, sum(csize, na.rm = T)]

    #Return the sizes of the files and subdirectories in the current directory
    rbind(size_of_files_here[, csize := size + cumulative_size][,type := "dir"], size_of_subdirs_here)
  }
  subdirs_files <- get_size_of_subdirectories(root_dir)

  depth_root <- str_count(root_dir, "/")
  subdirs_files[,depth := str_count(path, "/") - depth_root + 1]
  subdirs_files[1, depth := depth - 1]
  setkey(subdirs_files, depth)

  return(subdirs_files[depth <= max_depth & csize >= fs::as_fs_bytes(min_size)])
}

if(FALSE){
  ## test ##
  root_dir <- "~"
  all_dirs = size_of_subfolders(root_dir)
  root_dir <- "/pool001/ols/"
  pool_dirs = size_of_subfolders(root_dir)
  ##
  size_of_subfolders(root_dir, max_depth  = 4)
  root_dir = "~/Downloads/"

  root_dir <- "/pool001/ols/"
  with_largest = size_of_subfolders_with_largest_files(root_dir)
  with_largest[type == "file"]

}

