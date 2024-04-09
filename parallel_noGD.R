#---------------------------------HOUSEKEEPING----------------------------------
#install.packages("groundhog") #For reproducibility
#library(groundhog)
#packages <- c('mise','bigchess','googledrive')
#packages_date <- "2021-10-01" #yyyy--mm-dd (No later than 2 days ago)
#groundhog.library(packages, packages_date)
#mise() #Clear all R windows

#----------------------------VALUES TO BE MODIFIED------------------------------
#Input directory should contain only the files to be analyzed.
#Output is saved both locally and online.


library(bigchess)
library(parallel)
library(data.table)

input_directory <- "/sci/labs/yaakov.kareev/eranke8/input directory/"
output_directory <- "/sci/labs/yaakov.kareev/eranke8/output directory/"
engine_path <- "/sci/labs/yaakov.kareev/eranke8/komodo-12.1.1-linux-bmi2"
engine_depth <- c(15)
moves_to_end <- c(0, 4, 10)

#-------------------------FUNCTION: ANALYZE FILES-------------------------------
analyze_files <- function() {
  files <- list.files(path = input_directory, full.names = TRUE)
  for (f in files) {
    output_file_name = paste("output_",basename(f), sep = "")
    raw_data <- read.csv(f)
    new_data <- analyze_data(raw_data)
    print('writing csv')
    write.csv(new_data, output_file_name)
  }
}

#----------------------------FUNCTION: ANALYZE DATA-----------------------------
#Note 1: Bigchess gives int(0) instead of "mate in x moves".
#Here, this will be coded as NA.
#Note 2: Rarely, bigchess cannot analyze a game.
#Here, such a game will be removed (can be identified through the raw data).

analyze_data <- function(df) {
  for (remove in moves_to_end){
    cur_move_text_vec <- lapply(df$moveText, parse_move_text, num=remove)
    for (depth in engine_depth){
      column_score <- paste("score_", toString(remove), "_", toString(depth), sep="")
      column_time <- paste("time_", toString(remove), "_", toString(depth), sep="")
      #results <- lapply(cur_move_text_vec, get_score, depth=depth)
      results <- mclapply(cur_move_text_vec, get_score, depth=depth, mc.cores = 128)
	print(column_score)
      df[[column_score]] <- unlist(transpose(results)[1], use.names = FALSE)
      df[[column_time]] <- unlist(transpose(results)[2], use.names = FALSE)
    }
  }
  return(df)
}

#The get_score function
get_score <- function(move_txt, depth, show_time = TRUE) {
  #print('ok')
  tryCatch(
    {
      time <- system.time(ap <- analyze_position(engine_path, san=move_txt, depth=depth))
      score = ap$score
      #Assign 'NA' to games with score==int(0)
      if (length(score) == 0L) score <- NA
      values <- c(score)
      if (show_time) values[2] <- time[3] #Retrieve calculation time
    },
    error = function(e){
      values <- c(NA, 0)
    }
  )
  #print('ok')
  return(values)
}

parse_move_text <- function(move_text, num){
  i <- 0
  while (i < num){
    move_text <-  gsub("\\s?([0-9]*\\.)?\\s+[^ ]+$", "", move_text)
    i <- i+1
  }
  return(move_text)
}

#--------------------------------GENERATE DATA----------------------------------
setwd(output_directory)
analyze_files()
print("done")
#---
#---