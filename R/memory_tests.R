# # Load the pryr package for mem_used() function
# if (!require(pryr)) install.packages("pryr", dependencies = TRUE)
# library(pryr)
#
# # Initialize variable to track maximum memory usage
# max_mem <- 0
#
# # Function to monitor memory usage at different points in the session
# track_memory_usage <- function() {
#   current_mem <- mem_used()
#   max_mem <<- max(max_mem, current_mem)
#   cat("Current memory usage: ", current_mem, " Max memory usage: ", max_mem, "\n")
# }
#
# # Example of code that uses memory
# cat("Starting memory usage tracking...\n")
# for (i in 1:10) {
#   track_memory_usage()
#
#   # Example code that consumes memory
#   x <- rnorm(10000)
#   track_memory_usage()
# }
#
# cat("Maximum memory usage observed: ", max_mem, "\n")
