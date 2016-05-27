corr <- function(directory,threshold = 0){
  
  correlation <- vector('numeric')
  
  #  reads all files in list into one data frame
  filenames <-list.files(path=directory, full.names=TRUE)
  
  for (file in filenames){
    file_data <- read.csv(file,header = T)
    file_data <- file_data[complete.cases(file_data),]
    
    if (nrow(file_data)>threshold){
      correlation <- c(correlation,cor(file_data$sulfate,file_data$nitrate))}
  }
  correlation
}