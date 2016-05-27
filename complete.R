complete <- function(directory,id = 1:332){
  
  # creates a list of files for which data has to be read
  all_files <- id
  filenames <- id
  filenamectr <- 1
  for (file in all_files){
    idpad <- sprintf("%03d",file)
    filenames[filenamectr] <- paste(file.path(directory,idpad),".csv",sep="")
    filenamectr <- filenamectr +1
  }
  all_data <- lapply(filenames, function(x){read.csv(file=x,header=T)})
  
  file_count <- filenamectr - 1
  comp_frame <- data.frame(id=integer(file_count),nobs=integer(file_count))
  
  for (ctr in 1:file_count)
  {
    comp_frame$id[ctr]<-all_data[[ctr]][["ID"]][ctr]
    comp_frame$nobs[ctr] <- sum(complete.cases(all_data[[ctr]]))
  }
  comp_frame
}