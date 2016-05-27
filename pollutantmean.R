pollutantmean <- function(directory,pollutant,id = 1:332){
      # creates a list of filesfor which data has to be read
      all_files <- id
      filenames <- id
      filenamectr <- 1
      for (file in all_files){
       idpad <- sprintf("%03d",file)
        filenames[filenamectr] <- paste(file.path(directory,idpad),".csv",sep="")
        filenamectr <- filenamectr +1
      }
#  reads all files in list into one data frame
#    filenames <-list.files(path=directory, full.names=TRUE)
     all_data <- lapply(filenames, function(x){read.csv(file=x,header=T)})
     merged_data <-Reduce(function(x,y) {merge(x,y,all=TRUE,suffixes=c("",""))}, all_data)
     mean(merged_data[[pollutant]], na.rm = TRUE) 
}