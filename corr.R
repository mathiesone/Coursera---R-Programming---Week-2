corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        files <- list.files(path = directory)
        
        overall_list <- numeric()
        
        for (i in 1:length(files)) {
                
                
                FileToOpen <- openfile(directory, i)
                
                
                ## run the complete function on the file to retrieve a count
                ## of complete cases
                CompleteCount<- complete(directory,i)
                
                
                ## if this number is greater than the threshold value then...
                if (CompleteCount$nobs[1] > threshold) {
                        
                        FileToOpen2 <- FileToOpen[complete.cases(FileToOpen),]
                        
                        overall_list[i]  <- cor(FileToOpen2$sulfate, FileToOpen2$nitrate)
                }
                
                
        }
        
        overall_list <- overall_list[!is.na(overall_list)]
        
        overall_list
}
