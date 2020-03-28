pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating 
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        

        ## Empty list to capture average per file.
        overall_list <- numeric()
        
        for (i in id) {
                
                
                ## find the length of the id
                idlen <- nchar(i)
        
                if (idlen < 3) {
                        filename <- addzeros(i)
                }
                else {
                        filename <- i
                }
                
                ## call function to open each file
                FileToOpen <- openfile(directory, filename)
                
                ## x <- x[!is.na(x)]
                
                
                ## Check if the pullutant is "sulfate" or "nitrate" and select the specific column
                if (pollutant == "sulfate") {
                        NAFilter <- FileToOpen$sulfate
                }
                else if (pollutant == "nitrate") {
                        NAFilter <- FileToOpen$nitrate
                }
                
                ## Filter so only non-NA values remain.
                FileToOpen2 <- NAFilter[!is.na(NAFilter)]
                
                ## Add to the overall list from before
                overall_list <- c(overall_list, FileToOpen2)
                
        }
        
        ## Take average of all non-zero and non-NAN values in the overall list.
        mean(overall_list[!is.na(overall_list)])

}




openfile <- function(directory, id) {
        library(readr)
        
        FileLocationAndName <- paste(directory, id, ".csv", sep="")
        
        FileToOpen <- read_csv(FileLocationAndName, col_types = cols(col_guess(), col_double(), col_double(), col_double()))

        FileToOpen
}


## check if the length of the id = 3 to match the filenames
## if not, add leading zeros
addzeros <- function(x) {
        
        y <- nchar(x)
        
        if (y == 2) {
                x <- paste("0", x, sep="")
        }
        else if (y == 1) {
                x <- paste("00", x, sep="")
        }
        x
}
