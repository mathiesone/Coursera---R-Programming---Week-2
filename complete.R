complete <- function(directory, id=1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1 117
        ## 2 1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        
        ## Create empty dataframe to store the overall data.
        overall_list <- data.frame(id=character(0), nobs = character(0))
        
        for (i in id) {
                
                ## call function to open each file
                FileToOpen <- openfile(directory, i)
                
                
                ## complete.cases to get the TRUE/FALSE values
                FileToOpen2 <- complete.cases(FileToOpen)
                
                ## sum gets the total number of TRUE - or completed cases
                TotalTrue = sum(FileToOpen2)
                
                
                ## Create a new data frame with these values
                current_info <- data.frame(id = c(i), nobs = TotalTrue)
                
                
                ## Combine the current i dataframe to the overall dataframe
                overall_list <- rbind(overall_list, current_info)
                
        }
        
        ## return the complete list
        overall_list
}









openfile <- function(directory, id) {
        
        
        ## find the length of the id
        idlen <- nchar(id)
        
        if (idlen < 3) {
                filename <- addzeros(id)
        }
        else {
                filename <- id
        }
        
        
        library(readr)
        
        FileLocationAndName <- paste(directory, "/", filename, ".csv", sep="")
        
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
