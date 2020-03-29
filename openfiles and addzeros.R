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
