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
        mean_value = -1
        ##print ("parameter directory :")
        ##print (directory)
        ##print ("parameter pollutant : ")
        ##print (pollutant)
        ##print ("parameter id : ")
        ##print (id)
        
        ## this vector will hold final list of NON NA values from all the required files 
        ## and required column
        final_pollutant_values <- numeric()
        
        ## read the list of files in the directory 
        path_to_files <- list.files(directory, full.names = TRUE)
        
        ## print length of the vector path_to_files
        ##print (length(path_to_files))
        ##print("******************************")
        
        ## loop thru the vector and print the paths 
        for (i in seq_along(path_to_files))
        {       ## main for loop looping over all files STARTS
                ##print("---------------------------------")
                ##print(path_to_files[i]) 
                ## extract only the numeric part of the file name
                ## curr_file --> will hold current file full path 
                ## curr_file_number --> will hold current file number part only
                curr_file = path_to_files[i]
                ##print(curr_file)
                curr_file_number <- sub(directory,"",curr_file )
                curr_file_number <- sub("/","",curr_file_number )
                ##print(curr_file_number)
                curr_file_number <- sub(".csv","",curr_file_number )
                curr_file_number <- as.numeric(curr_file_number)
                ##print(curr_file_number)
                 
                ##check if the number is in the id vector
                if(is.element(curr_file_number, id)) {
                        ## if it is start reading that file 
                        ## print("File Matched")
                        ## get the file into a data frame
                        myData <- read.csv(file=curr_file, header=TRUE)
                        
                        ## get the required column into a vector
                        pollutant_column_data = myData[pollutant]
                                                
                        bad <- is.na(pollutant_column_data)
                        
                        tmp <- pollutant_column_data[!bad]
                        ##print(pollutant_column_data[!bad])

                        ##print(tmp_withOutNA)
                        for (i in seq_along(tmp)){
                                ##print(tmp[i])
                                final_pollutant_values <- c(final_pollutant_values, tmp[i])
                       }
                }
                else {
                        ## Ignore the file
                        #print("File NOT Matched")
                }

        }       ## main for loop looping over all files ENDS
        
        ##for(i in seq_along(final_pollutant_values))
        ##{print(final_pollutant_values[i])}
                
        mean_value <- mean(final_pollutant_values)
        mean_value <- round(mean_value,3)
        mean_value
}