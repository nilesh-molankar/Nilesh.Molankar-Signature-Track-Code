corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
#         print ("parameter directory :")
#         print (directory)
#         print ("parameter threshold : ")
#         print (threshold)
        
        ## create the return vector of length zero
        ##a <- numeric()
        return_vector <- numeric()
        
        ## checking that the type is numberic
        ## default length is 0
#         print(class(return_vector))
#         print(length(return_vector))
        
        ## read the directory and all its files ..
        ## variable to track to row at which return_frame is filled
        row_pointer = 1
        
        ## read the list of files in the directory 
        path_to_files <- list.files(directory, full.names = TRUE)
        
        for (i in seq_along(path_to_files))
        {
                curr_file = path_to_files[i]
#                 print(curr_file)
                
                ## read the file into a data frame
                curr_filedata <- read.csv(file=curr_file, header=TRUE)
#                 print(curr_filedata)
                
                ## get the complete cases 
                good <- complete.cases(curr_filedata)
                
                ## read only the complete cases into another vector
                curr_filedata_CompleteOnly <-  curr_filedata[good,][,]
                
                ## get the number of completely observered cases
                row_count = nrow(curr_filedata_CompleteOnly)
                
                ## if the number of completely observered cases is greater than the threshold
                ## calculate the correlation between sulfate and nitrate
                if(row_count > threshold)
                {
#                         print("GREATER")
#                         print(curr_file)
#                         print("-----------------------------")
#                         print(curr_filedata_CompleteOnly)
#                         print("-----------------------------")
#                         print("SULFATE")
#                         print(curr_filedata_CompleteOnly[,2])
                        
                        sulfate <- curr_filedata_CompleteOnly[,2]
                        
#                         print(sulfate)
#                         print("-----------------------------")
#                         print("NITRATE")
#                         print(curr_filedata_CompleteOnly[,3])
                        
                        nitrate <- curr_filedata_CompleteOnly[,3]
                        
#                         print(nitrate)
#                         print(cor(sulfate, nitrate))
                        #append(return_vector,cor(sulfate, nitrate))
                        # append the result to the return vector
                        return_vector <- c(return_vector, cor(sulfate, nitrate))
#                         print(return_vector)
                        
#                         > a <- numeric()
#                         > a <- c(a, 1)
#                         > a
#                         [1] 1
                }
#                 else {
#                         print("NOT GREATER")
#                 }
                
        }
        
#         print("~~~~~~~~~~~~~~~~~~~~")
#         for (i in seq_along(return_vector))
#         {
#                 
#                 print(return_vector[i])
#         }
#         print("~~~~~~~~~~~~~~~~~~~~")
#         ## find the complete cases in a given file 
#         ## if the number of completely observered cases is greater than the threshold
#         ## calculate the correlation between sulfate and nitrate
#         ## The function should return a vector of correlations for the monitors 
#         ## that meet the threshold requirement.
#         
#         print(class(return_vector))
#         print(length(return_vector))
        
        
        return_vector  ## returning vector 
} ## end of function 