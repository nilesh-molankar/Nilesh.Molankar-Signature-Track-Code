complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        
        ## creating an empty data frame that will be returned finally
        return_frame <- data.frame(id = numeric(), nobs = numeric()) 
        
        ## variable to track to row at which return_frame is filled
        row_pointer = 1
        
        ## read the list of files in the directory 
        path_to_files <- list.files(directory, full.names = TRUE)
        
        ## print length of the vector path_to_files
        ##print (length(path_to_files))
        ##print("******************************")
        
        for (i in seq_along(path_to_files))
        {
                ##print(path_to_files[i]) 
                ## extract only the numeric part of the file name
                ## curr_file --> will hold current file full path 
                ## curr_file_number --> will hold current file number part only
                curr_file = path_to_files[i]
                #print(curr_file)
                curr_file_number <- sub(directory,"",curr_file )
                curr_file_number <- sub("/","",curr_file_number )
                #print(curr_file_number)
                curr_file_number <- sub(".csv","",curr_file_number )
                curr_file_number <- as.numeric(curr_file_number)
                #print(curr_file_number)
                # print(id)
                if(is.element(curr_file_number, id)) 
                {
                        curr_filedata <- read.csv(file=curr_file, header=TRUE)
                        good <- complete.cases(curr_filedata)
                        curr_filedata_CompleteOnly <-  curr_filedata[good,][,]
                        
                        ##print("~~~~~~~~~~~~")
                        ##print(curr_filedata_CompleteOnly)
                        row_count = nrow(curr_filedata_CompleteOnly)
                        
                        ##print(curr_file_number)
                        ##print(row_count) 
                        
                        return_frame[row_pointer,] <- c(curr_file_number,row_count)
                        #rbind(return_frame,c(curr_file_number,row_count))
                        row_pointer <- row_pointer + 1 
                        ##print("~~~~~~~~~~~~")    
                }
        }
        
        # if id is given in the reverse order adjust the return data frame accordingly
        if((length(id)>1) && (id[1] > id[2]))
        {
                #print("Yes")
                
                tmp_frame <- return_frame
                row_count_2 <- nrow(return_frame)
                
                #print("~~~~~~~~~~~~")
                #print(row_count_2)
                #print("~~~~~~~~~~~~")
                #print(tmp_frame)
                #print("~~~~~~~~~~~~")
                #print(return_frame)
                ##print(return_frame[4][2])
                index_row <- row_count_2
                
                for(i in  1:row_count_2)
                {
                        tmp_id = tmp_frame[index_row,1]
                        #print(tmp_id)
                        tmp_nobs = tmp_frame[index_row,2]
                        #print(tmp_nobs)
                        
                        return_frame[i,] <- c(tmp_id, tmp_nobs)                          
                        index_row <- index_row - 1
                }
        }
       return_frame
}