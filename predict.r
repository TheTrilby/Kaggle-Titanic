####### Exploration functions #######
nas_per_col <- function(data)
{ # Returns a vector of the counts of NAs for each column.
    isnas <- c()
    for (column in c(1:length(colnames(data))))
    {
        isnas <- c(isnas, length(which(is.na(data[,column]))));
    }
return(isnas)
}


dirty_col_names <- function(data)
{ # Get the names of the columns in a data frame that have NAs
    na_count_bool <- c(nas_per_col(data) > 0)
    dirty_cols <- colnames(data)[which(na_count_bool)]
    return(dirty_cols)
}


####### Get some data ####### 
data = read.csv('train.csv', header = TRUE)
test_data = read.csv('test.csv', header = TRUE)

####### Exploration ####### 

#head(data)
#head(test_data)

#headers:
#data[0,]
#names(data)

#length(data[0,]) # 12 headers/columns
#length(data[,1]) #891 rows

#nas_per_col(data)

dirty_col <- as.vector(dirty_col_names(data))

#is.vector(dirty_col)

data$"Age"

#length(which(is.na(data$dirty_col)))



if(FALSE) {
na_col_summary <- function(data)
{ # Return the col name for columns with NAs, and the number of NAs in that column.
summary <- c()
for (dirty_col in c(dirty_col_names(data)))
    {
    #summary <- c(summary, dirty_col, length(which(is.na(data$dirty_col))))
    
    #return(length(which(is.na(data$dirty_col))))
    #c(dirty_col, length(which(is.na(data$dirty_col))))
    }
#return(summary)
return(length(which(is.na(data$dirty_col))))
}

na_col_summary(data)


#count of NAs for a column
#length(which(is.na(data$Age)))
}






#c(1:length(colnames(data)))

#which(is.na(data[,1]))
#which(is.na(data[,2]))
#which(is.na(data[,3]))
#which(is.na(data[,4]))
#which(is.na(data[,5]))
#length(which(is.na(data[,6]))) #177 NAs in this col
#which(is.na(data[,7]))
#which(is.na(data[,8]))
#which(is.na(data[,9]))
#which(is.na(data[,10]))
#which(is.na(data[,11]))
#which(is.na(data[,12]))

#colnames(data)[6] #it's the Age col that has NAs


#length(colnames(data))






# Clean the data


# Model Selection based on AIC


# Predict the submission set












