
############### Description ###############
#Patched missing Fare with a lm of Fare ~ Pclass.
#Patched missing Ages by 
#	- Pulling the honorific out of Name, adding a categorical column.
#	- Predicting Age with a lm of Age ~ SibSp + Parch + Hon
#Final glm of Survived ~ Pclass * Sex * Age
#TODO - There were still a handful of NAs (set to 0 for submission file): determine where they arose.



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


#### From Aug 7, 2017
# The above is a mess.
# I'll start fresh here.


### Exploring the messiness.
dirty_col_names(data)
# only Age has NAs

summary(data)
# Age has 177 NAs

length(data$Age)
# 891 entries in total, so ~20% are NAs.

### Dealing with Age NAs
# Set them to be the average
#   Not ideal because it will artificially reduce measures of variability
# Use the other variables to predict age with a lm
#   Maybe better?

dirty_col_names(test_data)
# In the test data there are blanks in both Age and Fare

summary(test_data)
# 86NAs in Age, 1 in Fare, 418 rows in total

# fare is probably related to class and embarked (port of embarkation)

# age might be related to sibsp or parch (eg few children travelling alone)

# if I can parse out the honorific, it might give clues about age (e.g. "miss" vs "mrs")
# possible honorifics at a glance:
#   Miss, Mrs, Mr, Master, none

lm(formula = Fare ~ Pclass * Embarked * Cabin, data=data)
# this came out pretty messy
# Pclass defaults to being continuous instead of ordinal

### Available variables
names(test_data)
#[1] "PassengerId" "Pclass"      "Name"        "Sex"         "Age"        
#[6] "SibSp"       "Parch"       "Ticket"      "Fare"        "Cabin"      
#[11] "Embarked"


lapply(data, class)
#$PassengerId
#[1] "integer"
#               Should be nominal?
#$Survived
#[1] "integer"
#
#$Pclass
#[1] "ordered" "factor" 
#               Fixed
data$Pclass <- as.ordered(data$Pclass)
#$Name
#[1] "factor"
#
#$Sex
#[1] "factor"
#
#$Age
#[1] "numeric"
#
#$SibSp
#[1] "integer"
#
#$Parch
#[1] "integer"
#
#$Ticket
#[1] "factor"
#               ok...
#$Fare
#[1] "numeric"
#
#$Cabin
#[1] "factor"
#
#$Embarked
#[1] "factor"
# 
data$Pclass <- as.ordered(data$Pclass)
test_data$Pclass <- as.ordered(test_data$Pclass)

Fare_model <- lm(formula = Fare ~ Pclass * Embarked * Cabin, data=data)
Fare_model <- lm(formula = Fare ~ Pclass * Cabin, data=data)
Fare_model <- lm(formula = Fare ~ Pclass + Cabin, data=data)
Fare_model <- lm(formula = Fare ~ Pclass, data=data)

# Where is the missing data?
test_data[is.na(test_data$Fare),]
# 153        1044      3 Storey, Mr. Thomas male 60.5     0     0   3701   NA
#    Cabin Embarked
#153             S

#fare_subset <- test_data[is.na(test_data$Fare),]
# don't need the whole row

predict(Fare_model, test_data[is.na(test_data$Fare),2], interval="predict")

predict(Fare_model, test_data[is.na(test_data$Fare),])

########################### Fix the Fare NA
test_data[is.na(test_data$Fare),9] <- predict(Fare_model, test_data[is.na(test_data$Fare),])


############### Age NAs
data[is.na(data$Age),]

lm(formula = Age ~ SibSp + Parch, data=data)

Age_model <- lm(formula = Age ~ SibSp + Parch, data=data)
Age_model <- lm(formula = Age ~ SibSp, data=data)
Age_model <- lm(formula = Age ~ SibSp * Parch, data=data)

grep(", [[:alpha:]]+\\. ", c, value=TRUE)

grep(", [[:alpha:]]+\\. ", data$Name[1:10], value=TRUE)

regmatches(data$Name[1:10], regexpr(", [[:alpha:]]+\\. ",  data$Name[1:10]))

regmatches(b, regexpr(", [[:alpha:]]+\\. ",  b))

grepl(", [[:alpha:]]+\\. ",  b)

#data$Hon <- regmatches(data$Name, regexpr(", [[:alpha:]]+\\. ",  data$Name))
# Not every name matches so this spits out 1 row too few

data$Hon <- ifelse(grepl(", [[:alpha:]]+\\. ",  data$Name), 
regmatches(data$Name, regexpr(", [[:alpha:]]+\\. ",  data$Name)), 
"No Hon")

test_data$Hon <- ifelse(grepl(", [[:alpha:]]+\\. ",  test_data$Name), 
regmatches(test_data$Name, regexpr(", [[:alpha:]]+\\. ",  test_data$Name)), 
"No Hon")




Age_model <- lm(formula = Age ~ SibSp * Parch * Hon, data=data)
Age_model <- lm(formula = Age ~ Parch * Hon, data=data)
Age_model <- lm(formula = Age ~ SibSp * Hon, data=data)

# Keeping this one.
Age_model <- lm(formula = Age ~ SibSp + Parch + Hon, data=data)

########################### Fix the Age NAs
data[is.na(data$Age),][1:10,]
data[is.na(data$Age),6]

data[is.na(data$Age),6] <- predict(Age_model, data[is.na(data$Age),])
test_data[is.na(test_data$Age),6] <- predict(Age_model, test_data[is.na(test_data$Age),])

#################### All the data issues should be patched ####################

# data issue summary
# missing Ages
# missing Fare (1x)
# variable types - Pclass defaults to continuous integer instead of ordinal


#################### Survival Models ####################
Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked
# Pclass, Cabin, Fare related?
# Age, SibSp, and Parch related?

survived_model <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked, data=data)
# AIC 902.07
survived_model <- glm(formula = Survived ~ Pclass + Sex + Age + Embarked, data=data)
#AIC: 825.89
survived_model <- glm(formula = Survived ~ Pclass + Sex + Age, data=data)
#AIC: 828.31
survived_model <- glm(formula = Survived ~ Pclass * Sex * Age, data=data)
#AIC: 791.36
survived_model <- glm(formula = Survived ~ Pclass + Sex * Age, data=data)
#AIC: 816.42
survived_model <- glm(formula = Survived ~ Pclass + Sex + Sex:Age, data=data)
#AIC: 816.42


# Using:
survived_model <- glm(formula = Survived ~ Pclass * Sex * Age, data=data)

test_data$Raw <- predict(survived_model, test_data)

test_data$Survived <- ifelse(test_data$Raw>0.5, 1, 0)

test_data$Survived <- ifelse(is.na(test_data$Survived), 0, test_data$Survived)



test_data[1:10,c(1,14)]


write.csv(test_data[,c(1,14)], file="submit.csv", row.names=FALSE)






