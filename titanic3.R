# Load the required libraries dplyr and tidyr
library(dplyr)
require(dplyr)

# Load the refine.csv file into a dataframe df_refine
#df_titanic <-read.csv("titanic3.csv",na.strings=c("", "NA"), header = TRUE)
df_titanic <-read.csv("titanic3.csv", header = TRUE)

######################################################################################################
#   1: Port of embarkation
#http://stackoverflow.com/questions/12763890/exclude-blank-and-na-in-r
######################################################################################################
#df_titanic <- df_titanic %>% replace_na(list(embarked = "S"), embarked)

df_titanic$embarked <- as.character(df_titanic$embarked)
df_titanic <- df_titanic %>% 
              mutate("embarked" = ifelse(embarked == "", 'S',df_titanic$embarked))
df_titanic$embarked <- as.factor(df_titanic$embarked)

######################################################################################################
#   2: AGE 
# Populate missing values in AGE column. Replace with mean value and other methods.
######################################################################################################
#avg_master <- mean(c(0:13))
#print(avg_master)
#df_titanic$age <- as.numeric(df_titanic$age)
#df_titanic <-df_titanic %>% 
#            filter(grepl("master", name,ignore.case=TRUE)) %>% 
#          mutate("age" = ifelse(is.na(age)==TRUE,avg_master,age))
#View(df_titanic)

avg_age <- mean(df_titanic$age,na.rm = TRUE)
print(avg_age)
df_titanic <-df_titanic %>% 
            mutate("age" = ifelse(is.na(age)==TRUE,avg_age,age))

######################################################################################################
#   3: LIFE BOAT
# Fill empty slots with a dummy value e.g. the string 'None' 
######################################################################################################
#df_titanic <- df_titanic %>% 
#              replace_na(list(boat = "None"), embarked)

df_titanic <- df_titanic %>% 
  mutate("boat" = ifelse(boat == "", 'None',df_titanic$boat))
View(df_titanic)

######################################################################################################
#   4: CABIN
# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.
######################################################################################################

df_titanic <- df_titanic %>% 
        mutate("has_cabin_number" = ifelse(cabin == "", 0,1))
######################################################################################################
#   5: VIEW THE RESULTS
# 
######################################################################################################

View(df_titanic)