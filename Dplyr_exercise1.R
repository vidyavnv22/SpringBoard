#install.packages("tidyr")
#install.packages("dplyr")

# Load the required libraries dplyr and tidyr
library(dplyr)
require(dplyr)
library(tidyr) # Load tidyr since separate, unite functions are used
require(tidyr)

###############################################################################################
# 0: LOAD THE DATA IN RSTUDIO
# Read into dataframe df_refine
###############################################################################################

df_refine <-read.csv("refine.csv",header = TRUE)

###############################################################################################
# 1: CLEAN UP BRAND NAMES
#   
###############################################################################################

# a. change company names from upper case to lowercase
df_refine <- df_refine %>% 
              mutate(company = tolower(company))
 
# DO NOT DELETE (OBSERVATION)
# grepl is used only for pattern matching. gsub can be used for replacing matching patterns
# DOUBT: Mutate function acts on an entire column while filter executes on rows.Can we use a combination of 
# mutate and filter. This doubt is more evident in the titanic example. 
#df_refine <-  filter(df_refine, grepl("^p.*| s$",company)) %>%
#  mutate(company="phillips")
#print(df_refine)

# b. Pattern matching and Replacement
df_refine$company <- gsub("^p.*", "philips", df_refine$company) #replace starting from p
df_refine$company <- gsub("\\w*s$", "philips", df_refine$company) #replace for words ending s
df_refine$company <- gsub("^a.*", "arzo", df_refine$company)
df_refine$company <- gsub("^u.*", "unilever", df_refine$company)
df_refine$company <- gsub("^v.*", "van houten", df_refine$company)
df_refine <- arrange(df_refine, company) #sort by company

###############################################################################################
# 2. SEPARATE PRODUCT CODE AND NUMBER
# save result in dataframe df_refine
###############################################################################################

df_refine <- df_refine %>% 
              separate("Product_code", c("pcode","pnumber"),"-")

#######################################################################################
# 3. ADD PRODUCT CATEGORIES 
# below requirement can be done in 3 ways
# rowwise mutate, vectorized function and using cbind.
#http://www.expressivecode.org/2014/12/17/mutating-using-functions-in-dplyr/
#######################################################################################


category <- function(pcode){
  if (pcode == "v"){
    return('TV')
  } else if (pcode == 'p'){
    return("Smartphone")
  } else if (pcode == 'x'){
    return("Laptop")
  } else if (pcode == 'q'){
    return("Tablet")
  } else {
    return("None")
  }
}

df_refinenew <- df_refine %>% 
  rowwise() %>%
  mutate("category" = category(pcode))

#View(df_refinenew)

###################################################################################################
# 4. ADD FULL ADDRESS FOR GEOCODING
# concatenates the three address fields (address, city, country), separated by commas
# http://stackoverflow.com/questions/21003311/how-to-combine-multiple-character-columns-into-a-single-column-in-an-r-data-fram
###################################################################################################

df_refinenew <- df_refinenew %>% 
          unite(full_address, address, city, country, sep=",", remove=FALSE)
 

###################################################################################################
# 5. CREATE DUMMY VARIABLES FOR COMPANY AND PRODUCT CATEGORY 
# a. Add four binary (1 or 0) columns for company
# b. Add four binary (1 or 0) columns for product category 
###################################################################################################

df_refinenew <-df_refinenew %>% 
              mutate("company_philips" = ifelse(company == "philips", TRUE,FALSE))  %>% 
              mutate("company_arzo" = ifelse(company == 'arzo', TRUE, FALSE)) %>% 
              mutate("company_unilever" = ifelse(company == 'unilever', TRUE, FALSE)) %>% 
              mutate("company_vanhouten" = ifelse(company == 'van houten', TRUE, FALSE)) 
View(df_refinenew)

df_refinenew <-df_refinenew %>% 
              mutate("product_smartphone" = ifelse(pcode == "p", TRUE,FALSE))  %>% 
              mutate("product_tv" = ifelse(pcode == 'v', TRUE, FALSE)) %>% 
              mutate("product_laptop" = ifelse(pcode == 'x', TRUE, FALSE)) %>% 
              mutate("product_tablet" = ifelse(pcode == 'q', TRUE, FALSE)) 

View(df_refinenew)

###################################################################################################
# 6.WRITE THE OUTPUT IN A NEW FILE refinenew.csv
###################################################################################################
write.csv(df_refinenew, file = "refinenew.csv")
