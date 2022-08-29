

library(tidyverse)
library(dplyr)


# why make a function?
# often when we're writing script we find a chunk of code
# we want to repeat over again with new input
# instead of having to write the code over and over again
# we can put that code in a function, then with the function 
# you do not need to rewrite the code 
# you only need to add in the new input (parameters)

# when you use a function you've saved, 
# the template of code is called
# and the new input is replaced into that code

#this saves a lot of time and can streamline your code


function_name <- function(input){
  #process you want to apply to input
  input*10
}

function_name(20)

#calculate BMI (kg/m^2)
calculate_BMI <- function(weight_in_kg, height_in_M){
  BMI <- weight_in_kg / height_in_M^2
  return(BMI)
}

calculate_BMI(weight_in_kg =70, height_in_M = 1.77) 
sols_BMI <-calculate_BMI(70, 1.77) 


# Can add defaults

calculate_BMI_with_defaults <- function(weight_in_kg=66, height_in_M=1.55){
  BMI <- weight_in_kg / height_in_M^2
  return(BMI)
}

average_BMI <-calculate_BMI_with_defaults()




#sometimes you want to use a function to call 
# on column names in a dataframe
#calling on column names using dplyr in functions requires a special formatting

#first we'll make a fake dataset to try this on

example_df <- data.frame(weight = c(76,89,55,145,90,468, NA,80,60),
                          height =  rnorm(n=9, mean = 1.7, sd = 0.2),
                          outcome =       c(1,1,1,1,0,0,0,0,1),
                          intervention = rbinom(n=9, size=1, prob=1/2),
                          infant_id = seq(1,9,1))


# Functions require special formatting when using dplyr and calling column names from dataframes
#this will not work
summarise_groups <- function(dataframe, grouping_var, column_name){
  dataframe %>%
    group_by(grouping_var) %>%  
    summarise(mean(column_name, na.rm = TRUE))
}


summarise_groups(example_df, intervention, weight)
#returns error can't find the variable



#To fix this error so that R can find the variable while using
#dplyr in a function you can use the double curly bracket
#see below

summarise_groups1 <- function(dataframe, grouping_var, column_name){
  dataframe %>%
    group_by({{grouping_var}}) %>%  
    summarise(mean = mean({{column_name}}, na.rm = TRUE))
}
summarise_groups1(example_df, intervention, weight)


#why curly curly are needed when using dplyr in custom functions (pretty dry only look at if you're interested)
#https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/

# to illustrate how useful functions can be
# below is a simplified example of a more complex function the NextGen team has used.
# The function is applied to print descriptives and check continuous variables
# It checks whether the lower and upper bound are within range and produces other 
# summary stats in a table

library(flextable)

check_continuous <- function(variable_name,lower, upper, treatment_name, dataset_mod){
  
  
  if (any(!is.na(dataset_mod %>% select({{variable_name}})))) {
    
    
    #summary per treatment
    df1 <- dataset_mod %>%
      filter(!is.na({{treatment_name}})) %>% 
      group_by({{treatment_name}}) %>% 
      dplyr::summarise( N = n(),
                        Missing = sum(is.na({{variable_name}})),
                        `Mean \n(SD)` = paste(round(mean({{variable_name}}, na.rm = TRUE), digit = 2), "\n(", round(sd({{variable_name}}, na.rm = TRUE),digit = 2), ")"),
                        `Median \n(IQR)` =  paste(round(median.default({{variable_name}}, na.rm = TRUE),1),"\n(",round(IQR({{variable_name}}, na.rm = TRUE),digit = 2), ")"),
                        Q1 = quantile({{variable_name}}, probs = c(0.25), na.rm = TRUE),
                        Q3 = quantile({{variable_name}}, probs = c(0.75), na.rm = TRUE), 
                        Range = paste0("(",min({{variable_name}}, na.rm = TRUE), " - ", max({{variable_name}}, na.rm = TRUE), ")"),
                        `Acceptable range?` = (range({{variable_name}}, na.rm=T)[1]>=lower & range({{variable_name}}, na.rm=T)[2]<=upper)) %>% as.data.frame()
    
    
    #summary overall
    df2 <- dataset_mod %>%
      filter(!is.na({{treatment_name}})) %>% 
      dplyr::summarise({{treatment_name}} := paste0("Total"),
                       N = n(),
                       Missing = sum(is.na({{variable_name}})),
                       `Mean \n(SD)` = paste(round(mean({{variable_name}}, na.rm = TRUE), digit = 2), "\n(", round(sd({{variable_name}}, na.rm = TRUE),digit = 2), ")"),
                       `Median \n(IQR)` =  paste(round(median.default({{variable_name}}, na.rm = TRUE),1),"\n(",round(IQR({{variable_name}}, na.rm = TRUE),digit = 2), ")"),
                       Q1 = quantile({{variable_name}}, probs = c(0.25), na.rm = TRUE),
                       Q3 = quantile({{variable_name}}, probs = c(0.75), na.rm = TRUE), 
                       Range = paste0("(",min({{variable_name}}, na.rm = TRUE), " - ", max({{variable_name}}, na.rm = TRUE), ")"),
                       `Acceptable range?` = (range({{variable_name}}, na.rm=T)[1]>=lower & range({{variable_name}}, na.rm=T)[2]<=upper)) %>% as.data.frame()
    
    #binding treatment summaries and overall summary
    df3 <- rbind(df1,df2)
    
    #make into flextable
    
    flx <- df3 %>%
      flextable() %>%
      align(align = "center", part = "all") %>%
      fontsize(size = 8, part = "all") %>%
      width(width = (6.3/ncol(df3))) %>%
      hline(part = "all") 
    
    flx <- bg(flx, i = ~ `Acceptable range?`== TRUE, 
              j = 8,
              bg="palegreen", part = "body")
    
    flx <- bg(flx, i = ~ `Acceptable range?`== FALSE, 
              j = 8,
              bg="lightpink", part = "body")
    
    return(flx)
    
    
  } else{
    
    print("This variable is all NA")
  }
}

# test function for variable weight, with a lower bound of 
# 20kg, upper bound of 400kg, split by the variable labelled intervention
# from the dataframe labelled example df
check_continuous(weight, 20, 400,intervention, example_df )
# and now swap the variable of interest and the bounds
check_continuous(height, 50, 250,intervention, example_df )
#note they were all out of bound because I accidentally provided in cm instead of meters
#redoing but putting the the correct units
check_continuous(height, 0.50, 2.50,intervention, example_df )

#occasionally helpful symbols
#
#  <<- assigns object straight to the global environment within a custom function
# name a object in the custom function using a parameter in curly curly brackets
#  := allows you to use a parameter so that you can assign objects to that parameter name in a function
#e.g., df %>% summarise({{parameter}} := count({parameter}))
#

# also helpful 
# saving a group of functions in a script (e.g., all your data checking functions)
#then you can call all these functions for future use without having to rewrite them out
# source("filepath/../../functions.R")
# this runs everything in the script "functions.R" (loading each function in that script)
#e.g., this is useful when cleaning a new dataset because every checking/cleaning function
# will be loaded for use
