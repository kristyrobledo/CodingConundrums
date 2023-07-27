# MICE package to impute missing data
# July 2023


library(tidyverse)
library(visdat)
library(survival)
library(mice)


# Visualisation of missing data

data(pbc)

## shows the type and missingness
vis_dat(pbc)

?mice

##complete case analysis

m1<-lm(albumin ~ trt+spiders+chol+age, data=pbc)

summary(m1)


##1. mice to impute the dataset
imputed_data<-mice(pbc,    #dataset to impute
                   m=10,   #number of datasets to create of imputed data
                   maxit=5, #iterations to perform the imputation
                   seed=20230727, #set a seed for reproducibility!
                   formulas = list(trt ~ spiders+chol+age,
                                   chol ~trt+spiders+age,
                                   spiders~trt+chol+age)
                   )

##2. with to analyse the datasets
mi_models<-with(data=imputed_data,
                exp = lm(albumin ~ trt+spiders+chol+age))

summary(mi_models$analyses[[10]])

##3. pool to pool the estimates
pool(mi_models)

combine<-pool(mi_models)

mi.results<-summary(combine)
mi.results

##to obtain 95% CI

est1<-round(mi.results$estimate[2])
lci<-est1-1.96*mi.results$std.error[2]


