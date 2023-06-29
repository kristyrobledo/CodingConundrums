library(tidyverse)
library(naniar)
library(visdat)
library(survival)


# Visualisation of missing data

data(pbc)

#typically...
glimpse(pbc)

##better - shows the type and missingness
vis_dat(pbc)

##shows just missingness
vis_miss(pbc)

#typically...
ggplot(data=pbc,
       aes(x=alk.phos,
           y=trig))+
  geom_point()
#with a warning for missing data

#instead
ggplot(data=pbc,
       aes(x=alk.phos,
           y=trig))+
  geom_miss_point()

gg_miss_var(pbc)

##numbers of missing
n_miss(pbc)
n_complete(pbc)
n_miss(pbc$spiders)

df_miss<-miss_case_summary(pbc)

##replace NA with an values

?replace_na()
?replace_with_na
?na_if

gg_miss_upset(pbc)

library(ggmice)
plot_pattern(pbc)

## shiny app for learning about missingness
# https://allisonhorst.shinyapps.io/missingexplorer/
