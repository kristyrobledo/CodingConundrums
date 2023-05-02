library(tidyverse)
library(sjlabelled)
library(sjPlot)
library(performance)
library(palmerpenguins)
library(gtsummary)
library(ggeasy)
library(DHARMa) ## mixed multilevel models

## I couldnt get sjPlot to work, but
devtools::install_github("strengejacke/sjPlot")

##labelling tables and figures
penguins
glimpse(penguins)

penguins %>%
  var_labels(species = "Species of penguin",
             bill_depth_mm = "Bill depth (mm)") ->df

df %>%
  tbl_summary(by = sex)

ggplot(df, aes(y=bill_depth_mm, x=sex))+
  geom_boxplot() +
  easy_labs()

### model checks

lm(bill_depth_mm~ sex + year +species,
   data=df) ->m1

m1 %>%
  tbl_regression()

check_model(m1)

## model viz

plot_model(m1)


