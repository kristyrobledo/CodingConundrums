
library(palmerpenguins)
library(tidyverse)
library(purrr)
library(gtsummary)
library(lmtest)

head(penguins)

glm(body_mass_g ~ sex+island+bill_length_mm, data=penguins)
glm(body_mass_g ~ sex+island+bill_depth_mm, data=penguins)
glm(body_mass_g ~ sex+island+flipper_length_mm , data=penguins)


c("bill_length_mm", "bill_depth_mm","flipper_length_mm") %>%
  map(
    function(varname){
      str_glue("body_mass_g ~ sex+island+ {varname}") %>%
        as.formula() %>%
      glm(data=penguins) %>%
        tbl_regression() %>%
        add_global_p()->mod

      return(mod)
    }
  ) ->modoutput


tbl_merge(modoutput)

## example two

c("bill_length_mm", "bill_depth_mm","flipper_length_mm") %>%
  map_df(
    function(varname){
      str_glue("body_mass_g ~ sex+island+ {varname}") %>%
        as.formula() %>%
        glm(data=penguins) ->mod

      modstats <- broom::glance(mod) %>%
        select(AIC, BIC, logLik) %>%
        mutate(variable = varname)
      return(modstats)
    }
  ) ->modoutput

## example three - map2

fulllist<-c("bill_length_mm+flipper_length_mm+bill_depth_mm",
            "bill_depth_mm +flipper_length_mm")

redlist<-c("bill_length_mm+flipper_length_mm",
           "flipper_length_mm")


map2_dbl(fulllist, redlist,
    function(full = x$fulllist,
             red= x$redlist){
      str_glue("body_mass_g ~ sex+island+ {full}") %>%
        as.formula() %>%
        glm(data=penguins) ->fmod

      str_glue("body_mass_g ~ sex+island+ {red}") %>%
        as.formula() %>%
        glm(data=penguins) ->rmod

      comp<-lrtest(fmod, rmod)
      return(comp$`Pr(>Chisq)`[2])
    }
  ) ->modcomp
