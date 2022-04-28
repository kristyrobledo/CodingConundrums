library(tidyverse)
library(metafor)
library(forestplot)
library(glue)
library(gtsummary)

# meta analysis
glimpse(dat.bcg)

dat <-escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos,di=cneg, data=dat.bcg)
dat$sei <-sqrt(dat$vi)

ma<-rma(yi, sei=sei, data=dat, method="FE")

metafor::forest(ma,
                xlab = "Relative risk (95% CI)")

# plot from gtsummary

library(palmerpenguins)
glimpse(penguins)

m1<-glm(bill_length_mm~ year+body_mass_g+island+species, data=penguins) %>%
  tbl_regression()

plot(m1)

# specific estimates

## example 1 - ggplot

penguins %>%
  pivot_longer(cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm)) %>%
  filter(!is.na(value)) %>%
  group_by(species, year, name) %>%
  summarise(n=n(),
            mn=mean(value),
            sd=sd(value),
            ci = qt(0.975, df=n-1)*sd/sqrt(n)) ->overtime.summ

labs<-c(bill_length_mm="Bill length",
        flipper_length_mm = "Flipper length",
        bill_depth_mm = "Bill depth")

overtime.summ %>%
  ggplot(aes(y= mn, x=as.factor(year), colour=species)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=mn-ci, ymax=mn+ci), position = position_dodge(0.3)) +
  coord_flip()+
  facet_wrap(~name, scales = "free",
             labeller =labeller(name=labs)) +
  labs(x="", y="Mean and 95% CI") +
  theme_minimal()

## example two - forestplot

overtime.summ %>%
  ungroup() %>%
  filter(name == "bill_depth_mm") %>%
  mutate(lci = mn - ci,
         uci = mn+ci,
         sum = glue("N = {n}, {round(mn, 1)} (95% CI: {round(lci, 1)} - {round(uci, 1)})")) %>%
  forestplot(mean = mn,
             lower = lci,
             upper = uci,
             labeltext = c(year, species, sum),
             boxsize=0.25,
             graph.pos=3)

