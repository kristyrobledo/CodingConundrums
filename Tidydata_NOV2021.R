#link to the slides on tidy data:

#https://www.openscapes.org/blog/2020/10/12/tidy-data/


#examples - https://r4ds.had.co.nz/tidy-data.html

library(tidyverse)
library(gtsummary)

#pivot_wider
?pivot_longer

table4a

table4a %>%
  pivot_longer(-country)

table4a %>%
  pivot_longer(-country, names_to = "year", values_to = "number") ->tidydf


#separate
#unite

library(palmerpenguins)
head(penguins)

penguins %>%
  tbl_summary(by=species,
              type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
              missing="no")


penguins %>%
  pivot_longer(cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm)) %>%
  filter(!is.na(value)) %>%
  group_by(species, year, name) %>%
  summarise(n=n(),
            mean=mean(value),
            sd=sd(value),
            ci = qt(0.975, df=n-1)*sd/sqrt(n)) ->overtime.summ

labs<-c(bill_length_mm="Bill length",
        flipper_length_mm = "Flipper length",
        bill_depth_mm = "Bill depth")

overtime.summ %>%
  ggplot(aes(y= mean, x=as.factor(year), colour=species)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position = position_dodge(0.3)) +
  facet_wrap(~name, scales = "free",
             labeller =labeller(name=labs)) +
  labs(x="", y="Mean and 95% CI") +
  theme_minimal()

