## data visualisation with ggplot
## February 2022

library(palmerpenguins)
library(tidyverse)

head(penguins)

## scatter

penguins %>%
  ggplot(aes(x=bill_length_mm, y=bill_depth_mm, colour=species))+
  geom_point()+
  theme_minimal()

penguins %>%
  ggplot(aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_minimal()


penguins %>%
  ggplot(aes(x=bill_length_mm, y=bill_depth_mm, colour=species))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x= "Bill length (mm)", y="Bill Depth (mm)")+
  theme_bw() +
  theme(legend.position = "bottom")

##boxplot

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x=as.factor(year), y=flipper_length_mm, fill=sex))+
  geom_boxplot()

## barchart

head(diamonds)

diamonds %>%
  ggplot(aes(x=cut, fill=color))+
  geom_bar()

diamonds %>%
  ggplot(aes(x=cut, fill=color))+
  geom_bar(position = "fill") +
  labs(y= "% color", x="")

diamonds %>%
  ggplot(aes(x=cut, fill=color))+
  geom_bar(position = "dodge") +
  labs(y= "Count", x="")

##means plot

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
  theme_minimal() ->penguins.means

library(ggeasy)

ggsave(penguins.means, device = "pdf", filename = "pengin means over time.pdf")
