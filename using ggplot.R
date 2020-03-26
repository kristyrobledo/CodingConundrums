
## GGPLOT - https://adrienne-marshall.github.io/ggplot2_workshop/

#Data organizing:
library(dplyr)
library(data.table)

#Palettes and visualization:
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(wesanderson)
library(ggthemes)
library(ggjoy)

#Data
library(gapminder)
library(maps)
library(maptools)

#density boxplot
remotes::install_github("ropenscilabs/gghdr")
library(gghdr)

#covid19 data
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)

#scatterplot
head(diamonds)

ggplot(data=diamonds, aes(x=carat, y=price, colour=clarity))+
  geom_point()

ggplot(data=diamonds, aes(x=carat, y=price))+
  geom_point(colour="blue")+
  labs(x="Carat", y="Price (AU$)")+
  scale_x_continuous(breaks = seq(0,5, 0.5))+
  theme_minimal()

#boxplot

diamonds %>%
  filter(cut=="Good") %>%
  ggplot(aes(x=price))+
  geom_boxplot(colour="red", fill="red") +
  coord_flip() +
  labs(x="Price")+
  scale_x_continuous(breaks = seq(0,20000, 5000))+
  theme_minimal()

##advanced boxplot
ggplot(faithful, aes(x=eruptions, y=waiting))+
  geom_point() +
  geom_hdr_rug(aes(x=eruptions), prob=c(0.9, 0.7, 0.5), fill= "blue")

#histogram
diamonds %>%
  filter(cut=="Good") %>%
  ggplot(aes(x=price))+
  geom_histogram(colour="red", fill="red", binwidth = 500)

#means and 95% CI
head(diamonds)
diamonds %>%
  group_by(cut) %>%
  summarise(mean=mean(price), n=n(), stddev = sd(price)) %>%
  mutate(ci = stddev/sqrt(n)*1.96 ) %>%
  ggplot(aes(y=mean, x=cut, colour=cut)) +
  geom_point()+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=0.2)+
  labs(x="Cut", y="Mean price (95% CI)", colour="Cut") +
  coord_flip() +
  theme(legend.key = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#lines over time
head(coronavirus)

coronavirus %>%
  filter(type=="confirmed") %>%
  group_by(date) %>%
  summarise(total = sum(cases)) %>%
  ggplot(aes(x=date, y=total)) +
  geom_line()

#by country
coronavirus %>%
  filter(type=="confirmed") %>%
  group_by(date, Country.Region) %>%
  summarise(total = sum(cases)) %>%
  filter(Country.Region %in% c("Australia", "US","China","Spain",
                               "Italy", "Switzerland")) %>%
  ggplot(aes(x=date, y=(total), color=Country.Region)) +
  geom_line()

table(coronavirus$type)
#by country
coronavirus %>%
  filter(type=="death") %>%
  group_by(date, Country.Region) %>%
  summarise(total = sum(cases)) %>%
  filter(Country.Region %in% c("Australia", "US","China","Spain",
                               "Italy", "Switzerland")) %>%
  ggplot(aes(x=date, y=total, color=Country.Region)) +
  geom_line()


