
#read in packages
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(survival)){
  install.packages("survival")
  library(survival)
}

if(!require(survminer)){
  install.packages("survminer")
  library(survminer)
}

if(!require(glue)){
  install.packages("glue")
  library(glue)
}


## kaplan meier plot
fit<-survfit(Surv(time, status) ~sex , data=lung)
fit

km<-ggsurvplot(fit, data=lung,
           xscale="d_y",   ##change to years
           break.time.by=365.25/2, ## chenge the xaxis ticks
           xlim=c(0, 2.5*365.25), ##change the x axis limits
           xlab="Years since registration", ylab="Proportion alive",
           risk.table = TRUE,
           palette = c("#ff0000", "#00008B"),
           legend.labs=c("Females", "Males"),
           legend="none",
           legend.title=" ",
           conf.int = TRUE,
           risk.table.col="strata",
           risk.table.height=0.2,
           tables.theme = theme_cleantable())



## logrank test
survdiff(Surv(time, status) ~sex , data=lung)

## cox regression
ph<-summary(coxph(Surv(time,status)~sex, data=lung))
HR<-round(ph$coefficients[2], digits=2)
HRl<-round(ph$conf.int[3], digits=2)
HRu<-round(ph$conf.int[4], digits=2)
p<-signif(ph$waldtest[3], digits=1)
stats<-glue('HR = {HR} (95% CI: {HRl}-{HRu}), p-value={p}')

##stick the cox regression data onto our curve
km$plot<-km$plot +
  annotate("text",
           x=1.5*365.25, y=0.8,
           hjust=0,
           label=stats)

ggsave(file="Survival.pdf", print(km))

library(patchwork)
(km$plot+km$plot)/(km$plot+km$plot)

##if you want to keep your risk tables, use arrange_ggsurvplots

## median survival time and other quantiles of survival
fit<-survfit(Surv(time, status) ~sex , data=lung)
fit
quantile(survfit(Surv(time, status) ~sex , data=lung))

## survival probability at time t
summary(survfit(Surv(time, status) ~sex , data=lung),
        times=c(.5*365.25, 365.25, 1.5*365.25))

## median followup time
table(lung$status)

lung %<>%
  mutate(followup = ifelse(status==2, 0, 1))

table(lung$followup)

quantile(survfit(Surv(time, followup) ~ 1 , data=lung))
