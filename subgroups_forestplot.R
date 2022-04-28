
library(readxl)
library(tidyverse)
library(forestplot)
library(haven)

t10<-read_sas("Y:\\Statistics\\1 STUDIES\\Cardiology and Vascular Diseases\\T4DM main study\\8.5 Analysis\\Final analysis\\Tables\\table10.sas7bdat")
glimpse(t10)

## categorical endpoint

t10<-read_excel("./subgroups.xlsx")

df<-t10 %>%
  separate(est, c("e", "lci", "uci"), sep = "([\\(\\-\\)])")
  
tabletext<-cbind((c("A", t10$text)), 
                 (c("Placebo \nn/N(%)", t10$grpPlacebo)),
                 (c("Testosterone \nn/N(%)", t10$grpTestosterone)),
                 (c("Relative Risk (95% CI)", t10$est)),
                 (c("P-interaction",t10$pint)))

m<-c(NA, as.numeric(df$e))
l<-c(NA, as.numeric(df$lci))
u<-c(NA, as.numeric(df$uci))

## cont endpoint

t10_cont<-read_excel("./subgroups.xlsx", sheet = "Sheet1")

dfcont<-t10_cont %>%
  separate(est, c("e", "lci", "uci"), sep = "([\\(\\,\\)])")

tabletext2<-cbind((c("B", t10_cont$text)), 
                 (c("Placebo \n(n) mean change (SD)", t10_cont$grpPlacebo)),
                 (c("Testosterone \n(n) mean change (SD)", t10_cont$grpTestosterone)),
                 (c("Mean difference (95% CI)", t10_cont$est)),
                 (c("P-interaction",t10_cont$pint)))

m2<-c(NA, as.numeric(dfcont$e))
l2<-c(NA, as.numeric(dfcont$lci))
u2<-c(NA, as.numeric(dfcont$uci))

#setup font requests
font.size<-fpTxtGp(label = gpar(cex=.8), 
                   ticks = gpar(cex=.5), 
                   xlab  = gpar(cex=.5),
                   title = gpar(cex=.85), 
                   summary = gpar(fontface=2))

pdf("Figure2 - subgroups updated.pdf", height = 8, width = 14) 
library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=2, ncol=1)))
pushViewport(viewport(layout.pos.row = 1))
forestplot(tabletext,m,l,u,zero=1,
           align = 'l',
           is.summary = c(TRUE,
                          TRUE, FALSE,FALSE,
                          TRUE, FALSE,FALSE,  
                          TRUE, FALSE,FALSE),
           xlog=FALSE, 
           xticks = c(0.25, 0.5, 0.75, 1, 1.25),
           boxsize = 0.15,
           graph.pos = 4,  #place graph before p-interaction
           txt_gp = font.size,
           xlab = "RR (95% CI)", 
           new_page = FALSE)
popViewport()
pushViewport(viewport(layout.pos.row = 2))
forestplot(tabletext2,m2,l2,u2,zero=0,
           align = 'l',
           is.summary = c(TRUE,
                          TRUE, FALSE,FALSE,
                          TRUE, FALSE,FALSE,  
                          TRUE, FALSE,FALSE),
           xlog=FALSE, 
           boxsize = 0.15,
           graph.pos = 4,  #place graph before p-interaction
           txt_gp = font.size,
           xlab = "Mean difference in change at 2 years from baseline (95% CI)", 
           new_page = FALSE)
popViewport(2)
dev.off()

