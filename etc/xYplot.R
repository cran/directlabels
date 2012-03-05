dfr <- expand.grid(month=1:12, continent=c('Europe','USA'), 
                   sex=c('female','male'))
set.seed(1)
dfr <- upData(dfr,
              y=month/10 + 1*(sex=='female') + 2*(continent=='Europe') + 
              runif(48,-.15,.15),
              lower=y - runif(48,.05,.15),
              upper=y + runif(48,.05,.15))
p <- xYplot(Cbind(y,lower,upper) ~ month,groups=sex, type='b', data=dfr,
            subset=continent=='Europe',method='bands')
print(p)
library(directlabels)
p2 <- direct.label(p)
print(p2)


library(Hmisc)
library(directlabels)
d <- read.table("xYplot.txt", header=T)
p <- xYplot(items.taken ~ bll, groups=ncond,
            data=subset(d, communicating=="T"),
            pch=".",  ylim=c(0,8), method=smean.cl.normal, lty.bands=c(0,0,0,1))
print(p)
direct.label(p)

