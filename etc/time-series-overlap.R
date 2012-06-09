 require(ggplot2)
 require(plyr)
 require(reshape)

# create sample data
set.seed(666)
dfn <- data.frame(
Referral  = seq(as.Date("2007-01-15"), len= 26, by="23 day"),
VISIT01  = seq(as.Date("2008-06-15"), len= 24, by="15 day")[sample(30, 26)],
VISIT02  = seq(as.Date("2008-12-15"), len= 24, by="15 day")[sample(30, 26)],
VISIT03  = seq(as.Date("2009-01-01"), len= 24, by="15 day")[sample(30, 26)],
VISIT04  = seq(as.Date("2009-03-30"), len= 24, by="60 day")[sample(30, 26)],
VISIT05  = seq(as.Date("2010-11-30"), len= 24, by="6 day")[sample(30, 26)],
VISIT06  = seq(as.Date("2011-01-30"), len= 24, by="6 day")[sample(30, 26)],
Discharge = seq(as.Date("2012-03-30"), len= 24, by="30 day")[sample(30, 26)],
Patient  = factor(1:26, labels = LETTERS),
openCase  = rep(0:1, 100)[sample(100, 26)])

 # set today's data for cases that do not have an Discharge date
 dfn$Discharge[ is.na(dfn$Discharge) ] <- as.Date("2014-01-30")

 mdfn <- melt(dfn, id=c('Patient', 'openCase'), variable_name = "Visit")
 names(mdfn)[4] <- 'Year' # rename 

 # order data in mdfn by 'Referral' in dfn
 mdfn$Patient <- factor(mdfn$Patient,levels = 
 (dfn$Patient[order(dfn$Referral)]),ordered = TRUE)

 # subset a dataset to avoid 'Discharge' for cases that are not closed 
 mdfn2 <- subset(mdfn,!(Visit=="Discharge" & Year > as.Date("2014-01-01")))

 # the plot as it looks now
no.labels <- ggplot(mdfn, aes(Year, Patient)) +
     geom_blank() +
     geom_line(data = mdfn[mdfn$openCase == 0,], colour = "black") +
     geom_line(data = mdfn[mdfn$openCase == 1,], colour = "grey") +
     geom_point(data = mdfn2, aes(colour = Visit), size = 4, shape = 124)
overlapping <- no.labels+geom_text(data=mdfn2, mapping=aes(x=Year, y=Patient, 
     label=substr(Visit, 1, 7), colour=Visit), size=2, 
     vjust=-.4, hjust=-.1, angle = 00)
print(overlapping)

library(directlabels)
## You need to edit this function 
adjust.boxes <- function(d,...){
  gapply(d,function(d,...){
    print(d)
    d$box.color <- "grey"
    draw.rects(d)
    ## examine the top/bottom/left/right columns of d to get the
    ## locations of the text bounding box in cm. You need to return a
    ## data.frame with altered x/y columns.
    bumped <- apply.method("top.bumptwice",d)
    bumped$box.color <- "red"
    draw.rects(bumped)
    browser()
    bumped
  },groups="y")
}

## this is the Positioning Method
dont.overlap <- list(dl.trans(vjust=-0.1,hjust=0,cex=0.5),
                     "calc.boxes","adjust.boxes")
direct.label(no.labels,"dont.overlap")

