load("directlabels_example.RData")

key <- list(corner = c(1,1),
            lines = list(col = "black", lty = 2),
            type = "l", text = list("Reference"),
            border = TRUE)
p <- xyplot(rate ~ dose, groups = group, type = 'l',
            data = dat, lty = 1,
            scales = "free", ylim = c(0, 1),  xlab = "Dose",
            ylab = "Growth Rate", main = "Example 1234",
            xlab.top = "Raw data at group level", key = key)
print(p)
library(directlabels)
direct.label(p,"last.qp")
trellis.focus()
llines(dat$dose, ref, col = "black", lty = 2)
trellis.unfocus()

load("directlabels_example.RData")
control.rows <- data.frame(rate=ref,
                 dose=dat$dose[1:length(ref)],
                 group="",type="control")
converted <- rbind(data.frame(dat,type="experiment"),
      control.rows)
group.colors <- rainbow(length(unique(dat$group)))
names(group.colors) <- unique(dat$group)
group.colors[""] <- "black"
library(ggplot2)
p <- ggplot(converted,aes(dose,rate))+
  geom_line(aes(colour=group,linetype=type))+
  scale_colour_manual(values=group.colors)
print(p)
library(directlabels)
direct.label(p) 

p <- ggplot(dat,aes(dose,rate))+
  geom_line(aes(colour=group))+
  geom_line(aes(linetype=type),data=control.rows)+
  scale_linetype_manual(values=c(control="dashed"))
print(p)
direct.label(p)
direct.label(p,list(rot=60,"last.qp",vjust=1))


p <- ggplot(converted)+
  geom_line(aes(dose,rate,colour=group,linetype=type))+
  scale_colour_manual(values=group.colors)
print(p)
direct.label(p) ## TODO: error! but this should be the same as above!

dat$group <- as.character(dat$group)
dat$group <-
  gsub("([0-9]{4})","\\\n\\1",dat$group,perl=TRUE)
