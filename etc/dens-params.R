### need to use same arguments to density as lattice does!!

library(solaR)
siar = readMAPA(prov=19, est=9, start='01/01/2000', end='22/05/2011')
#str(siar)
datos = data.frame(siar@data$Precipitacion)
names(datos)="lluvias"

library(chron)
datos$dia = factor(weekdays(as.Date(row.names(datos),"%Y-%m-%d")))
datos$mes = factor(as.yearmon(row.names(datos)))
datos$finde = factor(ifelse(is.weekend(as.Date(row.names(datos),"%Y-%m-%d")),"FINDE","RESTO"))
summary(datos)

library(lattice)
dens <- densityplot( ~ lluvias | mes , data = datos, groups=finde ,bw=2,
            layout = c(3, 4), xlab = "Estevita", plot.points = FALSE) 
direct.label(dens)

