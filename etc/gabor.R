### BUG: lattice colors do not match labels.

library(zoo)
library(lattice)
library(directlabels)

DF <-
structure(list(Date = structure(c(14868, 15076, 15148, 15076,
15148, 15300, 15076, 15148, 15300, 15400, 15300, 15400, 15498
), class = "Date"), P = c(0.01, 0.017, 0.019, 0.005, 0.006,
0.015, 0.011, 0.024, 0.028, 0.034, 0.025, 0.03, 0.038), Recorded =
structure(c(15148,
15148, 15148, 15300, 15300, 15300, 15400, 15400, 15400, 15400,
15498, 15498, 15498), class = "Date"), DT = c(9.69702219881228,
9.69702219881228, 9.69702219881228, 4.48708337398396, 4.48708337398396,
4.48708337398396, 7.68721120317047, 7.68721120317047, 7.68721120317047,
7.68721120317047, 10.781246696593, 10.781246696593, 10.781246696593
), `0.05` = structure(c(15552.8552268551, 15535.0599175429, 15559.7306872712,
15529.3837865259, 15565.484293371, 15537.0647454291, 15586.7607131701,
15395.5898243127, 15495.5901749651, 15530.0955259569, 15627.9295870214,
15641.6728229439, 15627.8367273192), class = "Date"), `0.10` =
structure(c(15847.8063187357,
15830.0110094234, 15854.6817791518, 15665.8659058179, 15701.966412663,
15673.5468647211, 15820.5800539332, 15629.4091650758, 15729.4095157282,
15763.91486672, 15955.8591740427, 15969.6024099653, 15955.7663143406
), class = "Date"), `0.20` = structure(c(16142.7574106162, 16124.962101304,
16149.6328710323, 15802.3480251099, 15838.448531955, 15810.0289840131,
16054.3993946963, 15863.2285058389, 15963.2288564913, 15997.7342074832,
16283.7887610641, 16297.5319969866, 16283.6959013619), class =
"Date")), .Names = c("Date",
"P", "Recorded", "DT", "0.05", "0.10", "0.20"), row.names = c(NA,
-13L), class = "data.frame")

ym <- floor(as.yearmon(cut(range(DF$Date), "year"))) + c(0, 11/12)
xlim <- c(as.Date(ym[1]), as.Date(ym[2], frac = 1))

plt <- xyplot(log2(P) ~ Date, DF, group = Recorded, panel = panel.superpose,
	xlim = xlim,
	panel.group = function(x, y, ..., group.number) {
		panel.lines(x, y, type = "o", col = group.number)
		n <- length(x)
		panel.points(x[n], y[n], pch = 20, col = group.number)
		panel.xyplot(x, y, type = "r", col = group.number, lty = 2)
	}
)
direct.label(plt, "last.points")

library(ggplot2)
plt <- ggplot(DF,aes(Date,log2(P)))+
  geom_line(aes(group=Recorded,colour=Recorded))
direct.label(plt, "last.points")
