pred <- transform(read.table("lines-data.txt"),
                  replicate=factor(replicate),
                  nu=factor(nu))
direct.label(xyplot(rate~log10(gamma)|replicate+nu,pred,groups=data,type="l"))
