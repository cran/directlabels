library(grImport)
xml.files <- Sys.glob(file.path("data","*.xml"))
names(xml.files) <- gsub(".xml$","",gsub(".*/","",xml.files))
read.or.null <- function(x){
  tryCatch(readPicture(x),error=function(e)NULL)
}
pics <- lapply(xml.files,read.or.null)
pdf("Rflags.pdf")
for(p in pics){
  if(!is.null(p)){
    grid.newpage()
    grid.picture(p)
    ##picturePaths(p)
  }
}
dev.off()
