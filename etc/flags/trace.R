library(grImport)
psfiles <- Sys.glob(file.path(getwd(),"data","*.ps"))
setwd(tempdir())
for(ps.file in psfiles){
  print(ps.file)
  xml.file <- sub("ps$","xml",ps.file)
  PostScriptTrace(ps.file,xml.file)
}
