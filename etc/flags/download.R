source("regexp.R")
u <- url("http://en.wikipedia.org/wiki/Flags_of_the_U.S._states")
html.lines <- readLines(u)
close(u)
grep("svg",html.lines,value=TRUE)
html <- paste(html.lines,collapse="\n")
pattern <- paste("title=\"Flag of ",
                 "(?<state>.+?)",
                 " *(?:[([].*?[])])? *[(]",
                 "(?<datestr>[A-Z][a-z]+ [0-9]+, [0-9]{4})",sep="")
m <- str_match_all_perl(html,pattern)[[1]]
states <- data.frame(m[1:50,-1])
stopifnot(length(unique(states$state))==nrow(states))
states <- within(states,{
  date <- strptime(datestr,"%B %d, %Y")
})
states[order(states$date),]
dir.create("data",FALSE)

## cache state web pages to a local directory.
state.pages <- c()
for(state in states$state){
  p <- gsub(" ","_",state)
  cat(state,p,"\n")
  f <- file.path("data",sprintf("%s.html",p))
  state.pages[state] <- if(file.exists(f)){
    paste(readLines(f),collapse="\n")
  }else{
    u <- url(sprintf("http://en.wikipedia.org/wiki/File:Flag_of_%s.svg",p))
    html <- paste(readLines(u),collapse="\n")
    close(u)
    cat(html,file=f)
    html
  }
}

svg.urls <- str_match_perl(state.pages,
                           "http://upload.*?Flag_of_(?<state>.*?)[.]svg")
download.args <- data.frame(svg.urls)
names(download.args) <- c("url","destfile")
download.args <- within(download.args,{
  destfile <- file.path("data",sprintf("%s.svg",destfile))
  url <- as.character(url)
})
for(darg in split(download.args,1:nrow(download.args))){
  print(darg)
  do.call(download.file,darg)
}

