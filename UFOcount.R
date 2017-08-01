#' UFOcount()
#' 
#' This function returns a count of the total monthly UFO sightings for a given month & year, or date range.
#' This defaults to returning a dataframe with all UFO sightings by month for all years available.

UFOcount <- function(date){
  library(httr)
  url_month<-"http://www.nuforc.org/webreports/ndxevent.html"##all the data if there, want to pull only for the month give
  lines<-readLines(url_month)
  date_pattern<-'<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>([^<]*)</TD>'
  datalines<-grep(date_pattern,lines[26:length(lines)], value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(date_pattern,datalines)
  matches = mapply(getexpr,datalines,gg)
  result = gsub(date_pattern,'\\1',matches)
  names(result) = NULL##UFO counts by month
  
  datpat<-'<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000><A HREF= ndxe([^<]*)</A></TD>'
  datadate<-grep(datpat,lines[26:length(lines)], value=TRUE)
  ggd = gregexpr(datpat,datadate)
  matchesd = mapply(getexpr,datadate,ggd)
  resultdate = gsub(datpat,'\\1',matchesd)
  names(resultdate) = NULL
  ##remove every 201611.html>
  ufos_month<-data.frame(Date = sapply(strsplit(resultdate, ">", fixed=TRUE),"[[",2),
                         UFOs = result)##make new data frame with UFO sightings by month
  date<-date
  return(ufos_month)
  
  
}

date<-'10/1988'
df<-UFOsightings(date='10/1988')
