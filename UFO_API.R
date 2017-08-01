##accessing data from data.gov

install.packages("htmltab")

library(httr)
url_month<-"http://www.nuforc.org/webreports/ndxevent.html"
mos<-GET(url_month)
names(mos)
mos$status_code ##good
mos$content
raw.content<-rawToChar(mos$content)
nchar(raw.content)
substr(raw.content, 1, 100)
this.content<-fromJSON(raw.content)
content(mos, as="text",type="read_xml")

library(rvest)
library(XML)
library(htmltab)
dd<-readLines(url_month)  

grep("Reports", dd)
dd[26:50]
##the href is the date 11/2016
#160 is the value

##date data wanted iis preceeded by HTML tag
##"<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000><A HREF= ndxe"  
##followed by "</A></TD>""
data_pattern<-'<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>([^<]*)</TD>'
datalines<-grep(data_pattern,dd[26:length(dd)], value=TRUE)
##pull data
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(data_pattern,datalines)
matches = mapply(getexpr,datalines,gg)
result = gsub(data_pattern,'\\1',matches)
names(result) = NULL
result[1:10]##pull out the numbers
datpat<-'<FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000><A HREF= ndxe([^<]*)</A></TD>'
datadate<-grep(datpat,dd[26:length(dd)], value=TRUE)
ggd = gregexpr(datpat,datadate)
matchesd = mapply(getexpr,datadate,ggd)
resultdate = gsub(datpat,'\\1',matchesd)
names(resultdate) = NULL

##remove every 201611.html>
ufos_month<-data.frame(Date = sapply(strsplit(resultdate, ">", fixed=TRUE),"[[",2),
                          UFOs = result)##make new data frame with UFO sightings by month

View(ufos_month)
write.csv(ufos_month, "/Users/colleennell/Dropbox/Projects/UFOs/UFOS_month.csv")
##so if the user enters a date range, needs to use date to write url address, the scrape those pages & bind them together
#start by month, then can add in day

date<-"10/1988"
url_obs<-"http://www.nuforc.org/webreports/ndxe201611.html"
obs<-readLines(url_obs)
grep("Date / Time", obs)##line found at
obs[26:55]
date_pattern<-'<TD bgcolor=\"#FFFFCC\" ><FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000><A HREF=([^<]*)</A></TD>'
data<-grep(date_pattern,obs[26:length(obs)], value=TRUE)
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(pattern,data)
matches = mapply(getexpr,data,gg)
result = gsub(pattern,'\\1',matches)
names(result) = NULL
str(result)
########
pat<-'<TR VALIGN=TOP>([^<]*)</TR>'
alld<-grep(pat, obs[26:length(obs)], value=TRUE)
gg = gregexpr(pat,alld)
matches = mapply(getexpr,alld,gg)
aresult = gsub(o_pattern,'\\1',matches)
names(aresult) = NULL
str(aresult)



o_pattern<-'<TD bgcolor=\"#FFFFCC\" ><FONT style=FONT-SIZE:11pt FACE=\"Calibri\" COLOR=#000000>([^<]*)</TD>'
odata<-grep(o_pattern,obs[26:length(obs)], value=TRUE)
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(o_pattern,odata)
matches = mapply(getexpr,odata,gg)
oresult = gsub(o_pattern,'\\1',matches)
names(oresult) = NULL
str(oresult)
omat<-matrix(oresult,ncol=6,byrow=TRUE)
str(omat)
odf<-as.data.frame(omat)
View(odf)
df<-cbind(result,omat)

monmat<-data.frame(Date=result,
                   )
########################os all that was awesome but jst need readHTMLTable
##search that pulls data either by location, month, observation
##enter date
year_start<-as.numeric(1988)
month_start<-as.numeric(10)
year_end<-as.numeric(1990)##make this like an as date situation that reads word months also?
month_end<-as.numeric(05)
date1<-as.Date(as.numeric(paste0(year_start,month_start,sep="")),format="%Y%m")
date1<-paste0(year_start,month_start,sep="")
str(date1)

format(date1, format="")
paste0(year_start,month_start,sep="")
seq(from=year_start, to=year_end)
seq(from=month_start, to=month_end)


str(year_start)
ser<-data.frame(years = as.numeric())


names<-paste(year, month, sep="")
url<-paste0("http://www.nuforc.org/webreports/ndxe", names, ".html")
obvy<-readHTMLTable(url)
View(obvy)




