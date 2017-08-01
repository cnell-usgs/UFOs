
##cleaning up UFO duration units
library(googlesheets)
library(dplyr)
nuforc<-gs_title("ufos")
ufos<-gs_read(nuforc, ws='sightings')%>%
  select(-Posted, -Summary)
##separate numbers and units into 2 columns,extract numbers (including punctuation)
ufos$dur_num<-as.character(unlist(gsub("[[:alpha:]]", "",unlist(ufos$Duration)), ""))##pulls out punct & numbers 
ufos$dur_unit<-as.character(unlist(gsub("[[:digit:]]", "",unlist(ufos$Duration)), ""))##pulls out letters
ufos$dur_unit_nop<-as.character(unlist(gsub("[[:punct:]]", "",unlist(ufos$dur_unit)), ""))##pulls out letters

##how many NAs? 
length(ufos$dur_unit_nop[is.na(ufos$dur_unit_nop)])##500 NA units

library(stringr)
ufos$unit_lower<-str_trim(tolower(ufos$dur_unit_nop))##lowercase and trims whitespaces

##create new variable that replaces strings that contain key word with desired spelling
ufos$hm<-ifelse(str_detect(ufos$unit_lower, "minute"), "min", 
                ifelse(str_detect(ufos$unit_lower, "hour"), "hr",
                       ifelse(str_detect(ufos$unit_lower, "second"),"sec",
                              ifelse(str_detect(ufos$unit_lower, "day"),"day",
                                     ifelse(str_detect(ufos$unit_lower,"week"),"week",
                                            ifelse(str_detect(ufos$unit_lower,"month"),"month",
                                                   ufos$unit_lower))))))

##make all unknowns NA
ufos$units<-ifelse(is.na(ufos$hm) | ufos$hm == "unknown" | ufos$hm == "" | ufos$hm == "not sure", NA, 
                   ifelse(ufos$hm == "mins", "min", ufos$hm))

ufos_un<-ufos%>%
  group_by(units)%>%
  summarize(n=length(Duration))
View(ufos_un)##320 groupings, 1386 NAs
##most of the rest are garbage , could group "continuous" 
###############################
#now the units
str(ufos)
UFO<-ufos%>%
  select(-unit_lower,-hm,-dur_unit,-dur_unit_nop)
str(UFO$dur_num)
###new var<- ifelse(is.numeric(dur_num), dur_num, )
UFO$numb<-ifelse(is.numeric(UFO$dur_num), UFO$dur_num, NA)
UFO$nu_num<-as.numeric(UFO$numb)
View(UFO)
##strip whitespace from numbers
UFO$dur_num<-str_trim(UFO$dur_num)
###paste numerics into new column, call NA if it is
UFO$numy<-ifelse(grepl("^[[:digit:]]+$", UFO$dur_num) == TRUE, as.numeric(UFO$dur_num),
                 ifelse(UFO$dur_num == "", NA,NA))
UFO$p<-ifelse(grepl("-", "",UFO$dur_num))
