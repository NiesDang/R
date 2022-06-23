library(readr)
library(data.table)
library(stringr)
library(dplyr)



drugreac <- setDT(read_delim("Downloads/faers_ascii_2018q4/ascii/REAC18Q4.txt", 
                       delim = "$", escape_double = FALSE, trim_ws = TRUE))
#for the drugreac dataset you can m
drugreacfunct= function(data,medterm) {
  data$aeyn=as.numeric(toupper(data$pt) %like% medterm )

sub_drugreac=unique(data,by=c("primaryid" ,  "caseid" ,"aeyn"))

sub_drugreac=setDT(sub_drugreac)

setkeyv(sub_drugreac,c("primaryid","caseid") )

rowindex=sub_drugreac[unique(sub_drugreac[duplicated(sub_drugreac,by=c("primaryid","caseid"))]),which=T]

dup_sub_drugreac=sub_drugreac[rowindex,]

dup_sub_drugreac_y=dup_sub_drugreac[aeyn==1,]

notdup_sub_drugreac=sub_drugreac[-rowindex,]

sub_drugreac_fin=rbind(notdup_sub_drugreac,dup_sub_drugreac_y)

return(sub_drugreac_fin)
}

reactiondata=drugreacfunct(data=drugreac,medterm = "DEPRESSION")

#data cleaning for drug dataset
drugdata <- setDT(read_delim("Downloads/faers_ascii_2018q4/ascii/DRUG18Q4.txt", 
                             delim = "$", escape_double = FALSE, trim_ws = TRUE))

drugfunct=function(data,drugname) {

data$drugyn=as.numeric(toupper(data$drugname) %like% drugname )


sub_drugdata=unique(data,by=c("primaryid" ,  "caseid" ,"drugyn"))

setkeyv(sub_drugdata,c("primaryid" ,  "caseid"))
rowindex=sub_drugdata[unique(sub_drugdata[duplicated(sub_drugdata,by=c("primaryid" ,  "caseid"))]),which=T]

dup_sub_drugdata=sub_drugdata[rowindex,]
dup_sub_drugdata_y=dup_sub_drugdata[drugyn==1,]

notdup_sub_drugdata=sub_drugdata[-rowindex,]


sub_drugdata_fin=rbind(notdup_sub_drugdata,dup_sub_drugdata_y)

return(sub_drugdata_fin)}


drugdata=drugfunct(data=drugdata,drugname = "BENICAR")


fin=left_join(drugdata,reactiondata,c("primaryid" ,  "caseid"))

fisher.test(fin$drugyn,fin$aeyn)













