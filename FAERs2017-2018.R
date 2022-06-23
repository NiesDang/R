library(readr)
library(data.table)
library(stringr)
library(dplyr)

reactiondata17 <- setDT(read.csv('C:/Users/tn09t/Downloads/Reaction2017/REAC17.csv'))

drugdata17 <- setDT(read.csv('C:/Users/tn09t/Downloads/Drug/Drug17.csv'))


drugreacfunct= function(data,medterm) {
  data$aeyn=as.numeric(toupper(data$pt) %like% medterm )
    sub_drugreac=unique(data,by=c("primaryid","aeyn"))
    sub_drugreac=setDT(sub_drugreac)
      setkeyv(sub_drugreac,c("primaryid") )
        rowindex=sub_drugreac[unique(sub_drugreac[duplicated(sub_drugreac,by=c("primaryid"))]),which=T]
          dup_sub_drugreac=sub_drugreac[rowindex,]
          dup_sub_drugreac_y=dup_sub_drugreac[aeyn==1,]
             notdup_sub_drugreac=sub_drugreac[-rowindex,]
              sub_drugreac_fin=rbind(notdup_sub_drugreac,dup_sub_drugreac_y)
  
  return(sub_drugreac_fin)
}

reactiondata17=drugreacfunct(data=reactiondata17,medterm = "SPRUE-LIKE ENTEROPATHY")
View(reactiondata17)
View(reactiondata1)

###
drugfunct=function(data,drugname) {
  
  data$drugyn=as.numeric(toupper(data$drugname) %like% drugname )
  
  
  sub_drugdata=unique(data,by=c("primaryid"  ,"drugyn"))
  
  setkeyv(sub_drugdata,c("primaryid" ))
  rowindex=sub_drugdata[unique(sub_drugdata[duplicated(sub_drugdata,by=c("primaryid"))]),which=T]
  
  dup_sub_drugdata=sub_drugdata[rowindex,]
  dup_sub_drugdata_y=dup_sub_drugdata[drugyn==1,]
  
  notdup_sub_drugdata=sub_drugdata[-rowindex,]
  
  
  sub_drugdata_fin=rbind(notdup_sub_drugdata,dup_sub_drugdata_y)
  
  return(sub_drugdata_fin)}

drugdata17=drugfunct(data=drugdata17,drugname = "EPROSARTAN")

drug1111<- drugdata17[drugdata17$drugname %like%"EPROSARTAN", ]
View(drug1111)
react1111 <- reactiondata17[reactiondata17$pt %like% "Sprue-like enteropathy"]
View(react1111)
fin2017 <- left_join(drugdata17,reactiondata17,c("primaryid"))
View(fin2017)
table(fin2017$aeyn,fin2017$drugyn)
fisher.test(fin2017$aeyn,fin2017$drugyn)
chisq.test(fin2017$aeyn,fin2017$drugyn)

###############################

drugdata17=drugfunct(data=drugdata17,drugname = "TELMISARTAN")

View(drugdata17)

fin2017 <- left_join(drugdata17,reactiondata17,c("primaryid"))
table(fin2017$aeyn,fin2017$drugyn)
fisher.test(fin2017$aeyn,fin2017$drugyn)


################################

drug1 <- drugdata17[drugdata17$drugname %like% "IRBESARTAN"]
View(drug1)
drugdata17=drugfunct(data=drugdata17,drugname = "IRBESARTAN")
View(drugdata17)
"irbesartan"
View(fin2017)
fin2017 <- left_join(drugdata17,reactiondata17,c("primaryid"))
table(fin2017$aeyn,fin2017$drugyn)
fisher.test(fin2017$aeyn,fin2017$drugyn)
chisq.test(fin2017$aeyn,fin2017$drugyn)


###################% "valsartan"]
drugdata11<- drugdata17[drugdata17$drugname %like% "VALSARTAN"]
View(drugdata11)
  drugdata17=drugfunct(data=drugdata17,drugname  ="VALSARTAN")
View(drugdata17)
fin2017 <- left_join(drugdata17,reactiondata17,c("primaryid"))
table(fin2017$aeyn,fin2017$drugyn)
fisher.test(fin2017$aeyn,fin2017$drugyn)
##############"losartan"

drugdata17=drugfunct(data=drugdata17,drugname  ="LOSARTAN")
fin2017 <- left_join(drugdata17,reactiondata17,c("primaryid"))
table(fin2017$aeyn,fin2017$drugyn)
fisher.test(fin2017$aeyn,fin2017$drugyn)

#################################################################DATA 2018##########################################################################

react2018 <- setDT(read.csv('C:/Users/tn09t/Downloads/reaction2018/reaction2018.csv'))
drug2018 <- setDT(read.csv('C:/Users/tn09t/Downloads/Drug2018/Drug18.csv'))

drugreacfunct= function(data,medterm) {
  data$aeyn=as.numeric(toupper(data$pt) %like% medterm )
  sub_drugreac=unique(data,by=c("primaryid","aeyn"))
  sub_drugreac=setDT(sub_drugreac)
  setkeyv(sub_drugreac,c("primaryid") )
  rowindex=sub_drugreac[unique(sub_drugreac[duplicated(sub_drugreac,by=c("primaryid"))]),which=T]
  dup_sub_drugreac=sub_drugreac[rowindex,]
  dup_sub_drugreac_y=dup_sub_drugreac[aeyn==1,]
  notdup_sub_drugreac=sub_drugreac[-rowindex,]
  sub_drugreac_fin=rbind(notdup_sub_drugreac,dup_sub_drugreac_y)
  
  return(sub_drugreac_fin)
}

reactiondata18=drugreacfunct(data=react2018,medterm = "SPRUE-LIKE ENTEROPATHY")
######
drugfunct=function(data,drugname) {
  
  data$drugyn=as.numeric(toupper(data$drugname) %like% drugname )
  
  
  sub_drugdata=unique(data,by=c("primaryid"  ,"drugyn"))
  
  setkeyv(sub_drugdata,c("primaryid" ))
  rowindex=sub_drugdata[unique(sub_drugdata[duplicated(sub_drugdata,by=c("primaryid"))]),which=T]
  
  dup_sub_drugdata=sub_drugdata[rowindex,]
  dup_sub_drugdata_y=dup_sub_drugdata[drugyn==1,]
  
  notdup_sub_drugdata=sub_drugdata[-rowindex,]
  
  
  sub_drugdata_fin=rbind(notdup_sub_drugdata,dup_sub_drugdata_y)
  
  return(sub_drugdata_fin)}


drugdata18=drugfunct(data=drug2018 ,drugname = "EPROSARTAN")

fin2018 <- left_join(drugdata18,reactiondata18,c("primaryid"))
table(fin2018$aeyn,fin2018$drugyn)

fisher.test(fin2017$aeyn,fin2017$drugyn)
chisq.test(fin2017$aeyn,fin2017$drugyn)

######TELMISARTAN

drugdata18=drugfunct(data=drug2018,drugname = "TELMISARTAN")
fin2018 <- left_join(drugdata18,reactiondata18,c("primaryid"))
table(fin2018$aeyn,fin2018$drugyn)
fisher.test(fin2018$aeyn,fin2018$drugyn)


########IRBESARTAN
drugggg <- drug2018[drug2018$drugname %like% "irbesartan"]
View(drugggg)
drugdata18=drugfunct(data=drug2018,drugname = "IRBESARTAN")
View(drugdata18)
Drug111111<- drugdata18[drugdata18 %like% "irbesartan",]

fin2018 <- left_join(drugdata18,reactiondata18,c("primaryid"))
table(fin2018$aeyn,fin2018$drugyn)
fisher.test(fin2018$aeyn,fin2018$drugyn)

#####VALSARTAN


drugdata18=drugfunct(data=drug2018,drugname  ="VALSARTAN")

fin2018 <- left_join(drugdata18,reactiondata18,c("primaryid"))
table(fin2018$aeyn,fin2018$drugyn)
fisher.test(fin2018$aeyn,fin2018$drugyn)


#####################"losartan"

drugdata18=drugfunct(data=drug2018,drugname  ="LOSARTAN")
fin2018 <- left_join(drugdata18,reactiondata18,c("primaryid"))
table(fin2018$aeyn,fin2018$drugyn)
fisher.test(fin2018$aeyn,fin2018$drugyn)


##################################################################DATASET2019############################################################
react19 <- setDT(read.csv('C:/Users/tn09t/Downloads/reaction2019/REAC19.csv'))
drug19 <- setDT(read.csv('C:/Users/tn09t/Downloads/Drug2019stack/drug19.csv'))

drugreacfunct= function(data,medterm) 
  {
  data$aeyn=as.numeric(toupper(data$pt) %like% medterm )
  sub_drugreac=unique(data,by=c("primaryid","aeyn"))
  sub_drugreac=setDT(sub_drugreac)
  setkeyv(sub_drugreac,c("primaryid") )
  rowindex=sub_drugreac[unique(sub_drugreac[duplicated(sub_drugreac,by=c("primaryid"))]),which=T]
  dup_sub_drugreac=sub_drugreac[rowindex,]
  dup_sub_drugreac_y=dup_sub_drugreac[aeyn==1,]
  notdup_sub_drugreac=sub_drugreac[-rowindex,]
  sub_drugreac_fin=rbind(notdup_sub_drugreac,dup_sub_drugreac_y)
  
  return(sub_drugreac_fin)}
reactiondata19=drugreacfunct(data=react19,medterm = "SPRUE-LIKE ENTEROPATHY")


####################################
drugfunct=function(data,drugname) {
  
  data$drugyn=as.numeric(toupper(data$drugname) %like% drugname )
  
  
  sub_drugdata=unique(data,by=c("primaryid"  ,"drugyn"))
  
  setkeyv(sub_drugdata,c("primaryid" ))
  rowindex=sub_drugdata[unique(sub_drugdata[duplicated(sub_drugdata,by=c("primaryid"))]),which=T]
  
  dup_sub_drugdata=sub_drugdata[rowindex,]
  dup_sub_drugdata_y=dup_sub_drugdata[drugyn==1,]
  
  notdup_sub_drugdata=sub_drugdata[-rowindex,]
  
  
  sub_drugdata_fin=rbind(notdup_sub_drugdata,dup_sub_drugdata_y)
  
  return(sub_drugdata_fin)}


drugdata19=drugfunct(data=drug19 ,drugname = "EPROSARTAN")

fin2019 <- left_join(drugdata19,reactiondata19,c("primaryid"))
table(fin2019$aeyn,fin2019$drugyn)



###################TELMISARTAN


drugdata19=drugfunct(data=drug19,drugname = "TELMISARTAN")
fin2019 <- left_join(drugdata19,reactiondata19,c("primaryid"))
table(fin2019$aeyn,fin2019$drugyn)
fisher.test(fin2019$aeyn,fin2019$drugyn)

##############IRBESARTAN
drugdata19=drugfunct(data=drug19,drugname = "IRBESARTAN")
fin2019 <- left_join(drugdata19,reactiondata19,c("primaryid"))
table(fin2019$aeyn,fin2019$drugyn)
fisher.test(fin2019$aeyn,fin2019$drugyn)

##############VALSARTAN

drugdata19=drugfunct(data=drug19,drugname  ="VALSARTAN")

fin2019 <- left_join(drugdata19,reactiondata19,c("primaryid"))
table(fin2019$aeyn,fin2019$drugyn)
fisher.test(fin2019$aeyn,fin2019$drugyn)
#####################"losartan"

drugdata19=drugfunct(data=drug19,drugname  ="LOSARTAN")
fin2019 <- left_join(drugdata19,reactiondata19,c("primaryid"))
table(fin2019$aeyn,fin2019$drugyn)
fisher.test(fin2019$aeyn,fin2019$drugyn)

###############################################DATASET2019


react20 <- setDT(read.csv('C:/Users/tn09t/Downloads/react2020/REAC20.csv'))
drug20 <- setDT(read.csv('C:/Users/tn09t/Downloads/drug2020/drug20.csv'))

drugreacfunct= function(data,medterm) 
{
  data$aeyn=as.numeric(toupper(data$pt) %like% medterm )
  sub_drugreac=unique(data,by=c("primaryid","aeyn"))
  sub_drugreac=setDT(sub_drugreac)
  setkeyv(sub_drugreac,c("primaryid") )
  rowindex=sub_drugreac[unique(sub_drugreac[duplicated(sub_drugreac,by=c("primaryid"))]),which=T]
  dup_sub_drugreac=sub_drugreac[rowindex,]
  dup_sub_drugreac_y=dup_sub_drugreac[aeyn==1,]
  notdup_sub_drugreac=sub_drugreac[-rowindex,]
  sub_drugreac_fin=rbind(notdup_sub_drugreac,dup_sub_drugreac_y)
  
  return(sub_drugreac_fin)}
reactiondata20=drugreacfunct(data=react20,medterm = "SPRUE-LIKE ENTEROPATHY")
#############


drugfunct=function(data,drugname) {
  
  data$drugyn=as.numeric(toupper(data$drugname) %like% drugname )
  
  
  sub_drugdata=unique(data,by=c("primaryid"  ,"drugyn"))
  
  setkeyv(sub_drugdata,c("primaryid" ))
  rowindex=sub_drugdata[unique(sub_drugdata[duplicated(sub_drugdata,by=c("primaryid"))]),which=T]
  
  dup_sub_drugdata=sub_drugdata[rowindex,]
  dup_sub_drugdata_y=dup_sub_drugdata[drugyn==1,]
  
  notdup_sub_drugdata=sub_drugdata[-rowindex,]
  
  
  sub_drugdata_fin=rbind(notdup_sub_drugdata,dup_sub_drugdata_y)
  
  return(sub_drugdata_fin)}


drugdata20=drugfunct(data=drug20 ,drugname = "EPROSARTAN")


fin2020 <- left_join(drugdata20,reactiondata20,c("primaryid"))
table(fin2020$aeyn,fin2020$drugyn)


###################TELMISARTAN


drugdata20=drugfunct(data=drug20,drugname = "TELMISARTAN")
fin2020<- left_join(drugdata20,reactiondata20,c("primaryid"))
table(fin2020$aeyn,fin2020$drugyn)

##############IRBESARTAN
drugdata20=drugfunct(data=drug20,drugname = "IRBESARTAN")
fin2020 <- left_join(drugdata20,reactiondata20,c("primaryid"))
table(fin2020$aeyn,fin2020$drugyn)

##############VALSARTAN

drugdata20=drugfunct(data=drug20,drugname  ="VALSARTAN")

fin2020 <- left_join(drugdata20,reactiondata20,c("primaryid"))
table(fin2020$aeyn,fin2020$drugyn)
fisher.test(fin2020$aeyn,fin2020$drugyn)
#####################"losartan"

drugdata20=drugfunct(data=drug20,drugname  ="LOSARTAN")
fin2020 <- left_join(drugdata20,reactiondata20,c("primaryid"))
table(fin2020$aeyn,fin2020$drugyn)
fisher.test(fin2020$aeyn,fin2020$drugyn)
