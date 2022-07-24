# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(dplyr)
install.packages("survey")
library(survey)
library(tidyverse)
install.packages("naniar")
library(naniar)
install.packages('tableone')
library(tableone)
#Demo datasets
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode="wb")
DEMO_J <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","SDMVSTRA","SDMVPSU","WTMEC2YR")]
DEMO_J
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT"),tf <- tempfile(),mode = "wb")
DEMO_H <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","SDMVSTRA","SDMVPSU","WTMEC2YR")]
DEMO_H
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode="wb")
DEMO_I <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","SDMVSTRA","SDMVPSU","WTMEC2YR")]
DEMO_I
#Smk datasets
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SMQ_J.XPT", tf <- tempfile(), mode="wb")
SMOK_J <- foreign::read.xport(tf)[,c("SEQN","SMD100MN")]
SMOK_J
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SMQ_H.XPT", tf <- tempfile(), mode="wb")
SMOK_H <- foreign::read.xport(tf)[,c("SEQN","SMD100MN")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SMQ_I.XPT", tf <- tempfile(), mode="wb")
SMOK_I <- foreign::read.xport(tf)[,c("SEQN","SMD100MN")]
#drug_use datasets
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DUQ_H.XPT",tf <- tempfile(),mode = "wb")
DRUG_H <- foreign::read.xport(tf)[,c('SEQN','DUQ240','DUQ330','DUQ250','DUQ260','DUQ290','DUQ340','DUQ370','DUQ390')]
DRUG_H
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DUQ_I.XPT",tf <- tempfile(),mode = "wb")
DRUG_I <-  foreign::read.xport(tf)[,c('SEQN','DUQ240','DUQ330','DUQ250','DUQ260','DUQ290','DUQ340','DUQ370','DUQ390')]
DRUG_I
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DUQ_J.XPT",tf <- tempfile(),mode = "wb")
DRUG_J <-  foreign::read.xport(tf)[,c('SEQN','DUQ240','DUQ330','DUQ250','DUQ260','DUQ290','DUQ340','DUQ370','DUQ390')]
DRUG_J
DEMO <- bind_rows(DEMO_J, DEMO_I,DEMO_H)
DEMO
SMOK <- bind_rows(SMOK_J,SMOK_I,SMOK_H)
DRUG <- bind_rows(DRUG_H,DRUG_I,DRUG_J)

typeof(One$WTMEC2YR)
One <- left_join(DEMO, SMOK, by="SEQN") 
One
TWO <- left_join(One,DRUG,BY = "SEQN")
TWO
#write.csv(TWO,"C:/Users/tn09t/Downloads/One.csv", row.names = FALSE)
#%>%
  #(vars(SMD100MN), 
            #~ifelse(., NA, .))
final <- TWO %>% drop_na(SMD100MN) 
final
View(final)
final$WTMEC4YR  = final$WTMEC2YR/3
final

final$DUQ240[final$DUQ240 >2] <- NA
final
final$DUQ250[final$DUQ250 >2] <- NA
final$DUQ370[final$DUQ370 > 2 ]<- NA
#replace_with_na(final,
#                replace = list('DUQ240' > 2))

#final <- final%>%replace_with_na(replace = variable(SMD100MN >2))

#final%>%replace_with_na(replace = list( DUQ240 >2))

NHANES_all <- svydesign(data=final, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC4YR, nest=TRUE)
summary(NHANES_all)
#View(NHANES_all)
#inAnalysis= subset(final,SMD100MN != NA)

final
table1 <- svytable(~SMD100MN+DUQ240,NHANES_all)
table2 <- svytable(~SMD100MN+DUQ250,NHANES_all)
table2
table3 <- table(final$SMD100MN,final$DUQ250)
table3
chisquadrug <- svychisq(~SMD100MN+DUQ240,NHANES_all)
chisqua
chisqua

CreateTableOne(data = NHANES_all)

svyCreateTableOne(data = NHANES_all,strata = c('SMD100MN'),
                  vars = c('DUQ240','DUQ330','DUQ250','DUQ260','DUQ290','DUQ340','DUQ370','DUQ390'),
                  factorVars = c('DUQ240','DUQ330','DUQ250','DUQ290','DUQ370')
                  )


NHANES_all
vars <- ModuleReturnVarsExist(vars, NHANES_all$SMD100MN)

Grop <- svyCreateCatTable(
  vars = c('DUQ240','DUQ330','DUQ250','DUQ290','DUQ370'),
  strata = c('SMD100MN'),
  data = NHANES_all,
  includeNA = FALSE,
  test = TRUE,
  testApprox = svyTestChisq,
  argsApprox = NULL,
  smd = TRUE,
  addOverall = FALSE
)

print(Grop)



'DUQ260','DUQ340','DUQ390'