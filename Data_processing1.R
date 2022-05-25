library(reshape)
library("readxl")
library(dplyr)
library(ggplot2)
library(treemap)
library(RColorBrewer)

# DATA PROCESSING
#NOTE: 
# 21 MENA countries: Algeria, Bahrain, Egypt, Iran, Israel, Jordan, Kuwait, Lebanon, Libya, Morocco, Oman, Qatar, Saudi Arabia, Syria, Tunisia, United Arab Emirates, Yemen, Djibouti, Iraq, Palestine, Pakistan, Turkey 
# Yemen, Djibouti, Iraq, Palestine, Pakistan, Turkey is excluded in our data due the the missing data in some variables.

#1 . Corruption perception index (Transparency International)
MENA_CPI <- read.csv(file = 'CPI_RR.csv')
MENA_CPI[MENA_CPI$Jurisdiction %in% c('Algeria','Bahrain', 'Egypt', 'Iran', 'Israel', 'Jordan', 'Kuwait', 
                                      'Lebanon', 'Libya', 'Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syria', 'Tunisia', 'United Arab Emirates'), 
         c('Jurisdiction', 'X2003', 'X2004','X2005','X2006','X2007','X2008','X2009', 'X2010', 'X2011','X2012')] -> MENA_CPI
colnames(MENA_CPI) <- c('Country', '2003','2004','2005','2006','2007','2008','2009','2010','2011','2012')
MENA_CPI$`2007`[MENA_CPI$`2007`=='-'] <- '4.7' # fill the missing value for Kuwait at year 2007
MENA_CPI <- melt(MENA_CPI, id=c("Country")) # covert data from wide frame format into long frame format
colnames(MENA_CPI) <- c('Country','year','CPI')
MENA_CPI$CPI <- as.numeric(MENA_CPI$CPI)
MENA_CPI$CPI <- ifelse(MENA_CPI$CPI<10, MENA_CPI$CPI*10, MENA_CPI$CPI) 
MENA_CPI$LNCPI <- log(MENA_CPI$CPI)

#2. Per capita GDP (WORLD BANK)
MENA_GDP <- read_excel("GDP_RR.xlsx", sheet = 1)
MENA_GDP[MENA_GDP$`Country Name` %in% c('Algeria','Bahrain','Egypt, Arab Rep.', 'Iran, Islamic Rep.', 'Israel', 'Jordan', 'Kuwait', 
                                        'Lebanon', 'Libya' ,'Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Tunisia', 
                                        'United Arab Emirates'),  c('Country Name', '2003','2004','2005','2006','2007','2008','2009','2010','2011','2012')] -> MENA_GDP
MENA_GDP <- melt(as.data.frame(MENA_GDP), id=c('Country Name'))
colnames(MENA_GDP) <- c('Country','year','PCGDP')
MENA_GDP[order(MENA_GDP$year, MENA_GDP$Country),] -> MENA_GDP
MENA_GDP$Country[MENA_GDP$Country =='Egypt, Arab Rep.'] <- 'Egypt'
MENA_GDP$Country[MENA_GDP$Country =='Iran, Islamic Rep.'] <- 'Iran'
MENA_GDP$Country[MENA_GDP$Country =='Syrian Arab Republic'] <- 'Syria'
MENA_GDP$LNPCGDP <- log(MENA_GDP$PCGDP)

#3. OPENESS (WORLD BANK)
MENA_EXP <- read_excel("EXP_RR.xlsx", sheet = 1)
MENA_EXP[MENA_EXP$`Country Name` %in% c('Algeria','Bahrain', 'Egypt, Arab Rep.', 'Iran, Islamic Rep.', 'Israel', 'Jordan', 'Kuwait', 
                                        'Lebanon', 'Libya' ,'Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Tunisia', 
                                        'United Arab Emirates'),  c('Country Name', '2003','2004','2005','2006','2007','2008','2009','2010','2011','2012')] -> MENA_EXP
MENA_EXP <- melt(as.data.frame(MENA_EXP), id=c('Country Name'))
colnames(MENA_EXP) <- c('Country','year','EXP')

MENA_IMP <- read_excel("IMP_RR.xlsx", sheet = 1)
MENA_IMP[MENA_IMP$`Country Name` %in% c('Algeria','Bahrain','Egypt, Arab Rep.', 'Iran, Islamic Rep.', 'Israel', 'Jordan', 'Kuwait', 
                                        'Lebanon', 'Libya' ,'Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Tunisia', 
                                        'United Arab Emirates'),  c('Country Name', '2003','2004','2005','2006','2007','2008','2009','2010','2011','2012')] -> MENA_IMP
MENA_IMP <- melt(as.data.frame(MENA_IMP), id=c('Country Name'))
colnames(MENA_IMP) <- c('Country','year','IMP')

MENA_OPEN <- merge(x = MENA_EXP, y = MENA_IMP, by = c("Country", 'year'), all = TRUE)
MENA_OPEN$OPEN <- MENA_OPEN$EXP+ MENA_OPEN$IMP
MENA_OPEN[,c('Country', 'year', 'OPEN')] -> MENA_OPEN
MENA_OPEN[order(MENA_OPEN$year, MENA_OPEN$Country),] -> MENA_OPEN

MENA_OPEN$Country[MENA_OPEN$Country =='Egypt, Arab Rep.'] <- 'Egypt'
MENA_OPEN$Country[MENA_OPEN$Country =='Iran, Islamic Rep.'] <- 'Iran'
MENA_OPEN$Country[MENA_OPEN$Country =='Syrian Arab Republic'] <- 'Syria'
#rm(MENA_EXP)
#rm(MENA_IMP)



#4. FISCAL _ INVFREE (Heritage's Index of Economic Freedom)
MENA_FISCAL <- read_excel("FISCAL_RR_EXPAND.xlsx", sheet = 1)

MENA_FISCAL[MENA_FISCAL$Name %in% c('Algeria','Bahrain','Egypt', 'Iran', 'Israel', 'Jordan', 'Kuwait', 
                                    'Lebanon', 'Libya','Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syria', 'Tunisia', 
                                    'United Arab Emirates'), c('Name','Index Year','Tax Burden', 'Investment Freedom')] -> MENA_FISCAL


colnames(MENA_FISCAL) <- c('Country','year', 'FISCAL', 'FREEINV')
MENA_FISCAL[order(MENA_FISCAL$year, MENA_FISCAL$Country),] -> MENA_FISCAL
MENA_FISCAL$FISCAL <- round(as.numeric(MENA_FISCAL$FISCAL),1)
MENA_FISCAL$FREEINV <- as.numeric(MENA_FISCAL$FREEINV)

#7. FDI- FDIPC (WORLD BANK)
MENA_FDI <- read_excel("FDI_RR.xlsx", sheet = 1)
MENA_FDI[MENA_FDI$`Country Name` %in% c('Algeria','Bahrain','Egypt, Arab Rep.', 'Iran, Islamic Rep.', 'Israel', 'Jordan', 'Kuwait', 
                                        'Lebanon', 'Libya' ,'Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Tunisia', 
                                        'United Arab Emirates'),  c('Country Name', '2004','2005','2006','2007','2008','2009','2010','2011','2012','2013')] -> MENA_FDI
MENA_FDI$`2012`[is.na(MENA_FDI$`2012`)] <- 0
MENA_FDI$`2013`[is.na(MENA_FDI$`2013`)] <- 0

MENA_FDI <- melt(as.data.frame(MENA_FDI), id=c('Country Name'))
colnames(MENA_FDI) <- c('Country','year','FDI')
MENA_FDI[order(MENA_FDI$year, MENA_FDI$Country),] -> MENA_FDI

MENA_FDI$Country[MENA_FDI$Country =='Egypt, Arab Rep.'] <- 'Egypt'
MENA_FDI$Country[MENA_FDI$Country =='Iran, Islamic Rep.'] <- 'Iran'
MENA_FDI$Country[MENA_FDI$Country =='Syrian Arab Republic'] <- 'Syria'

MENA_FDI$LNFDI <- ifelse(MENA_FDI$FDI == 0 , 0, log(MENA_FDI$FDI + sqrt(MENA_FDI$FDI^2 + 200)))
MENA_FDI$year <- as.factor(as.numeric(as.character(MENA_FDI$year))-1)


#Merge all data together
MENA_DATA_ALL <- merge(x = MENA_CPI, y = MENA_GDP, by = c('Country','year'), all = TRUE)  %>% 
  merge(MENA_OPEN,by = c('Country','year'), all = TRUE ) %>% 
  merge(MENA_FISCAL,by = c('Country','year'), all = TRUE ) %>% 
  merge(MENA_FDI,by = c('Country','year'), all = TRUE )

MENA_DATA_ALL$Country <- as.character(MENA_DATA_ALL$Country)
MENA_DATA_ALL$year <- as.integer(as.character(MENA_DATA_ALL$year))
MENA_DATA_0309 <- MENA_DATA_ALL[MENA_DATA_ALL$year %in% c(2003,2004,2005,2006,2007,2008),c('Country','year','CPI','LNCPI','LNPCGDP','OPEN','FISCAL','FREEINV','LNFDI','FDI')]
MENA_DATA_0313 <- MENA_DATA_ALL[MENA_DATA_ALL$year %in% c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012),c('Country','year','CPI','LNCPI','LNPCGDP','OPEN','FISCAL','FREEINV','LNFDI','FDI')]


write.csv(MENA_DATA_0309,"MENA_DATA_0309.csv", row.names = FALSE)
write.csv(MENA_DATA_0313,"MENA_DATA_0313.csv", row.names = FALSE)

MENA_DATA_0309_test <- MENA_DATA_0309[,c(-3,-10)]
fixed_0309 <-plm(LNFDI~LNCPI+LNPCGDP+OPEN+FISCAL+FREEINV, data=MENA_DATA_0309, index=c("Country",'year'), model="within")
summary(fixed_0309)


random_0309 <-plm(LNFDI~LNCPI+LNPCGDP+OPEN+FISCAL+FREEINV, data=MENA_DATA_0309, index=c("Country",'year'), model="random")
summary(random_0309)

OLS_0309 <-plm(LNFDI~LNCPI+LNPCGDP+OPEN+FISCAL+FREEINV, data=MENA_DATA_0309, index=c("Country", "year"), model="pooling")
summary(OLS_0309)

fixed.time_0309 <-plm(LNFDI~LNCPI+LNPCGDP+OPEN+FISCAL+FREEINV + factor(year), data=MENA_DATA_0309, index=c("Country", "year"), model="within")
summary(fixed.time_0309)

phtest(fixed_0309, random_0309)

pFtest(fixed_0309, OLS_0309)

pFtest(fixed.time_0309, fixed_0309)


fixed_0313 <-plm(LNFDI~LNCPI+LNPCGDP+OPEN+FISCAL+FREEINV, data=MENA_DATA_0313, index=c("Country",'year'), model="within")
summary(fixed_0313)


random_0313 <-plm(LNFDI~LNCPI+LNPCGDP+OPEN+FISCAL+FREEINV, data=MENA_DATA_0313, index=c("Country",'year'), model="random")
summary(random_0313)

OLS_0313 <-plm(LNFDI~LNCPI+LNPCGDP+OPEN+FISCAL+FREEINV, data=MENA_DATA_0313, index=c("Country", "year"), model="pooling")
summary(OLS_0313)

fixed.time_0313 <-plm(LNFDI~LNCPI+LNPCGDP+OPEN+FISCAL+FREEINV + factor(year), data=MENA_DATA_0313, index=c("Country", "year"), model="within")
summary(fixed.time_0313)

phtest(fixed_0313, random_0313)

plmtest(OLS_0313, type=c("bp"))




