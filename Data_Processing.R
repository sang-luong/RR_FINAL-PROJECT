library(reshape)
library("readxl")
library(dplyr)

# DATA PROCESSING
#NOTE: 
# 21 MENA countries: Algeria, Bahrain, Egypt, Iran, Israel, Jordan, Kuwait, Lebanon, Libya, Morocco, Oman, Qatar, Saudi Arabia, Syria, Tunisia, United Arab Emirates, Yemen, Djibouti, Iraq, Palestine, Pakistan, Turkey 
# Yemen, Djibouti, Iraq, Palestine, Pakistan, Turkey is excluded in our data due the the missing data in some variables.

#1 . Corruption perception index (Transparency International)
MENA_CPI <- read.csv(file = 'CPI_RR.csv')
MENA_CPI[MENA_CPI$Jurisdiction %in% c('Algeria','Bahrain', 'Egypt', 'Iran', 'Israel', 'Jordan', 'Kuwait', 
                                      'Lebanon', 'Libya', 'Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syria', 'Tunisia', 'United Arab Emirates'), 
         c('Jurisdiction', 'X2003', 'X2004','X2005','X2006','X2007','X2008','X2009')] -> MENA_CPI
colnames(MENA_CPI) <- c('Country', '2003','2004','2005','2006','2007','2008','2009')
MENA_CPI$`2007`[MENA_CPI$`2007`=='-'] <- '4.7' # fill the missing value for Kuwait at year 2007
MENA_CPI <- melt(MENA_CPI, id=c("Country")) # covert data from wide frame format into long frame format
colnames(MENA_CPI) <- c('Country','year','CPI')
MENA_CPI$CPI <- as.numeric(MENA_CPI$CPI)


#2. Per capita GDP (WORLD BANK)
MENA_GDP <- read_excel("GDP_RR.xlsx", sheet = 1)
MENA_GDP[MENA_GDP$`Country Name` %in% c('Algeria','Bahrain','Egypt, Arab Rep.', 'Iran, Islamic Rep.', 'Israel', 'Jordan', 'Kuwait', 
                                        'Lebanon', 'Libya' ,'Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Tunisia', 
                                        'United Arab Emirates'),  c('Country Name', '2003','2004','2005','2006','2007','2008','2009')] -> MENA_GDP
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
                                        'United Arab Emirates'),  c('Country Name', '2003','2004','2005','2006','2007','2008','2009')] -> MENA_EXP
MENA_EXP <- melt(as.data.frame(MENA_EXP), id=c('Country Name'))
colnames(MENA_EXP) <- c('Country','year','EXP')

MENA_IMP <- read_excel("IMP_RR.xlsx", sheet = 1)
MENA_IMP[MENA_IMP$`Country Name` %in% c('Algeria','Bahrain','Egypt, Arab Rep.', 'Iran, Islamic Rep.', 'Israel', 'Jordan', 'Kuwait', 
                                        'Lebanon', 'Libya' ,'Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Tunisia', 
                                        'United Arab Emirates'),  c('Country Name', '2003','2004','2005','2006','2007','2008','2009')] -> MENA_IMP
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

#4. SCH (school enrollment) (WORLD BANK)
MENA_SCH <- read_excel("SCH_RR.xlsx", sheet = 1)
MENA_SCH[MENA_SCH$`Country Name` %in% c('Algeria','Bahrain','Egypt, Arab Rep.', 'Iran, Islamic Rep.', 'Israel', 'Jordan', 'Kuwait', 
                                        'Lebanon', 'Libya','Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Tunisia', 
                                        'United Arab Emirates'),  c('Country Name', '2003','2004','2005','2006','2007','2008','2009')] -> MENA_SCH
MENA_SCH <- melt(as.data.frame(MENA_SCH), id=c('Country Name'))
colnames(MENA_SCH) <- c('Country','year','SCH')
MENA_SCH[order(MENA_SCH$year, MENA_SCH$Country),] -> MENA_SCH

MENA_SCH$Country[MENA_SCH$Country =='Egypt, Arab Rep.'] <- 'Egypt'
MENA_SCH$Country[MENA_SCH$Country =='Iran, Islamic Rep.'] <- 'Iran'
MENA_SCH$Country[MENA_SCH$Country =='Syrian Arab Republic'] <- 'Syria'

#5. LAW (United Nations Office on Drugs and Crime)
MENA_LAW <- read_excel("LAW_RR.xlsx", sheet = 2)
MENA_LAW[MENA_LAW$Territory %in% c('Algeria','Bahrain','Egypt', 'Iran (Islamic Republic of)', 'Israel', 'Jordan', 'Kuwait', 
                                   'Lebanon', 'Libya','Morocco', 'Oman', 'Qatar', 'Saudi Arabia', 'Syrian Arab Republic', 'Tunisia', 
                                   'United Arab Emirates') & MENA_LAW$Year %in% c(2003,2004,2005,2006,2007,2008,2009),  c( 'Value','Territory','Year')] -> MENA_LAW
colnames(MENA_LAW) <- c('LAW','Country','year')
MENA_LAW[,c(2,3,1)] -> MENA_LAW
MENA_LAW[order(MENA_LAW$year, MENA_LAW$Country),] -> MENA_LAW

MENA_LAW$Country[MENA_LAW$Country =='Egypt, Arab Rep.'] <- 'Egypt'
MENA_LAW$Country[MENA_LAW$Country =='Iran (Islamic Republic of)'] <- 'Iran'
MENA_LAW$Country[MENA_LAW$Country =='Syrian Arab Republic'] <- 'Syria'
MENA_LAW$LAW <- as.numeric(MENA_LAW$LAW)

#6. FISCAL _ INVFREE (Heritage's Index of Economic Freedom)
MENA_FISCAL <- read_excel("FISCAL_RR.xlsx", sheet = 1)

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
                                        'United Arab Emirates'),  c('Country Name', '2004','2005','2006','2007','2008','2009','2010')] -> MENA_FDI
MENA_FDI <- melt(as.data.frame(MENA_FDI), id=c('Country Name'))
colnames(MENA_FDI) <- c('Country','year','FDI')
MENA_FDI[order(MENA_FDI$year, MENA_FDI$Country),] -> MENA_FDI

MENA_FDI$Country[MENA_FDI$Country =='Egypt, Arab Rep.'] <- 'Egypt'
MENA_FDI$Country[MENA_FDI$Country =='Iran, Islamic Rep.'] <- 'Iran'
MENA_FDI$Country[MENA_FDI$Country =='Syrian Arab Republic'] <- 'Syria'

MENA_FDI$LNFDI <- log(MENA_FDI$FDI + sqrt(MENA_FDI$FDI^2 + 100))
MENA_FDI$year <- as.factor(as.numeric(as.character(MENA_FDI$year))-1)


#Merge all data together
MENA_DATA <- merge(x = MENA_CPI, y = MENA_GDP, by = c('Country','year'), all = TRUE)  %>% 
  merge(MENA_OPEN,by = c('Country','year'), all = TRUE ) %>% 
  merge(MENA_SCH,by = c('Country','year'), all = TRUE ) %>% 
  merge(MENA_LAW,by = c('Country','year'), all = TRUE ) %>% 
  merge(MENA_FISCAL,by = c('Country','year'), all = TRUE ) %>% 
  merge(MENA_FDI,by = c('Country','year'), all = TRUE )

MENA_DATA$Country <- as.character(MENA_DATA$Country)
MENA_DATA$year <- as.integer(as.character(MENA_DATA$year))
MENA_DATA <- MENA_DATA[MENA_DATA$year %in% c(2003,2004,2005,2006,2007,2008),]


write.csv(MENA_DATA,"MENA_DATA.csv", row.names = FALSE)
MENA_test <- read.csv(file = 'MENA_DATA.csv')
