setwd("/home/gabriel/Finance/R - financial analysis/index analysis/Brazilian Indexes")

library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(Amelia)
library(corrplot)
####Analisar os indices e concluir os mais volateis e menos volateis, alem de verificar correlaçao com algumas commodities
##### e instrumentos financeiros
##### Dolar, Ouro, Bond yield, energy 
###########################  Getting Data  ########################################################################
IMAT <- read.csv("Basic Materials Historical Data.csv")
IBOV <- read.csv("Bovespa Historical Data.csv")
ICON <- read.csv("Consumption Historical Data.csv")
IFNC <- read.csv("Financials Historical Data.csv")
UTIL <- read.csv("Public Utilities Historical Data.csv")
SMLL <- read.csv("Small Cap Index Historical Data.csv")
IEE <- read.csv("Bovespa Electrical Energy Historical Data.csv")
INDX <- read.csv("Bovespa Industrial Sector Historical Data.csv")
IMOB <- read.csv("Real Estate Historical Data.csv")
MLCX <- read.csv("Mid-Large Cap Index Historical Data.csv")
IDIV <- read.csv("DIVO11 Historical Data.csv")
IFIX <- read.csv("BM&FBOVESPA Real Estate IFIX Historical Data.csv")
BDRX <- read.csv("BM&FBOVESPA Unsponsored BDRX Historical Data.csv")
IVBX <- read.csv("IVBX Historical Data.csv")
USD_BRL <- read.csv("USD_BRL Historical Data.csv")

#############################  clean the datas to transform into xts ##############################################
IMAT[ ,2:5] <- lapply(IMAT[,2:5], as.character)
IMAT[ ,2:5] <- lapply(IMAT[,2:5], function(X) gsub(",", "", X))
IMAT[ ,2:5] <- lapply(IMAT[,2:5], as.numeric)
  
IMAT <- IMAT[, c(1,3,4,5,2)]
names(IMAT) <- c("Date", "Open", "High", "Low", "Close")
IMAT$Date <- gsub(pattern = ",", replacement = "", x = IMAT$Date) %>%
  as.POSIXct(format = "%h %d %Y")
IMAT <- as.xts(IMAT[, c(2, 3, 4, 5)], order.by = IMAT$Date)

############
IBOV[ ,2:5] <- lapply(IBOV[,2:5], as.character)
IBOV[ ,2:5] <- lapply(IBOV[,2:5], function(X) gsub(",", "", X))
IBOV[ ,2:5] <- lapply(IBOV[,2:5], as.numeric)

IBOV <- IBOV[, c(1,3,4,5,2)]
names(IBOV) <- c("Date", "Open", "High", "Low", "Close")
IBOV$Date <- gsub(pattern = ",", replacement = "", x = IBOV$Date) %>%
  as.POSIXct(format = "%h %d %Y")
IBOV <- as.xts(IBOV[, c(2, 3, 4, 5)], order.by = IBOV$Date)

###########
ICON[ ,2:5] <- lapply(ICON[,2:5], as.character)
ICON[ ,2:5] <- lapply(ICON[,2:5], function(X) gsub(",", "", X))
ICON[ ,2:5] <- lapply(ICON[,2:5], as.numeric)

ICON <- ICON[, c(1,3,4,5,2)]
names(ICON) <- c("Date", "Open", "High", "Low", "Close")
ICON$Date <- gsub(pattern = ",", replacement = "", x = ICON$Date) %>%
  as.POSIXct(format = "%h %d %Y")
ICON <- as.xts(ICON[, c(2, 3, 4, 5)], order.by = ICON$Date)

###########

IFNC[ ,2:5] <- lapply(IFNC[,2:5], as.character)
IFNC[ ,2:5] <- lapply(IFNC[,2:5], function(X) gsub(",", "", X))
IFNC[ ,2:5] <- lapply(IFNC[,2:5], as.numeric)

IFNC <- IFNC[, c(1,3,4,5,2)]
names(IFNC) <- c("Date", "Open", "High", "Low", "Close")
IFNC$Date <- gsub(pattern = ",", replacement = "", x = IFNC$Date) %>%
  as.POSIXct(format = "%h %d %Y")
IFNC <- as.xts(IFNC[, c(2, 3, 4, 5)], order.by = IFNC$Date)

###########

UTIL[ ,2:5] <- lapply(UTIL[,2:5], as.character)
UTIL[ ,2:5] <- lapply(UTIL[,2:5], function(X) gsub(",", "", X))
UTIL[ ,2:5] <- lapply(UTIL[,2:5], as.numeric)

UTIL <- UTIL[, c(1,3,4,5,2)]
names(UTIL) <- c("Date", "Open", "High", "Low", "Close")
UTIL$Date <- gsub(pattern = ",", replacement = "", x = UTIL$Date) %>%
  as.POSIXct(format = "%h %d %Y")
UTIL <- as.xts(UTIL[, c(2, 3, 4, 5)], order.by = UTIL$Date)

###########

SMLL[ ,2:5] <- lapply(SMLL[,2:5], as.character)
SMLL[ ,2:5] <- lapply(SMLL[,2:5], function(X) gsub(",", "", X))
SMLL[ ,2:5] <- lapply(SMLL[,2:5], as.numeric)

SMLL <- SMLL[, c(1,3,4,5,2)]
names(SMLL) <- c("Date", "Open", "High", "Low", "Close")
SMLL$Date <- gsub(pattern = ",", replacement = "", x = SMLL$Date) %>%
  as.POSIXct(format = "%h %d %Y")
SMLL <- as.xts(SMLL[, c(2, 3, 4, 5)], order.by = SMLL$Date)

############

IEE[ ,2:5] <- lapply(IEE[,2:5], as.character)
IEE[ ,2:5] <- lapply(IEE[,2:5], function(X) gsub(",", "", X))
IEE[ ,2:5] <- lapply(IEE[,2:5], as.numeric)

IEE <- SIEE[, c(1,3,4,5,2)]
names(IEE) <- c("Date", "Open", "High", "Low", "Close")
IEE$Date <- gsub(pattern = ",", replacement = "", x = IEE$Date) %>%
  as.POSIXct(format = "%h %d %Y")
IEE <- as.xts(IEE[, c(2, 3, 4, 5)], order.by = IEE$Date)
              
#############

INDX[ ,2:5] <- lapply(INDX[,2:5], as.character)
INDX[ ,2:5] <- lapply(INDX[,2:5], function(X) gsub(",", "", X))
INDX[ ,2:5] <- lapply(INDX[,2:5], as.numeric)

INDX <- INDX[, c(1,3,4,5,2)]
names(INDX) <- c("Date", "Open", "High", "Low", "Close")
INDX$Date <- gsub(pattern = ",", replacement = "", x = INDX$Date) %>%
  as.POSIXct(format = "%h %d %Y")
INDX <- as.xts(INDX[, c(2, 3, 4, 5)], order.by = INDX$Date)

###############

IMOB[ ,2:5] <- lapply(IMOB[,2:5], as.character)
IMOB[ ,2:5] <- lapply(IMOB[,2:5], function(X) gsub(",", "", X))
IMOB[ ,2:5] <- lapply(IMOB[,2:5], as.numeric)

IMOB <- IMOB[, c(1,3,4,5,2)]
names(IMOB) <- c("Date", "Open", "High", "Low", "Close")
IMOB$Date <- gsub(pattern = ",", replacement = "", x = IMOB$Date) %>%
  as.POSIXct(format = "%h %d %Y")
IMOB <- as.xts(IMOB[, c(2, 3, 4, 5)], order.by = IMOB$Date)
               
################

MLCX[ ,2:5] <- lapply(MLCX[,2:5], as.character)
MLCX[ ,2:5] <- lapply(MLCX[,2:5], function(X) gsub(",", "", X))
MLCX[ ,2:5] <- lapply(MLCX[,2:5], as.numeric)

MLCX <- MLCX[, c(1,3,4,5,2)]
names(MLCX) <- c("Date", "Open", "High", "Low", "Close")
MLCX$Date <- gsub(pattern = ",", replacement = "", x = MLCX$Date) %>%
  as.POSIXct(format = "%h %d %Y")
MLCX <- as.xts(MLCX[, c(2, 3, 4, 5)], order.by = MLCX$Date)
               
########### 

IDIV[ ,2:5] <- lapply(IDIV[,2:5], as.character)
IDIV[ ,2:5] <- lapply(IDIV[,2:5], function(X) gsub(",", "", X))
IDIV[ ,2:5] <- lapply(IDIV[,2:5], as.numeric)

IDIV <- IDIV[, c(1,3,4,5,2)]
names(IDIV) <- c("Date", "Open", "High", "Low", "Close")
IDIV$Date <- gsub(pattern = ",", replacement = "", x = IDIV$Date) %>%
  as.POSIXct(format = "%h %d %Y")
IDIV <- as.xts(IDIV[, c(2, 3, 4, 5)], order.by = IDIV$Date)

############

IFIX[ ,2:5] <- lapply(IFIX[,2:5], as.character)
IFIX[ ,2:5] <- lapply(IFIX[,2:5], function(X) gsub(",", "", X))
IFIX[ ,2:5] <- lapply(IFIX[,2:5], as.numeric)

IFIX <- IFIX[, c(1,3,4,5,2)]
names(IFIX) <- c("Date", "Open", "High", "Low", "Close")
IFIX$Date <- gsub(pattern = ",", replacement = "", x = IFIX$Date) %>%
  as.POSIXct(format = "%h %d %Y")
IFIX <- as.xts(IFIX[, c(2, 3, 4, 5)], order.by = IFIX$Date)

##############

BDRX[ ,2:5] <- lapply(BDRX[,2:5], as.character)
BDRX[ ,2:5] <- lapply(BDRX[,2:5], function(X) gsub(",", "", X))
BDRX[ ,2:5] <- lapply(BDRX[,2:5], as.numeric)

BDRX <- BDRX[, c(1,3,4,5,2)]
names(BDRX) <- c("Date", "Open", "High", "Low", "Close")
BDRX$Date <- gsub(pattern = ",", replacement = "", x = BDRX$Date) %>%
  as.POSIXct(format = "%h %d %Y")
BDRX <- as.xts(BDRX[, c(2, 3, 4, 5)], order.by = BDRX$Date)

##################

IVBX[ ,2:5] <- lapply(IVBX[,2:5], as.character)
IVBX[ ,2:5] <- lapply(IVBX[,2:5], function(X) gsub(",", "", X))
IVBX[ ,2:5] <- lapply(IVBX[,2:5], as.numeric)

IVBX <- IVBX[, c(1,3,4,5,2)]
names(IVBX) <- c("Date", "Open", "High", "Low", "Close")
IVBX$Date <- gsub(pattern = ",", replacement = "", x = IVBX$Date) %>%
  as.POSIXct(format = "%h %d %Y")
IVBX <- as.xts(IVBX[, c(2, 3, 4, 5)], order.by = IVBX$Date)

#################

USD_BRL[ ,2:5] <- lapply(USD_BRL[,2:5], as.character)
USD_BRL[ ,2:5] <- lapply(USD_BRL[,2:5], function(X) gsub(",", "", X))
USD_BRL[ ,2:5] <- lapply(USD_BRL[,2:5], as.numeric)

USD_BRL <- USD_BRL[, c(1,3,4,5,2)]
names(USD_BRL) <- c("Date", "Open", "High", "Low", "Close")
USD_BRL$Date <- gsub(pattern = ",", replacement = "", x = USD_BRL$Date) %>%
  as.POSIXct(format = "%h %d %Y")
USD_BRL <- as.xts(USD_BRL[, c(2, 3, 4, 5)], order.by = USD_BRL$Date)

########################## Get Returns from datas ################################################################

BDRX_rets <- CalculateReturns(BDRX[,4])
IBOV_rets <- CalculateReturns(IBOV[,4])
ICON_rets <- CalculateReturns(ICON[,4])
IDIV_rets <- CalculateReturns(IDIV[,4])
IEE_rets <- CalculateReturns(IEE[,4])
IFIX_rets <- CalculateReturns(IFIX[,4])
IFNC_rets <- CalculateReturns(IFNC[,4])
IMAT_rets <- CalculateReturns(IMAT[,4])
IMOB_rets <- CalculateReturns(IMOB[,4])
INDX_rets <- CalculateReturns(INDX[,4])
IVBX_rets <- CalculateReturns(IVBX[,4])
MLCX_rets <- CalculateReturns(MLCX[,4])
SMLL_rets <- CalculateReturns(SMLL[,4])
USD_BRL_rets <- CalculateReturns(USD_BRL[,4])
UTIL_rets <- CalculateReturns(UTIL[,4])

all_Returns <- na.omit(merge(BDRX_rets, IBOV_rets, ICON_rets, IDIV_rets, IEE_rets,
                     IFIX_rets, IFNC_rets, IMAT_rets, IMOB_rets, INDX_rets, IVBX_rets, MLCX_rets, 
                     SMLL_rets, USD_BRL_rets,  UTIL_rets))

names(all_Returns) <- c("BDRX", "IBOV", "ICON", "IDIV", "IEE", "IFIX", "IFNC", "IMAT", "IMOB",
                        "INDX", "IVBX", "MLCX", "SMLL", "USD/BRL", "UTIL")

###################################################################################################################

charts.PerformanceSummary(all_Returns[ , c(9, 6, 14)], legend.loc = "left", main = "Performance Summary", 
                          colorset = c("black", "red", "green3", "blue", "cyan", "magenta", "yellow",
                                      "gray", "purple", "brown", "green", "pink", "bisque", "azure4", "aquamarine4"))

chart.Correlation(all_Returns)

all_Returns_Corr <- cor(all_Returns)
corrplot(all_Returns_Corr)

  
