# *************************************************************** #
# *************************************************************** #
# WIKI PRICES TRADING MODEL
# *************************************************************** #
# *************************************************************** #
# - Carson Goeke 
# - 11/30/2017
#
# This script is used to pick stocks from Quandl's WIKI/PRICES dataset.
# It alose makes use of their Federal Reserve Economic Data (FRED), as
# well NASDAQ's industy codes

# Load Packages ---------------------------------------------------------------------------------
library(Quandl) # data API
library(ggplot2) # data visualization
#library(TTR)
library(DataCombine) # times series variables  
library(magrittr) # pipe operator
library(data.table) # fread
library(keras) # neural network
library(ggplot2) # visualization
library(ggridges) # ridge plot extension
library(dplyr) # data munging / grouping / summarising

# Load Data ---------------------------------------------------------------------------------

# Quandl API key
api.key <- 'sEsKbUQEbjvokfsfpUzo'
Quandl.api_key(api.key)

# Load Stock Prices
wiki.prices <- fread("~/Desktop/wiki_prices/WIKI_PRICES_212b326a081eacca455e13140d7bb9db.csv") %>% 
 as.data.frame(stringsAsFactors = FALSE)

# Preprocessing Data ---------------------------------------------------------------------------------

# Changing data types
wiki.prices$date %<>% as.Date()
wiki.prices$adj_volume %<>% as.numeric()
wiki.prices$split_ratio %<>% as.numeric()

# keep only variables adjusted for stock splits and such
not.adjusted <- c("open", "high", "low", "close", "volume")
wiki.prices[,not.adjusted] <- NULL

# calculating log returns
# first get lagged adjusted close
wiki.prices <- slide(wiki.prices,
                     Var = "adj_close",
                     TimeVar = 'date',
                     GroupVar = "ticker",
                     slideBy = 1,
                     NewVar = "adj_close_1")

# calculate returns from lagged values
wiki.prices$return <- (wiki.prices$adj_close - wiki.prices$adj_close_1) / wiki.prices$adj_close_1
wiki.prices <- na.omit(wiki.prices)

# Getting industry classification
industry.files <- list.files("~/Desktop/industries")
industries <- read.csv(paste0("~/Desktop/industries/",industry.files[1]), stringsAsFactors = FALSE)
for (i in 2:length(industry.files)) {
  industries %<>% rbind(read.csv(paste0("~/Desktop/industries/",industry.files[i]),stringsAsFactors = FALSE))
}

# only keep stocks we have industry info on
wiki.prices <- wiki.prices[wiki.prices$ticker %in% industries$Symbol,]
colnames(industries)[1] <- "ticker"
industries <- industries[,c("ticker", "Sector")]

# merging with orginal df
wiki.prices <- merge(wiki.prices, industries, by = "ticker", all.x = TRUE)
colnames(wiki.prices) <- gsub(" ", ".", colnames(wiki.prices))
colnames(wiki.prices) <- gsub("-", "_", colnames(wiki.prices))

# plots ------------------------------------------------------------------------

# Ridges
ridge_return <- wiki.prices %>%
  ggplot(aes(return, Sector, fill = Sector)) +
  geom_density_ridges(bandwidth = 2) +
  ggtitle('Daily Stock Returns by Sector') +
  theme(legend.position = "none")  +
  xlab('One Day Return (Closing Price)') +
  ylab('Sector') +
  scale_fill_cyclical(values = c("#3E99F6", "#FFC300"))

wiki.prices$alr <- log(wiki.prices$return + 1)
ridge_return

ridge_alr <- wiki.prices %>%
  ggplot(aes(alr, Sector, fill = Sector)) +
  geom_density_ridges(bandwidth = 2) +
  ggtitle('Daily Stock Log Returns by Sector') +
  theme(legend.position = "none")  +
  xlab('One Day Log Return (Closing Price)') +
  ylab('Sector') +
  scale_fill_cyclical(values = c("#3E99F6", "#FFC300"))
ridge_alr

# Points
point_return <- wiki.prices %>% group_by(Sector) %>% summarise(mean_return = mean(return)) %>%
  ggplot(aes(mean_return, Sector)) +
    geom_point() +
    ggtitle('Mean Daily Return by Sector') +
    xlab('Average Daily') +
    ylab('Sector')
point_return

point_alr <- wiki.prices %>% group_by(Sector) %>% summarise(mean_alr = mean(alr)) %>%
  ggplot(aes(mean_alr, Sector)) +
  geom_point() +
  ggtitle('Mean Daily Log Return by Sector') +
  xlab('Average Log Return') +
  ylab('Sector')
point_alr

# Smoothers
smooth_return <- wiki.prices %>% group_by(date, Sector) %>% summarise(mean_return=mean(return)) %>%
  ggplot(aes(date, mean_return, color = Sector, fill = Sector)) +
  geom_smooth(span=0.1, se=FALSE) +
  ggtitle('Average Daily Returns over Time by Sector') +
  xlab('Date') +
  ylab('Average Daily Return')
smooth_return

smooth_alr <- wiki.prices %>% group_by(date, Sector) %>% summarise(mean_alr= mean(alr)) %>%
  ggplot(aes(date, mean_alr, color = Sector, fill = Sector)) +
  geom_smooth(se=FALSE) +
  ggtitle('Average Daily Log Returns over Time by Sector') +
  xlab('Date') +
  ylab('Average Daily Return')
smooth_alr

# convert industry to indicator variables
sectors <- as.data.frame(model.matrix(~industries$Sector - 1))
colnames(sectors) <- levels(as.factor(industries$Sector))
industries <- as.data.frame(cbind(industries$ticker, sectors), stringsAsFactors = FALSE)
rm(sectors)
colnames(industries)[1] <- "ticker"




# /////////////////////////////////////////////////////////////// #
# Data Exploration
# /////////////////////////////////////////////////////////////// #

# Over 3100 total tickers
tickers <- levels(as.factor(wiki.prices$ticker))

# Get Industry for each ticker



most.recent <- wiki.prices[wiki.prices$date == max(wiki.prices$date),]
cheap <- most.recent[most.recent$adj_close < 30, ]
wiki.prices <- wiki.prices[wiki.prices$ticker %in% cheap$ticker,]
wiki.prices <- wiki.prices[order(wiki.prices$date),]
wiki.prices <- wiki.prices[order(wiki.prices$ticker),]
tickers <- levels(as.factor(wiki.prices$ticker))
wiki.prices$one.week.avg <- NA
wiki.prices$one.month.avg <- NA
#wiki.prices$three.month.avg <- NA

# We want to calculate the moving average for each ticker
library(progress)
pb <- progress_bar$new(total = length(tickers))
for (i in 1:length(tickers)) {
  ticker = tickers[i]
  series     <- ts(wiki.prices[wiki.prices$ticker == ticker,"adj_close"])
  one.week.avg    <- as.numeric(SMA(series, n = 5)) # markets are only open 5 days a week
  one.month.avg   <- as.numeric(SMA(series, n = 20)) # four, five-day weeks
  #three.month.avg <- as.numeric(SMA(series, n = 60)) # 3 months
  wiki.prices[wiki.prices$ticker == ticker, "one.week.avg"]    <- one.week.avg
  wiki.prices[wiki.prices$ticker == ticker, "one.month.avg"]   <- one.month.avg
 # wiki.prices[wiki.prices$ticker == ticker, "three.month.avg"] <- three.month.avg
  pb$tick()
}

# make one week worth of lags
# use the slide function to create lagged variables for wiki.prices.data
wiki.prices <- slide(wiki.prices,
                         Var = "adj_open",
                         GroupVar = "ticker",
                         slideBy = -1,
                         NewVar = "open.1")
wiki.prices <- slide(wiki.prices,
                      Var = "adj_close",
                      GroupVar = "ticker",
                      slideBy = -1,
                      NewVar = "close.1")
wiki.prices <- slide(wiki.prices,
                      Var = "adj_high",
                      GroupVar = "ticker",
                      slideBy = -1,
                      NewVar = "high.1")
wiki.prices <- slide(wiki.prices,
                      Var = "adj_low",
                      GroupVar = "ticker",
                      slideBy = -1,
                      NewVar = "low.1")
wiki.prices <- slide(wiki.prices,
                      Var = "adj_volume",
                      GroupVar = "ticker",
                      slideBy = -1,
                      NewVar = "volume.1")

# week of close in future (to predict)
wiki.prices <- slide(wiki.prices,
                      Var = "adj_close",
                      GroupVar = "ticker",
                      slideBy = 1,
                      NewVar = "future.close.1")
wiki.prices <- slide(wiki.prices,
                      Var = "adj_close",
                      GroupVar = "ticker",
                      slideBy = 2,
                      NewVar = "future.close.2")
wiki.prices <- slide(wiki.prices,
                      Var = "adj_close",
                      GroupVar = "ticker",
                      slideBy = 3,
                      NewVar = "future.close.3")
wiki.prices <- slide(wiki.prices,
                      Var = "adj_close",
                      GroupVar = "ticker",
                      slideBy = 4,
                      NewVar = "future.close.4")
wiki.prices <- slide(wiki.prices,
                      Var = "adj_close",
                      GroupVar = "ticker",
                      slideBy = 5,
                      NewVar = "future.close.5")

# /////////////////////////////////////////////////////////////// #
# Economic Indicators from Federal Reserve
# /////////////////////////////////////////////////////////////// #

# Gross Domestic Product
fred   <- Quandl('FRED/GDP')
colnames(fred)[2] <- 'GDP'

# /////////////////////////////////////////////////////////////// #
# Prices / Inflation
# /////////////////////////////////////////////////////////////// #

# Consumer Price Index for All Urban Consumers: All Items
CPI <- Quandl('FRED/CPIAUCSL')
colnames(CPI)[2] <- "CPI"
fred <- merge(fred, CPI, by = "Date", all = TRUE)
rm(CPI)

# Consumer Price Index for All Urban Consumers: All Items Less Food & Energy
CPI.nfne <- Quandl('FRED/CPILFESL') 
colnames(CPI.nfne)[2] <- "CPI.nfne"
fred <- merge(fred, CPI.nfne, by = "Date", all = TRUE)
rm(CPI.nfne)

# /////////////////////////////////////////////////////////////// #
# Money Supply
# /////////////////////////////////////////////////////////////// #

# St. Louis Adjusted Monetary Base
BASE <- Quandl('FRED/BASE') 
colnames(BASE)[2] <- "BASE"
fred <- merge(fred, BASE, by = "Date", all = TRUE)
rm(BASE)

# M1 Money Stock
M1   <- Quandl('FRED/M1') 
colnames(M1)[2] <- "M1"
fred <- merge(fred, M1, by = "Date", all = TRUE)
rm(M1)

# M2 Money Stock
M2   <- Quandl('FRED/M2') 
colnames(M2)[2] <- "M2"
fred <- merge(fred, M2, by = "Date", all = TRUE)
rm(M2)

# Velocity of M1 Money Stock
M1V  <- Quandl('FRED/M1V') 
colnames(M1V)[2] <- "M1V"
fred <- merge(fred, M1V, by = "Date", all = TRUE)
rm(M1V)

# Velocity of M2 Money Stock
M2V  <- Quandl('FRED/M2V') 
colnames(M2V)[2] <- "M2V"
fred <- merge(fred, M2V, by = "Date", all = TRUE)
rm(M2V)

# /////////////////////////////////////////////////////////////// #
# Interest Rates
# /////////////////////////////////////////////////////////////// #

# Effective Federal Funds Rate
DFF     <- Quandl('FRED/DFF') 
colnames(DFF)[2] <- "DFF"
fred <- merge(fred, DFF, by = "Date", all = TRUE)
rm(DFF)

# 3-Month Treasury Bill: Secondary Market Rate
DTB3    <- Quandl('FRED/DTB3') 
colnames(DTB3)[2] <- "DTB3"
fred <- merge(fred, DTB3, by = "Date", all = TRUE)
rm(DTB3)

# 5-Year Treasury Constant Maturity Rate
DGS5    <- Quandl('FRED/DGS5') 
colnames(DGS5)[2] <- "DGS5"
fred <- merge(fred, DGS5, by = "Date", all = TRUE)
rm(DGS5)

# 10-Year Treasury Constant Maturity Rate
DGS10   <- Quandl('FRED/DGS10') 
colnames(DGS10)[2] <- "DGS10"
fred <- merge(fred, DGS10, by = "Date", all = TRUE)
rm(DGS10)

# 30-Year Treasury Constant Maturity Rate
DGS30   <- Quandl('FRED/DGS30') 
colnames(DGS30)[2] <- "DGS30"
fred <- merge(fred, DGS30, by = "Date", all = TRUE)
rm(DGS30)

# 5-year Breakeven Inflation Rate
T5YIE   <- Quandl('FRED/T5YIE') 
colnames(T5YIE)[2] <- "T5YIE"
fred <- merge(fred, T5YIE, by = "Date", all = TRUE)
rm(T5YIE)

# 10-year Breakeven Inflation Rate
T10YIE  <- Quandl('FRED/T10YIE')
colnames(T10YIE)[2] <- "T10YIE"
fred <- merge(fred, T10YIE, by = "Date", all = TRUE)
rm(T10YIE)

# 5-Year, 5-Year Forward Inflation Expectation Rate
T5YIFR  <- Quandl('FRED/T5YIFR') 
colnames(T5YIFR)[2] <- "T5YIFR"
fred <- merge(fred, T5YIFR, by = "Date", all = TRUE)
rm(T5YIFR)

# TED Spread
TEDRATE <- Quandl('FRED/TEDRATE') 
colnames(TEDRATE)[2] <- "TEDRATE"
fred <- merge(fred, TEDRATE, by = "Date", all = TRUE)
rm(TEDRATE)

# Bank Prime Loan Rate
DPRIME  <- Quandl('FRED/DPRIME') 
colnames(DPRIME)[2] <- "DPRIME"
fred <- merge(fred, DPRIME, by = "Date", all = TRUE)
rm(DPRIME)

# /////////////////////////////////////////////////////////////// #
# Employment
# /////////////////////////////////////////////////////////////// #

# Civilian Unemployment Rate
UNRATE   <- Quandl('FRED/UNRATE') 
colnames(UNRATE)[2] <- "UNRATE"
fred <- merge(fred, UNRATE, by = "Date", all = TRUE)
rm(UNRATE)

# Natural Rate of Unemployment (Long-Term)
NROU     <- Quandl('FRED/NROU') 
colnames(NROU)[2] <- "NROU"
fred <- merge(fred, NROU, by = "Date", all = TRUE)
rm(NROU)

#	Natural Rate of Unemployment (Short-Term)
NROUST   <- Quandl('FRED/NROUST') 
colnames(NROUST)[2] <- "NROUST"
fred <- merge(fred, NROUST, by = "Date", all = TRUE)
rm(NROUST)

# Civilian Labor Force Participation Rate
CIVPART  <- Quandl('FRED/CIVPART') 
colnames(CIVPART)[2] <- "CIVPART"
fred <- merge(fred, CIVPART, by = "Date", all = TRUE)
rm(CIVPART)

# Civilian Employment-Population Ratio
EMRATIO  <- Quandl('FRED/EMRATIO') 
colnames(EMRATIO)[2] <- "EMRATIO"
fred <- merge(fred, EMRATIO, by = "Date", all = TRUE)
rm(EMRATIO)

# Unemployed
UNEMPLOY <- Quandl('FRED/UNEMPLOY') 
colnames(UNEMPLOY)[2] <- "UNEMPLOY"
fred <- merge(fred, UNEMPLOY, by = "Date", all = TRUE)
rm(UNEMPLOY)

# /////////////////////////////////////////////////////////////// #
# Income and Expenditure
# /////////////////////////////////////////////////////////////// #

# Real Median Household Income in the United States
MEHOINUSA672N  <- Quandl('FRED/MEHOINUSA672N') 
colnames(MEHOINUSA672N)[2] <- "MEHOINUSA672N"
fred <- merge(fred, MEHOINUSA672N, by = "Date", all = TRUE)
rm(MEHOINUSA672N)

# Real Disposable Personal Income
DSPIC96   <- Quandl('FRED/DSPIC96') 
colnames(DSPIC96)[2] <- "DSPIC96"
fred <- merge(fred, DSPIC96, by = "Date", all = TRUE)
rm(DSPIC96)

# Personal Consumption Expenditures
PCE     <- Quandl('FRED/PCE') 
colnames(PCE)[2] <- "PCE"
fred <- merge(fred, PCE, by = "Date", all = TRUE)
rm(PCE)

# Personal Savings Rate
PSAVERT <- Quandl('FRED/PSAVERT') 
colnames(PSAVERT)[2] <- "PSAVERT"
fred <- merge(fred, PSAVERT, by = "Date", all = TRUE)
rm(PSAVERT)

# Disposable Personal Income
DSPI    <- Quandl('FRED/DSPI') 
colnames(DSPI)[2] <- "DSPI"
fred <- merge(fred, DSPI, by = "Date", all = TRUE)
rm(DSPI)

# /////////////////////////////////////////////////////////////// #
# Other Economic Indicators
# /////////////////////////////////////////////////////////////// #
# Industrial Production Index
INDPRO  <- Quandl('FRED/INDPRO') 
colnames(INDPRO)[2] <- "INDPRO"
fred <- merge(fred, INDPRO, by = "Date", all = TRUE)
rm(INDPRO)

# Capacity Utilization: Total Industry
TCU     <- Quandl('FRED/TCU') 
colnames(TCU)[2] <- "TCU"
fred <- merge(fred, TCU, by = "Date", all = TRUE)
rm(TCU)

# Housing Starts: Total: New Privately Owned Housing Units Started
HOUST   <- Quandl('FRED/HOUST') 
colnames(HOUST)[2] <- "HOUST"
fred <- merge(fred, HOUST, by = "Date", all = TRUE)
rm(HOUST)

# Gross Private Domestic Investment
GPDI    <- Quandl('FRED/GPDI') 
colnames(GPDI)[2] <- "GPDI"
fred <- merge(fred, GPDI, by = "Date", all = TRUE)
rm(GPDI)

# Corporate Profits After Tax (without IVA and CCAdj)
CP      <- Quandl('FRED/CP') 
colnames(CP)[2] <- "CP"
fred <- merge(fred, CP, by = "Date", all = TRUE)
rm(CP)

# St. Louis Fed Financial Stress Index
STLFSI  <- Quandl('FRED/STLFSI') 
colnames(STLFSI)[2] <- "STLFSI"
fred <- merge(fred, STLFSI, by = "Date", all = TRUE)
rm(STLFSI)

# Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma
DCOILWTICO     <- Quandl('FRED/DCOILWTICO') 
colnames(DCOILWTICO)[2] <- "DCOILWTICO"
fred <- merge(fred, DCOILWTICO, by = "Date", all = TRUE)
rm(DCOILWTICO)

# Leading Index for the United States
USSLIND <- Quandl('FRED/USSLIND') 
colnames(USSLIND)[2] <- "USSLIND"
fred <- merge(fred, USSLIND, by = "Date", all = TRUE)
rm(USSLIND)

# Trade Weighted U.S. Dollar Index: Major Currencies
DTWEXM  <- Quandl('FRED/DTWEXM') 
colnames(DTWEXM)[2] <- "DTWEXM"
fred <- merge(fred, DTWEXM, by = "Date", all = TRUE)
rm(DTWEXM)

# /////////////////////////////////////////////////////////////// #
# Debt
# /////////////////////////////////////////////////////////////// #
# Federal Debt: Total Public Debt
GFDEBTN     <- Quandl('FRED/GFDEBTN') 
colnames(GFDEBTN)[2] <- "GFDEBTN"
fred <- merge(fred, GFDEBTN, by = "Date", all = TRUE)
rm(GFDEBTN)

# Federal Debt: Total Public Debt as Percent of Gross Domestic Product
GFDEGDQ188S <- Quandl('FRED/GFDEGDQ188S') 
colnames(GFDEGDQ188S)[2] <- "GFDEGDQ188S"
fred <- merge(fred, GFDEGDQ188S, by = "Date", all = TRUE)
rm(GFDEGDQ188S)

# Excess Reserves of Depository Institutions
EXCSRESNW    <- Quandl('FRED/EXCSRESNW') 
colnames(EXCSRESNW)[2] <- "EXCSRESNW"
fred <- merge(fred, EXCSRESNW, by = "Date", all = TRUE)
rm(EXCSRESNW)

# Commercial and Industrial Loans, All Commercial Banks
TOTCI   <- Quandl('FRED/TOTCI') 
colnames(TOTCI)[2] <- "TOTCI"
fred <- merge(fred, TOTCI, by = "Date", all = TRUE)
rm(TOTCI)

# Make sure no future dates
fred <- fred[fred$Date <= Sys.Date(),]

# Interpolate NA values
# should just be last non.na vlaue
colnames(fred)[1] <- "date"
dates <- wiki.prices$date
dates <- as.data.frame(dates[!duplicated(dates)])
colnames(dates)[1] <- "date"
fred <- merge(fred, dates, by = "date", all = TRUE)
fred <- as.zoo(fred)
fred <- as.data.frame(na.locf(fred, fromLast = FALSE, na.rm = FALSE))
fred$date <- as.Date(fred$date)
fred[,2:43] <- sapply(fred[,2:43], as.numeric)

wiki.prices <- merge(wiki.prices, fred, by = "date", all.x = TRUE)
rm(fred)

# we can check which tickers are still active by seeing how many observations
# there are with the most recent date
most.recent <- wiki.prices[wiki.prices$date == max(wiki.prices$date),]

# which are the lowest price
cheap <- most.recent[most.recent$adj_close < 20, ]

# need to have high volume
cheap <- cheap[order(cheap$adj_volume, decreasing = TRUE),]
cheap <- cheap[1:(nrow(cheap)/4),]

# need to be moving a lot
cheap <- cheap[order(abs(cheap$velocity), decreasing = TRUE),]
high.velocity <- cheap[1:15,]

# lets subset the ordiginal data with our tickers of interest
history <- wiki.prices[(wiki.prices$ticker %in% high.velocity$ticker),]

# we dont need all the historical data
last.3.months <- history[history$date > (max(history$date) - 90),]

# We can take a look at the historical trends
g <- ggplot(last.3.months, aes(date, adj_close, color = ticker, fill = ticker)) +
  geom_point() +
  geom_smooth()
g

# get blue chips as well
# We'll use the DOW 30
DOW <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS",
         "DIS", "XOM", "GE", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", 
         "MCD", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UTX",
         "UNH", "VZ", "V", "WMT")
#DOW.prices <- wiki.prices[wiki.prices$ticker %in% DOW,]

# Calculate moving averages
#cheap.stocks <- wiki.prices[(wiki.prices$ticker %in% cheap$ticker),]
#cheap.stocks <- rbind(cheap.stocks, DOW.prices)

# /////////////////////////////////////////////////////////////// #
# Basic Model
# /////////////////////////////////////////////////////////////// #
wiki.prices$three.month.avg <- NULL
wiki.prices$Miscellaneous <- NULL
wiki.prices$ex.dividend <- NULL
this.weekend <- wiki.prices[wiki.prices$date == max(wiki.prices$date),]

# get numeric columns
num.cols <- sapply(wiki.prices, is.numeric)
nums <- na.omit(wiki.prices[,num.cols])

## 75% of the sample size
smp_size <- floor(0.5 * nrow(nums))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(nums)), size = smp_size)

# using matrix algebra for ols model with 5 response variables (future time stepss)
xs <- nums[train_ind,!grepl("future", colnames(nums))]
ys <- nums[train_ind,grepl("future", colnames(nums))]
proj <- t(as.matrix(xs)) %*% as.matrix(ys)
cov <- t(as.matrix(xs)) %*% as.matrix(xs)
inv <- solve(cov,tol = 1e-20)
bs <- inv %*% proj # coefficients

# test accuracy by calculating r2 for each stock
tickers <- levels(as.factor(this.weekend$ticker))
tests <- na.omit(wiki.prices)
tests <- tests[-train_ind,]
r2.df <- as.data.frame(tickers)
r2.df$r2 <- 0
pb <- progress_bar$new(total = length(tickers))
for (i in 1:length(tickers)) {
  
   temp <- tests[tests$ticker == tickers[i], num.cols]
   temp.x <- as.matrix(temp[,!grepl("future", colnames(temp))])
   temp.y <- as.matrix(temp[,grepl("future", colnames(temp))])
   temp.est <- temp.x %*% bs
   temp.err <- temp.y - temp.est
   r2.df$r2[i] <- 1 - sum(temp.err^2) / (sum(temp.y^2))
   pb$tick()
}
rm(temp)
rm(temp.x)
rm(temp.y)
rm(temp.err)
rm(temp.est)

wiki.net <- nnet(x = as.matrix(xs),
                 y = as.matrix(ys),
                 size = 10,
                 linout = TRUE,
                 Hess = TRUE)

colnames(r2.df)[1] <- "ticker"
this.weekend <- merge(this.weekend, r2.df, by = "ticker", all.x = TRUE)
this.weekend <- this.weekend[this.weekend$r2 > .90,]
next.week.nums <- this.weekend[,num.cols]

# predict this coming week
this.weekend[,grepl("future", colnames(this.weekend))] <- as.matrix(next.week.nums[,!grepl("future", colnames(next.week.nums))]) %*% bs
this.weekend$projected.growth <- ((this.weekend$future.close.5 - this.weekend$adj_close) +
                                (this.weekend$future.close.4 - this.weekend$adj_close) +
                                  (this.weekend$future.close.3 - this.weekend$adj_close) +
                                (this.weekend$future.close.2 - this.weekend$adj_close) +
                                  (this.weekend$future.close.1 - this.weekend$adj_close)) /
                                 (5*this.weekend$adj_close)

this.weekend <- this.weekend[order(this.weekend$projected.growth, decreasing = TRUE),]
this.weekend <- this.weekend[this.weekend$projected.growth < 1, ]

# subset companies that we are interested in
cheap.tickers <- levels(as.factor(this.weekend$ticker[1:10]))
forecast <- this.weekend[,c("ticker","adj_close", "date", "projected.growth")]
forecast <- forecast[forecast$ticker %in% cheap.tickers,]
forecast$date <- as.character(forecast$date)
forecast$adj_close <- as.character(forecast$adj_close)
forecast$projected.growth <- as.character(forecast$projected.growth)

# reshape for visualization
for (i in 1:length(cheap.tickers)) {
  temp <- this.weekend[this.weekend$ticker == cheap.tickers[i],]
  adj_close <- as.numeric(temp[1,28:32])
  ticker <- as.character(rep(temp$ticker[1], 5))
  dates <-  as.character(c(as.Date(temp$date[1]+1),
             as.Date(temp$date[1]+2),
             as.Date(temp$date[1]+3),
             as.Date(temp$date[1]+4),
             as.Date(temp$date[1]+5)))
  projected.growth <- as.character(rep(temp$projected.growth[1], 5))
  temp <- as.data.frame(cbind(ticker, adj_close, dates, projected.growth), stringsAsFactors = FALSE)
  colnames(temp) <- colnames(forecast)
  forecast <- rbind(forecast, temp)
}

# fixing types before ggplot
forecast$adj_close <- as.numeric(forecast$adj_close)
forecast$projected.growth <- as.numeric(forecast$projected.growth)
forecast$date <- as.Date(forecast$date)

# graphics
g <- ggplot(forecast, aes(date, adj_close, color = ticker, fill = ticker)) +
  geom_point() + 
  geom_smooth() #+
  #theme(legend.position="none")
g

