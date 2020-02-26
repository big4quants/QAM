
# Q1. Using CRSP stock data, define the universe of monthly returns that can be used in calculating momentum portfolios, as well as their ranking return, following the procedure in Daniel and Moskowitz (2016) 1. Your output should be from 1927-2017. 

library(data.table)
library(zoo)
library(lubridate)
library(DataAnalytics)
library(moments)

PS3_Q1 <- function(universe){
  universe <- as.data.table(universe) # just to make sure
  universe <-  universe[(SHRCD %in% c(10, 11)) & (EXCHCD %in% c(1,2,3)),]
  # filter out the duplicates in the sample
  setkey(universe, NULL)
  universe <- unique(universe)
  
  universe[, date:= mdy(date)]
  universe[,Year:= year(date)] #year column
  universe[,Month:= month(date)] #month column
  universe <- universe[!RET %in% c(-99,-88,-77,-66,-55,-44)]
  universe <- universe[!DLRET %in% c(-99,-88,-77,-66,-55,-44)]
  universe[, RET := as.numeric(as.character(RET))]
  universe[, DLRET := as.numeric(as.character(DLRET))]
  universe[, PRC := abs(as.numeric(as.character(PRC)))]
  
  universe[, SHROUT := as.numeric(as.character(SHROUT))]
  universe[, `:=`(cumDivRet, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]
  universe[, `:=`(ME, abs(PRC * SHROUT * 1000))] # in dollar
  
  # get 1-month lag market capitalization
  universe[, YM := as.yearmon(date)]
  universe[, shifted_YM_1 := shift(YM), by = .(PERMNO)]
  universe[, lag_Mkt_Cap_flag := ifelse(YM-1/12 == shifted_YM_1, 1, 0)]
  universe[, ME_lag := ifelse(lag_Mkt_Cap_flag == 1, shift(ME), NA)]
  
  # create flag to determine if firm has at least 13 month of data
  universe[, shifted_YM_13 := shift(YM,13), by = PERMNO]
  universe[, shifted_YM_13flag := ifelse(YM - 13/12 == shifted_YM_13, 1, NA)]
  
  # clean and grab relevant columns 
  formation_universe <- universe[, .(PERMNO, EXCHCD, Year, Month, YM, cumDivRet, ME_lag, shifted_YM_13flag)]

  formation_universe[, plus_ret := cumDivRet+1]
  formation_universe[, Ranking_ret := ifelse(shifted_YM_13flag==1, log(Reduce(`*` , shift(plus_ret, 2:12))), NA)]
  
  CRSP_Stocks_Momentum <- na.omit(formation_universe[, .(Year, Month, PERMNO, EXCHCD, ME_lag, Ret = cumDivRet, Ranking_ret)])
  CRSP_Stocks_Momentum <- CRSP_Stocks_Momentum[!Ret==0]
  return(CRSP_Stocks_Momentum)
} # this is check!!


Monthly_CRSP_Stocks <- read.csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS3/CRSP.csv")
CRSP_Stocks_Momentum <- PS3_Q1(Monthly_CRSP_Stocks)
head(CRSP_Stocks_Momentum)
tail(CRSP_Stocks_Momentum)



# Q2. Define the monthly momentum portfolio decile of each stock as defined by both Daniel and Moskowitz (2016) and Kenneth R. French. Your output should be from 1927-2017.

PS3_Q2 <- function(CRSP_Stocks_Momentum){
  # define the categorical variable from 1 - 10 of which ranking_ret decile a stock belongs to
  CRSP_Stocks_Momentum[,DM_decile:= cut(Ranking_ret, breaks = quantile(Ranking_ret,probs=c(0:10)/10, type=5), labels = FALSE), by=c("Year","Month")]
  
  CRSP_Stocks_Momentum[,KRF_decile:=cut(Ranking_ret, breaks = c(-Inf, quantile(Ranking_ret[EXCHCD==1], probs=c(1:9)/10, type=5), Inf), labels=FALSE), by=c("Year","Month")]
  
  CRSP_Stocks_Momentum_decile <- CRSP_Stocks_Momentum[, .(Year, Month, PERMNO, ME_lag, Ret, DM_decile, KRF_decile)]
  
  return(CRSP_Stocks_Momentum_decile)
  }


CRSP_Stocks_Momentum_decile <- PS3_Q2(CRSP_Stocks_Momentum)
head(CRSP_Stocks_Momentum_decile)
tail(CRSP_Stocks_Momentum_decile)



# Q3. Calculate the monthly momentum portfolio decile returns as defined by both Daniel and Moskowitz (2016) and Kenneth R. French. Your output should be from 1927-2017.

PS3_Q3 <- function(CRSP_Stocks_Momentum_decile,FF_data){
  # to clean the date, ready for merge
  FF_data[, date:= paste0(X,"20")]
  FF_data[,date := ymd(date)]
  FF_data[,Year:= year(date)] #year column
  FF_data[,Month:= month(date)] #month column
  
  CRSP_Stocks_Momentum_rf <- merge(CRSP_Stocks_Momentum_decile, FF_data, by = c("Year", "Month"))
  CRSP_Stocks_Momentum_rf <- as.data.table(CRSP_Stocks_Momentum_rf) # just to make sure
  CRSP_Stocks_Momentum_rf <- CRSP_Stocks_Momentum_rf[,.(Year, Month, PERMNO, ME_lag, Ret, DM_decile, KRF_decile, Mkt.RF=Mkt.RF/100, RF=RF/100)]
  
  DM_Ret <- CRSP_Stocks_Momentum_rf[,list(DM_Ret = weighted.mean(Ret, ME_lag)), by = list(DM_decile, Year, Month)]
  KRF_Ret <- CRSP_Stocks_Momentum_rf[,list(KRF_Ret = weighted.mean(Ret, ME_lag)), by = list(KRF_decile, Year, Month)]
  
  DM_Ret <- DM_Ret[, decile := DM_decile]
  KRF_Ret <- KRF_Ret[, decile := KRF_decile]  
    
  CRSP_Stocks_Momentum_Return <- merge(DM_Ret, KRF_Ret, by=c("Year","Month","decile"))
  CRSP_Stocks_Momentum_Return <- merge(CRSP_Stocks_Momentum_Return, FF_data, by=c("Year","Month"))
  
  CRSP_Stocks_Momentum_Return <- CRSP_Stocks_Momentum_Return[, .(Year, Month, decile, DM_Ret, KRF_Ret, Mkt.RF=Mkt.RF/100, RF=RF/100)]
  
  return(CRSP_Stocks_Momentum_Return)
}


FF_data <- read.csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS3/F-F_Research_Data_Factors.csv")
FF_data <- as.data.table(FF_data)
CRSP_Stocks_Momentum_Return <- PS3_Q3(CRSP_Stocks_Momentum_decile,FF_data)
head(CRSP_Stocks_Momentum_Return)
tail(CRSP_Stocks_Momentum_Return[Year<2018])


# Q4. Replicate Table 1 in Daniel and Moskowitz (2016), except for a, t(a), beta, and sk(d) rows, and the Market column. Match the format and methodology to the extent possible.

PS3_Q4 <- function(CRSP_Stocks_Momentum_Return){
  # filter out the sample period as DM does
  Q4 <- CRSP_Stocks_Momentum_Return[Year >= 1927 & Year <= 2013]
  
  # excess return
  Q4[, ex_ret := DM_Ret-RF]
  # calculate statistic for 1~10 decile portfolios
  Table1 <- Q4[, .( Excess_Ret = mean(ex_ret)*12, 
                    Vol = sd(ex_ret)*sqrt(12),
                    SR = mean(ex_ret)/sd(ex_ret)*sqrt(12),
                    skewness = moments::skewness(log(1+DM_Ret)) ), by = .(decile)]
  # calculate WML column
  WML <- Q4[decile == 1 | decile == 10, .(Year, Month, decile, DM_Ret, RF)] # grab columns for decile 1 and 10 only
  WML_dcast <- dcast.data.table(WML, Year + Month + RF ~ decile, value.var = "DM_Ret")
  WML_dcast[, WML := `10` - `1`]
  WML_column <- WML_dcast[, .( decile = "WML",
                               Excess_Ret = mean(WML)*12, 
                               Vol = sd(WML)*sqrt(12),
                               SR = mean(WML)/sd(WML)*sqrt(12),
                               skewness = moments::skewness(log(1+WML+RF)) )]
  # combine WML column with table1
  Table1 <- rbind(Table1, WML_column)
  Table1[, c("Excess_Ret", "Vol") := .(100*Excess_Ret, 100*Vol)]
  
  return(t(Table1))
}

PS3_Q4(CRSP_Stocks_Momentum_Return)



# Q5. Calculate the correlation of your portfolio returns with the Daniel and Moskowitz (2016) breakpoints (by decile), to the portfolio returns on Daniel's website. Also calculate the correlation of your portfolio returns with the Kenneth R. French breakpoints (by decile), to the portfolio returns on French's website. Round to 4 decimal places. Correlations should be calculated from 1927-2017.

library(readxl)
DM_returns <- as.data.table(read_excel("DM_return.xlsx"))[,1:3]
KRF_returns <- as.data.table(read.csv("10_Portfolios_Prior_12_2.csv", header = T))

PS3_Q5 <- function(CRSP_Stocks_Momentum_returns, DM_returns, KRF_returns){
  # bunch of cleaning 
  colnames(KRF_returns) <- c("date", 1:10)
  KRF_returns[, date := ymd(paste0(date,"20"))]
  
  # clean the date and reshape the portfolio
  KRF_returns <- melt(KRF_returns, id=c("date"))
  setkey(KRF_returns, date)
  KRF_returns[, Year:= year(date)]
  KRF_returns[, Month:= month(date)]
  KRF_returns <- KRF_returns[Year<2019 & Year >1926]
  
  # change return to decimal
  KRF_returns[, value := value/100]
  KRF_returns[, date := NULL]
  colnames(KRF_returns) <- c("decile", "KRF_Ret_actual","Year", "Month")
  ################# done with KRF returns
  
  DM_returns[, Year:= year(ymd(date))]
  DM_returns[, Month:= month(ymd(date))]
  DM_returns[, date := NULL]
  DM_returns <- DM_returns[-(1:10),] # remove first month 
  result <- matrix(0, nrow=2,ncol=11)
  
  ################### correlation between DM estimate and actual 1927 to 2016
  DM_2016_estimate <- CRSP_Stocks_Momentum_Return[Year < 2017]
  for(i in 1:10){
    DM_Ret_estimate <- DM_2016_estimate[decile==i, DM_Ret]
    DM_Actual <- DM_returns[decile==i, ret]
    result[1,i] <- round(cor(DM_Ret_estimate, DM_Actual),4)
  }
   # corr for WML
  wml_est <-  DM_2016_estimate[decile ==10] - DM_2016_estimate[decile ==1]
  wml_actual <- DM_returns[decile ==10] - DM_returns[decile ==1]
  result[1, 11] <- round(cor(wml_est$DM_Ret, wml_actual$ret),4)
  
  
  KRF_returns <- KRF_returns[-(1:10),] # remove first month
  for(i in 1:10){
    KRF_Ret_estimate <- CRSP_Stocks_Momentum_Return[decile==i, KRF_Ret]
    KRF_Actual <- KRF_returns[decile==i, KRF_Ret_actual]
    result[2,i] <- round(cor(KRF_Ret_estimate, KRF_Actual),4)
  }
  
  ################### winner minus losers KRF 
  wml_est <-  CRSP_Stocks_Momentum_Return[decile ==10] - CRSP_Stocks_Momentum_Return[decile ==1]
  wml_actual <- KRF_returns[decile ==10, KRF_Ret_actual] - KRF_returns[decile ==1, KRF_Ret_actual]
  
  result[2, 11] <- round(cor(wml_est$KRF_Ret, wml_actual), 4)
  colnames(result) <- c("decile 1","decile 2","decile 3","decile 4","decile 5","decile 6","decile 7","decile 8","decile 9","decile 10","WML")
  rownames(result) <- c("DM","KRF")
  return(result)
}

Q5_result <- PS3_Q5(CRSP_Stocks_Momentum_returns, DM_returns, KRF_returns)

print(Q5_result)



# Q6. Has the momentum anomaly worked in the past few years? Show some empirical evidence.

Q5_09 <- CRSP_Stocks_Momentum_Return[Year < 2009]
Q5_09[, DM_cum_ret := cumsum(log(1+DM_Ret)), by=decile]
Q5_17 <- CRSP_Stocks_Momentum_Return[Year >= 2009]
Q5_17[, DM_cum_ret := cumsum(log(1+DM_Ret)), by=decile]


plot(Q5_09[decile==1, DM_cum_ret],type="l",ylim=c(-5,15),col="dodgerblue", main="Cumulative Return Per Portfolio from 1927 to 2009", ylab="Return %")
for(i in 2:10){
  lines(Q5_09[decile==i, DM_cum_ret],col=c(i))
}

plot(Q5_17[decile==1, DM_cum_ret],type="l",ylim=c(-0.5,2),col="dodgerblue",main="Cumulative Return Per Portfolio from 2009 to 2017", ylab="Return %")
for(i in 2:10){
  lines(Q5_17[decile==i, DM_cum_ret],col=c(i))
}

