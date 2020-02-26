
# 1. Construct the equal-weighted bond market return, value-weighted bond market return, and lagged total bond market capitalization using CRSP Bond data 1. Your output should be from January 1926 to December 2018, at a monthly frequency.

library(foreign)
library(data.table)
library(ggplot2)
library(lubridate)
library(moments)
library(readr)
library(zoo)

Monthly_CRSP_Stocks <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS1/Market data.csv")
Monthly_CRSP_Bonds <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS2/bond data.csv",col_types = cols(TMTOTOUT = col_integer()))

PS2_Q1 <- function(universe){
  #universe <- Monthly_CRSP_Bonds
  universe <- as.data.table(universe) # just to make sure
  # First step clean
  universe$MCALDT <- as.Date(universe$MCALDT,"%m/%d/%Y")
  setkey(universe, MCALDT)
  universe[,Year:= year(MCALDT)] #year column
  universe[,Month:= month(MCALDT)] #month column
  universe[, TMRETNUA := as.numeric(as.character(TMRETNUA))]
  universe <- universe[!TMRETNUA %in% c(-99)] # to get rid of -99
  universe[, TMTOTOUT := as.numeric(as.character(TMTOTOUT))]
  
  # market value = TMTOTOUT
  universe[, MV := TMTOTOUT]
  universe[, MV_lag := shift(MV), by=c("KYCRSPID")]
  
  # second step clean, remove NAs
  universe <- universe[TMRETNUA != "NA"]
  universe <- universe[MV_lag != "NA"]
  
  # value weight portfolio
  Bond_Vw_ret <- universe[,list(vwret = weighted.mean(TMRETNUA, MV_lag, na.rm = TRUE)), by=list(Year, Month)]
  
  # equal weight portfolio
  Bond_Ew_ret <- universe[,list(ewret = mean(TMRETNUA)),by=list(Year, Month)]
  
  # Total market value of the previous month
  Bond_lag_MV <- universe[,list(MV_lag = sum(MV_lag)), by=list(Year, Month)]
  output <- cbind(Bond_Vw_ret, Bond_Ew_ret[,3],Bond_lag_MV[,3])
  
  return(output)
}

PS2_1ans <- PS2_Q1(Monthly_CRSP_Bonds)

# 2. Aggregate stock, bond, and riskless datatables. For each year-month, calculate the lagged market value and excess value-weighted returns for both stocks and bonds. Your output should be from January 1926 to December 2018, at a monthly frequency.
## direct copy from PS_1
PS1_Q1 <- function(universe){
  universe <- as.data.table(universe) # just to make sure
  universe <-  universe[(SHRCD %in% c(10, 11)) & (EXCHCD %in% c(1,2,3)),]
  universe[, date:= mdy(date)]
  setkey(universe, date)
  universe[,Year:= year(date)] #year column
  universe[,Month:= month(date)] #month column
  universe <- universe[!RET %in% c(-99,-88,-77,-66,-55,-44)]
  universe <- universe[!DLRET %in% c(-99,-88,-77,-66,-55,-44)]
  universe[, RET := as.numeric(as.character(RET))]
  universe[, DLRET := as.numeric(as.character(DLRET))]
  universe[, PRC := as.numeric(as.character(PRC))]
  universe[, SHROUT := as.numeric(as.character(SHROUT))]
  universe[, `:=`(cumDivRet, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]
  universe[, `:=`(ME, abs(PRC * SHROUT * 1000))]
  universe[, ME_lag := shift(ME), by=c("PERMNO")]
  uuniverse <- universe[PRC != "NA"]
  universe <- universe[RET != "NA"]
  universe <- universe[cumDivRet != "NA"]
  universe <- universe[ME_lag != "NA"]
  value_weight <- universe[,list(vwret = weighted.mean(cumDivRet, ME_lag, na.rm = TRUE)), by=list(Year, Month)]
  equal_weight <- universe[,list(ewret = mean(cumDivRet)),by=list(Year, Month)]
  mkt_cap <- universe[,list(ME_lag = sum(ME_lag)/1000000), by=list(Year, Month)]
  mkt_ret <- cbind(value_weight, equal_weight[,3],mkt_cap[,3])
  
  return(mkt_ret)
}
PS1_1ans <- PS1_Q1(Monthly_CRSP_Stocks)
 
############## 
Monthly_CRSP_Riskless <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS2/Risk free data.csv")
PS2_Q2 <- function(PS1_1ans, PS2_1ans, Monthly_CRSP_Riskless){
  rf <- as.data.table(Monthly_CRSP_Riskless)
  rf[,caldt := ymd(caldt)]
  rf[,Year := year(caldt)]
  rf[,Month := month(caldt)]
 
  rf_table <- cbind(rf$Year,rf$Month,rf$t30ret) # 30-day rf as reference for risk free rate
  rf_table <- rf_table[-1,]
  
  Stock_Excess_Vw_Ret <- PS1_1ans[,3] - rf_table[,3]
  Bond_Excess_Vw_Ret <- PS2_1ans[,3] - rf_table[,3]
  
  result <- cbind(PS2_1ans[,c(1,2,5)],Bond_Excess_Vw_Ret, PS1_1ans[,5], Stock_Excess_Vw_Ret)
  colnames(result) <- c("Year", "Month", "Bond_lag_MV", "Bond_Excess_Vw_Ret", "Stock_lag_MV", "Stock_Excess_Vw_Ret")
  
  return(result)
}

PS2_2ans <- PS2_Q2(PS1_1ans, PS2_1ans, Monthly_CRSP_Riskless)



# 3. Calculate the monthly unlevered and levered risk-parity portfolio returns as defined by Asness, Frazzini, and Pedersen (2012). For the levered risk-parity portfolio, match the value-weighted portfolio's over the longest matched holding period of both. Your output should be from January 1926 to December 2018, at a monthly frequency.

PS2_Q3 <- function(Monthly_CRSP_Universe){
  # 60-40 portfolio return 
  Monthly_CRSP_Universe[,Excess_60_40_Ret := 0.6 * Stock_Excess_Vw_Ret + 0.4 * Bond_Excess_Vw_Ret]
  
  # vol as estimated by using three-year monthly excess returns up to month (t-1) 
  Monthly_CRSP_Universe[,Stock_inverse_sigma_hat := shift(1/rollapply(Stock_Excess_Vw_Ret,36, sd, fill=NA, align="right"))]
  Monthly_CRSP_Universe[,Bond_inverse_sigma_hat := shift(1/rollapply(Bond_Excess_Vw_Ret,36, sd, fill=NA, align="right"))]
  
  # unlevered RP where k = 1/sum of inverse of combined sigma
  Monthly_CRSP_Universe[,Unlevered_k := 1/(Stock_inverse_sigma_hat + Bond_inverse_sigma_hat)]
  # weight as defined as k * inverse of sigma 
  Monthly_CRSP_Universe[,Excess_Unlevered_RP_Ret := (Unlevered_k * Stock_inverse_sigma_hat * Stock_Excess_Vw_Ret 
                                                     + Unlevered_k * Bond_inverse_sigma_hat * Bond_Excess_Vw_Ret)]
  
  # to construct value weighted portfolio with stocks and bonds 
  Monthly_CRSP_Universe[,stock_Vw_Weight := Stock_lag_MV/(Stock_lag_MV + Bond_lag_MV)]
  Monthly_CRSP_Universe[,Excess_Vw_Ret := stock_Vw_Weight * Stock_Excess_Vw_Ret + (1 - stock_Vw_Weight) * Bond_Excess_Vw_Ret]
  
  # to get the constant levered K
  vol_levered_port <- sd(Monthly_CRSP_Universe$Stock_inverse_sigma_hat * Monthly_CRSP_Universe$Stock_Excess_Vw_Ret 
                         + Monthly_CRSP_Universe$Bond_inverse_sigma_hat * Monthly_CRSP_Universe$Bond_Excess_Vw_Ret, na.rm = TRUE)
  Monthly_CRSP_Universe[,Levered_k := sd(Monthly_CRSP_Universe$Excess_Vw_Ret)/vol_levered_port]
  
  # same as above for unlevered port
  Monthly_CRSP_Universe[,Excess_Levered_RP_Ret := Levered_k * Stock_inverse_sigma_hat * Stock_Excess_Vw_Ret 
                        + Levered_k * Bond_inverse_sigma_hat * Bond_Excess_Vw_Ret]
  
  result <- Monthly_CRSP_Universe[,.(Year, Month, Stock_Excess_Vw_Ret, Bond_Excess_Vw_Ret, Excess_Vw_Ret, Excess_60_40_Ret, 
                                     Stock_inverse_sigma_hat, Bond_inverse_sigma_hat, Unlevered_k,
                                     Excess_Unlevered_RP_Ret, Levered_k, Excess_Levered_RP_Ret)]
  result <- result[-(1:36),]
  return(result)
  }

PS2_3ans <- PS2_Q3(PS2_2ans)


# 4. Replicate and report Panel A of Table 2 in Asness, Frazzini, and Pedersen (2012), except for Alpha and t-stat of Alpha columns. Specifically, for all strategies considered, report the annualized average excess returns, t-statistic of the average excess returns, annualized volatility, annualized Sharpe Ratio, skewness, and excess kurtosis. Your sample should be from January 1930 to June 2010, at monthly frequency. Match the format of the table to the extent possible. Discuss the difference between your table and the table reported in the paper. It is zero? If not, justify whether the difference is economically negligible or not. What are the reasons a nonzero difference?

PS2_Q4 <- function(Port_Rets){
  # #January 1930 to June 2010
  Port_Rets[, date:= ymd(paste0(Year,'/',Month,'/01'))] #combined Year and Month
  Port_Rets <- Port_Rets[date %between% c("1930-01-01", "2010-06-01")]
  
  # CRSP_stock
  crsp_stock <- c(0)
  crsp_stock[1] <- mean(Port_Rets$Stock_Excess_Vw_Ret, na.rm = TRUE) * 12 *100 #add %
  crsp_stock[2] <- t.test(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE)[1] # t-stats
  crsp_stock[3] <- sd(Port_Rets$Stock_Excess_Vw_Ret, na.rm = TRUE) * sqrt(12)*100
  crsp_stock[4] <- crsp_stock[[1]]/crsp_stock[[3]]
  crsp_stock[5] <- skewness(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE)
  crsp_stock[6] <- kurtosis(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE) -3
  
  # CRSP_bonds
  crsp_bonds <- c()
  crsp_bonds[1] <-mean(Port_Rets$Bond_Excess_Vw_Ret, na.rm = TRUE) * 12 *100
  crsp_bonds[2] <- t.test(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE)[1]
  crsp_bonds[3] <- sd(Port_Rets$Bond_Excess_Vw_Ret, na.rm = TRUE) * sqrt(12)*100
  crsp_bonds[4] <- crsp_bonds[[1]]/crsp_bonds[[3]]
  crsp_bonds[5] <- skewness(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE)
  crsp_bonds[6] <- kurtosis(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE) -3
  
  # Value weight return
  VW_Portfolio <- c()
  VW_Portfolio[1] <-mean(Port_Rets$Excess_Vw_Ret, na.rm = TRUE) * 12 *100
  VW_Portfolio[2] <- t.test(Port_Rets$Excess_Vw_Ret,na.rm = TRUE)[1]
  VW_Portfolio[3] <- sd(Port_Rets$Excess_Vw_Ret, na.rm = TRUE) * sqrt(12)*100
  VW_Portfolio[4] <- VW_Portfolio[[1]]/VW_Portfolio[[3]]
  VW_Portfolio[5] <- skewness(Port_Rets$Excess_Vw_Ret,na.rm = TRUE)
  VW_Portfolio[6] <- kurtosis(Port_Rets$Excess_Vw_Ret,na.rm = TRUE) -3
  
  # 60/40 portfolio
  Excess_60_40_Portfolio <- c()
  Excess_60_40_Portfolio[1] <-mean(Port_Rets$Excess_60_40_Ret, na.rm = TRUE) * 12 *100
  Excess_60_40_Portfolio[2] <- t.test(Port_Rets$Excess_60_40_Ret,na.rm = TRUE)[1]
  Excess_60_40_Portfolio[3] <- sd(Port_Rets$Excess_60_40_Ret, na.rm = TRUE) * sqrt(12)*100
  Excess_60_40_Portfolio[4] <- Excess_60_40_Portfolio[[1]]/Excess_60_40_Portfolio[[3]]
  Excess_60_40_Portfolio[5] <- skewness(Port_Rets$Excess_60_40_Ret,na.rm = TRUE)
  Excess_60_40_Portfolio[6] <- kurtosis(Port_Rets$Excess_60_40_Ret,na.rm = TRUE) -3
  
  # unlevered RP
  Unlevered_RP_Portfolio <- c()
  Unlevered_RP_Portfolio[1] <-mean(Port_Rets$Excess_Unlevered_RP_Ret, na.rm = TRUE) * 12 *100
  Unlevered_RP_Portfolio[2] <- t.test(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE)[1]
  Unlevered_RP_Portfolio[3] <- sd(Port_Rets$Excess_Unlevered_RP_Ret, na.rm = TRUE) * sqrt(12)*100
  Unlevered_RP_Portfolio[4] <- Unlevered_RP_Portfolio[[1]]/Unlevered_RP_Portfolio[[3]]
  Unlevered_RP_Portfolio[5] <- skewness(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE)
  Unlevered_RP_Portfolio[6] <- kurtosis(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE) -3
  
  # levered RP
  Levered_RP_Portfolio <- c()
  Levered_RP_Portfolio[1] <-mean(Port_Rets$Excess_Levered_RP_Ret, na.rm = TRUE) * 12 *100
  Levered_RP_Portfolio[2] <- t.test(Port_Rets$Excess_Levered_RP_Ret,na.rm = TRUE)[1]
  Levered_RP_Portfolio[3] <- sd(Port_Rets$Excess_Levered_RP_Ret, na.rm = TRUE) * sqrt(12)*100
  Levered_RP_Portfolio[4] <- Levered_RP_Portfolio[[1]]/Levered_RP_Portfolio[[3]]
  Levered_RP_Portfolio[5] <- skewness(Port_Rets$Excess_Levered_RP_Ret,na.rm = TRUE)
  Levered_RP_Portfolio[6] <- kurtosis(Port_Rets$Excess_Levered_RP_Ret,na.rm = TRUE) -3
  
  result <- cbind(crsp_stock, crsp_bonds, VW_Portfolio, Excess_60_40_Portfolio, 
                  Unlevered_RP_Portfolio, Levered_RP_Portfolio)
  result <- t(result)
  colnames(result) <- c("Annualized Mean %","t-stat of Annualized Mean","Annualized Standard Deviation %",
                        "Annualized Sharpe Ratio","Skewness","Excess Kurtosis")
  return(result)
}
PS2_4ans <- PS2_Q4(PS2_3ans)



