
library(foreign)
library(data.table)
library(ggplot2)
library(lubridate)
library(moments)
library(readr)

Market_data <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/Market data.csv")
Market_data <- as.data.table(Market_data)

####################################################### PS1_1 #######################################################
PS1_Q1 <- function(universe){
  # Filter sharecode 10 and 11 and EXCHCD 1 2 3
  universe <- as.data.table(universe) # just to make sure
  universe <-  universe[(SHRCD %in% c(10, 11)) & (EXCHCD %in% c(1,2,3)),]
  
  # First step clean
  universe[, date:= mdy(date)]
  setkey(universe, date)
  universe[,Year:= year(date)] #year column
  universe[,Month:= month(date)] #month column
  universe <- universe[!RET %in% c(-99,-88,-77,-66,-55,-44)]
  universe <- universe[!DLRET %in% c(-99,-88,-77,-66,-55,-44)]
  
  # convert the ret and delisting return into numeric
  universe[, RET := as.numeric(as.character(RET))]
  universe[, DLRET := as.numeric(as.character(DLRET))]
  universe[, PRC := as.numeric(as.character(PRC))]
  universe[, SHROUT := as.numeric(as.character(SHROUT))]
  
  # returns = ret holding, if dlret missing; = delret if holding missing; both then = geo return
  universe[, `:=`(cumDivRet, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]
  
  # market equity = price * share then one period lag
  universe[, `:=`(ME, abs(PRC * SHROUT * 1000))]
  universe[, ME_lag := shift(ME), by=c("PERMNO")]
  
  # second step clean, remove NAs
  uuniverse <- universe[PRC != "NA"]
  universe <- universe[RET != "NA"]
  universe <- universe[cumDivRet != "NA"]
  universe <- universe[ME_lag != "NA"]
  
  # value weight portfolio
  value_weight <- universe[,list(vwret = weighted.mean(cumDivRet, ME_lag, na.rm = TRUE)), by=list(Year, Month)]
  
  # equal weight portfolio
  equal_weight <- universe[,list(ewret = mean(cumDivRet)),by=list(Year, Month)]
  
  # Total market value of the previous month
  mkt_cap <- universe[,list(ME_lag = sum(ME_lag)/1000000), by=list(Year, Month)]
  mkt_ret <- cbind(value_weight, equal_weight[,3],mkt_cap[,3])
  
  return(mkt_ret)
}

PS1_ans <- PS1_Q1(Market_data)




# Q2
FF_mkt <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/F-F_Research_Data_Factors.csv")
mkt_ret <- PS1_ans

PS1_Q2 <- function(FF_mkt, universe){
  FF_mkt <- as.data.table(FF_mkt)
  universe <- as.data.table(universe)
  
  # first step clean                          
  colnames(FF_mkt) <- c("date", "Mkt_RF","SMB","HML","RF")
  universe <- universe[6:dim(universe)[1],] # relevant year from 1926 July
  FF_mkt[, RF := RF/100] # from % to decimal
  FF_mkt[, Mkt_RF := Mkt_RF/100]
  universe <- cbind(universe, RF=FF_mkt$RF)
  
  # calculate the estimated return
  universe[, Ex_MktRet := vwret - RF]
  universe <- cbind(universe,Actual_Ret=FF_mkt$Mkt_RF)
  ret <- universe[,.(Ex_MktRet,Actual_Ret)] 
  
  # all stats
  Annual_Mean <- c(mean(ret$Ex_MktRet) * 12 , mean(ret$Actual_Ret) * 12)
  Annual_SD <- c(sd(ret$Ex_MktRet) * sqrt(12) , sd(ret$Actual_Ret) * sqrt(12))
  AnnualizeSR <- c(Annual_Mean[1]/Annual_SD[1], Annual_Mean[2]/Annual_SD[2])
  Skewness <- c(skewness(ret$Ex_MktRet),skewness(ret$Actual_Ret))
  ExcessKurtosis <-  c(kurtosis(ret$Ex_MktRet),kurtosis(ret$Actual_Ret))-3
  
  result <- cbind(c(Annual_Mean[1], Annual_SD[1], AnnualizeSR[1],Skewness[1],ExcessKurtosis[1]), c(Annual_Mean[2], Annual_SD[2], AnnualizeSR[2],Skewness[2],ExcessKurtosis[2]))
  colnames(result) <- c("Ex_MktRet","Actual_MktRet")
  rownames(result) <- c("Mean","SD","SR","Skw","Kurt")
  return(result)
}

PS1_Q2_ans <- PS1_Q2(FF_mkt, mkt_ret)


# Question 3

PS1_Q3 <- function(FF_mkt, universe){
  FF_mkt <- as.data.table(FF_mkt)
  universe <- as.data.table(universe)
  
  # first step clean                          
  colnames(FF_mkt) <- c("date", "Mkt_RF","SMB","HML","RF")
  universe <- universe[6:dim(universe)[1],] # relevant year from 1926 July
  FF_mkt[, RF := RF/100] # from % to decimal
  FF_mkt[, Mkt_RF := Mkt_RF/100]
  universe <- cbind(universe, RF=FF_mkt$RF)
  
  # calculate the estimated return
  universe[, Ex_MktRet := vwret - RF]
  universe <- cbind(universe,Actual_Ret=FF_mkt$Mkt_RF)
  ret <- universe[,.(Ex_MktRet,Actual_Ret)] 
  
  result <- c(cor(ret$Ex_MktRet,ret$Actual_Ret), max(abs(ret$Ex_MktRet - ret$Actual_Ret)))
  names(result) <- c("Correlation","Max difference")
  return(result)
}

PS1_Q3_ans <- PS1_Q3(FF_mkt,mkt_ret)

