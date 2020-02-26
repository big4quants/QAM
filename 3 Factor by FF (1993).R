# 1. Prepare data for analysis. Combine necessary CRSP and Compustat datasets needed to define size and book-to-market decile portfolios as defined in Fama and French (1992b)1, as well as the HML and SMB factors as defined in Fama and French (1993)2. Detail which datasets you use, how you merged them, how you calculated the portfolios, and any differences between the building of the decile portfolios and the factors. Output should be between January 1973 and December 2018.

library(readr)
library(data.table)
library(DataAnalytics)
library(lubridate)
library(dplyr)
library(zoo)
library(moments)

Compustats_1973 <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS4/Compustats_1973.csv")
Pension <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS4/Pension Annual.csv")
CRSP_1973 <- read.csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS4/CRSP_1973.csv")
Linking <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS4/Linking.csv")

Compustats_1973 <- as.data.table(Compustats_1973)
Pension <- as.data.table(Pension)
CRSP_1973 <- as.data.table(CRSP_1973)
Linking <- as.data.table(Linking)

#----------------------------------------- PART I CRSP -----------------------------------------
## CRSP cleaning, same routine as before
CRSP_1973 <- unique(CRSP_1973)
CRSP_1973 <- CRSP_1973[(SHRCD %in% c(10, 11)) & (EXCHCD %in% c(1,2,3)),]

CRSP_1973[,date:= mdy(date)]
CRSP_1973[,year:= year(date)] #month column
CRSP_1973[,month:= month(date)] #month column

CRSP_1973[, RET := as.numeric(as.character(RET))]
CRSP_1973[, DLRET := as.numeric(as.character(DLRET))]
CRSP_1973[, PRC := abs(as.numeric(as.character(PRC)))]
CRSP_1973[, SHROUT := as.numeric(as.character(SHROUT))]

CRSP_1973[, `:=`(cumDivRet, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]
CRSP_1973[, `:=`(ME, abs(PRC * SHROUT / 1000))] # in dollar
CRSP_1973[,lag_ME := shift(ME),by = PERMNO]

##calculate the size ME
# this gives half year of ME from Jun t-1 and half year of Me from Jun t
CRSP_1973[,size_ME := lag_ME[month == 7],by = .(PERMNO,year)]
CRSP_1973[,size_ME := shift(size_ME,n=6),by = PERMNO]

##calculate the ME of last december 
CRSP_1973[,Dec_ME := lag_ME[month == 1], by = .(PERMNO,year)]
CRSP_1973[,Dec_ME := shift(Dec_ME,n=6),by = PERMNO]


#----------------------------------------- PART II LINKING -----------------------------------------
## to clean the LINKING rable
Linking[,LINKDT:=ymd(LINKDT)]
Linking[,yearStart:= year(LINKDT)]
Linking=Linking[LINKPRIM == 'C'|LINKPRIM == 'P',]
Linking=Linking[LINKTYPE == 'LU'|LINKTYPE == 'LC',]
Linking[,yearEnd:=ifelse(LINKENDDT=="E",year(ymd("20181231")),year(ymd(LINKENDDT)))]
Linking[,PERMCO:= LPERMCO]
Linking[,PERMNO:= LPERMNO]

CRSP_Linking <- merge(CRSP_1973, Linking, by="PERMNO",allow.cartesian = T,all.x = T)
CRSP_Linking[,flag := (year<= yearEnd) & (year >= yearStart)]
CRSP_Linking <- CRSP_Linking[flag==TRUE,]

CRSP_Linking <- CRSP_Linking[,.(year, month, PERMNO, EXCHCD, PERMCO=PERMCO.x, cumDivRet, lag_ME, size_ME, Dec_ME, gvkey)]

# to add subsidiaries under same PERMCO
setkey(CRSP_Linking,PERMCO,PERMNO,year,month)
CRSP_Linking[,size_ME := sum(size_ME),by = .(PERMCO,year,month)]
CRSP_Linking[,Dec_ME := sum(Dec_ME),by = .(PERMCO,year,month)]


#----------------------------------------- PART III COMPUSTATS -----------------------------------------
## To get book value as per defined BE = SHE - PS + DT - PRBA
# SHE = SEQ, or CEQ + PSTK, or AT - LT - MIB, or AT - LT
# coalesce to find the first non-missing value
Compustats_1973[,SHE:= coalesce(seq, ceq + pstk, at - lt - mib, at - lt)]
#merged_compustats <- merged_compustats[!is.na(SHE)]

# PS = PSTKRV, or PSTKL, or PSTK
Compustats_1973[,PS:= coalesce(pstkrv, pstkl, pstk)]

# DT = TXDITC, or ITCB, or TXDB
Compustats_1973[,DT:= coalesce(txditc, itcb, txdb)]

# to merge pension into compustats data
# format date
Pension[,datadate:= mdy(datadate)]
Pension[, Year:= year(datadate)]
Compustats_1973[,Year:= fyear] # note this is fiscal year
merged_compustats <- merge(Compustats_1973,Pension,by=c("gvkey","Year","indfmt"))
merged_compustats <- merged_compustats[,.(gvkey, Year, indfmt, SHE, PS, DT, PRBA=prba)]

# BE = SHE - PS + DT - PRBA
#merged_compustats[,BE := shift(mapply(function(a,b,c,d) return(sum(a,-b,c,-d, na.rm=T)),SHE,PS,DT,PRBA)),by = gvkey]
merged_compustats[, BE:= shift(ifelse(is.na(PRBA), (SHE-PS+DT), (SHE-PS+DT-PRBA))),by = gvkey] # treat PRBA as 0 prior to 1993


# to merge with LINKING
compustats_Linking <- merge(merged_compustats, Linking, by="gvkey",allow.cartesian = T,all.x = T)
compustats_Linking[,flag := (Year<= yearEnd) & (Year >= yearStart)]
compustats_Linking <- compustats_Linking[flag==TRUE,]

# only want INDL firm, non-Fs
compustats_Linking <- compustats_Linking[indfmt=="INDL"]
compustats_Linking <- compustats_Linking[,.(gvkey, year=Year, PERMCO, PERMNO, BE)]



#----------------------------------------- PART IV Merge CRSP and COMPUSTATS -----------------------------------------
# final merge
master_data <- merge(CRSP_Linking, compustats_Linking, by=c("PERMCO","gvkey","year","PERMNO"), allow.cartesian = T, all= T)
master_data <- master_data[!is.na(cumDivRet),]
master_data <- master_data[!is.na(lag_ME),]
# 6month gap
master_data[,BE:= shift(BE, 6), by=PERMNO]



#----------------------------------------- PART V Portfolio Construction -----------------------------------------
# sort size on NYSE
master_data <- master_data[!is.na(size_ME) & size_ME!=0]
#master_data[,size_decile := findInterval(size_ME, quantile(size_ME[EXCHCD==1], seq(.1,.9,.1)), left.open = T) + 1,by = .(year,month)]
master_data[,size_decile:= cut(size_ME, breaks = quantile(size_ME[EXCHCD==1],probs=c(0:10)/10, type=5), labels = FALSE), by=c("year","month")]
#master_data <- master_data[!is.na(size_decile),]

size_port <- master_data[,list(size_ret = weighted.mean(cumDivRet, lag_ME, na.rm=TRUE)), by = list(year, month, size_decile)]

# sort BtM on all three exchanges
master_data <- master_data[!is.na(BE) & !is.na(Dec_ME) & BE > 0] # disgard negative book value 
master_data[,BtM:= BE/Dec_ME] # the unit may be wrong, but doesnt matter in sorting
master_data[,BtM_decile :=findInterval(BtM, quantile(BtM[EXCHCD==1], seq(.1,.9,.1)), left.open = T) + 1,by = .(year, month)]
#master_data[,BtM_decile:= cut(BtM, breaks = quantile(BtM,probs=c(0:10)/10, type=5), labels = FALSE), by=c("year","month")]

BtM_port <- master_data[,list(BtM_ret = weighted.mean(cumDivRet, lag_ME, na.rm=TRUE)), by = list(year, month, BtM_decile)]


## to construct SMB and HML
# value broken by 30-70 percentiles, size broken up at 50 percentile (on NYSE stocks)
master_data[,SB:=ifelse(size_decile<6,"S","B")]
master_data[,HL:=ifelse(BtM_decile<4,"L",ifelse(BtM_decile<8,"M","H"))]
# combine text
master_data[,port_name := mapply(function(a,b) return(paste(c(a,b),collapse = "")),SB,HL)]

six_port = master_data[,.(six_Ret = weighted.mean(cumDivRet,lag_ME, na.rm = T)),by =c("year","month","port_name")]
#setkey(six_port, year,month,six_Ret)

# SMB is defined as (SL + SM + SH)/3 - (BL + BM + BH)/3
# HMLis defined as (SH + BH)/2 - (SL + BH)/2
SMB <- six_port[,list(SMB = (six_Ret[port_name=="SL"]+ six_Ret[port_name=="SM"]+ six_Ret[port_name=="SH"] -six_Ret[port_name=="BL"] -six_Ret[port_name=="BM"] -six_Ret[port_name=="BH"])/3), by =.(year,month)]
HML = six_port[,list(HML = (six_Ret[port_name=="SH"] + six_Ret[port_name=="BH"] - six_Ret[port_name=="SL"] - six_Ret[port_name=="BL"])/2),by =.(year,month)]


# to present result in one single table
colnames(BtM_port) <- c("year","month","decile","BtM_ret")
colnames(size_port) <- c("year","month","decile","size_ret")

PS4_Q1_Result <- merge(size_port, BtM_port, by=c("year","month","decile"))
PS4_Q1_Result <- merge(PS4_Q1_Result, HML, by=c("year","month"))
PS4_Q1_Result <- merge(PS4_Q1_Result, SMB, by=c("year","month"))

print(PS4_Q1_Result[1:20,]) #good to go!
print(tail(PS4_Q1_Result))



## Q2. For each size decile and the long-short portfolio, report the annualized average excess returns, annualized volatility, Sharpe Ratio, and skewness. Also report the correlation between the portfolios that you have constructed (the 10 portfolios and the long-short portfolio) and those from French's website.

FF_data <- read.csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS3/F-F_Research_Data_Factors.csv")
FF_data <- as.data.table(FF_data)

FF_size <- read.csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS4/Portfolios_Formed_On_ME.csv")
FF_size <- as.data.table(FF_size)

## some routine cleaning work for FF_market
FF_data[, date:= paste0(X,"20")]
FF_data[,date := ymd(date)]
FF_data[,year:= year(date)] #year column
FF_data[,month:= month(date)] #month column
FF_data <- FF_data[year>1973]
FF_data[,2:5] <- FF_data[,2:5]/100

## merge our calculated portfolio with FF, for the purpose of getting RF
Result_Fama <- merge(FF_data, PS4_Q1_Result, by=c("year","month"))
Result_Fama <- Result_Fama[,c(1:7,9:13)]
Fama_clean <- FF_data[-c(1:6),]
rm(Fama_data)


## some routine cleaning work for FF_size
FF_size[, date:= paste0(X,"20")]
FF_size[,date := ymd(date)]
FF_size[,year:= year(date)] #year column
FF_size[,month:= month(date)] #month column
FF_size <- FF_size[year>1973 & year<2019]
FF_size <- FF_size[-c(1:6),]

FF_size <- FF_size[,11:23]
FF_size[,1:10] <- FF_size[,1:10]/100
colnames(FF_size) <- c("1","2","3", "4", "5", "6", "7" , "8" , "9" , "10","date","year","month")
FF_size <- data.frame(FF_size)


# stats results for decile - size portfolio 
table <- matrix(0,4,11)
corr_1 <-matrix(0,1,11)
for(i in 1:10){
  temp <- Result_Fama[decile==i]
  table[1,i] <- mean(temp$size_ret-temp$RF) * 12
  table[2,i] <- sd(temp$size_ret) * sqrt(12)
  table[3,i] <- table[1,i]/table[2,i]
  table[4,i] <- skewness(temp$size_ret)
  corr_1[i] <- cor(temp$size_ret,FF_size[,i])
}

size_LS <- Result_Fama[decile==10] - Result_Fama[decile==1]
table[1,11] <- mean(size_LS$size_ret-size_LS$RF) * 12
table[2,11] <- sd(size_LS$size_ret) * sqrt(12)
table[3,11] <- table[1,11]/table[2,11]
table[4,11] <- skewness(size_LS$size_ret)
corr_1[11] <- cor(size_LS$size_ret, (FF_size[,10]-FF_size[,1]))

colnames(table) <- c(paste0(rep("decile",10),1:10),"long-short")
rownames(table) <- c("Average Ex_return", "Volatility", "SR", "Skewness")

colnames(corr_1) <- c(paste0(rep("decile",10),1:10),"long-short")
rownames(corr_1) <- c("Correlation")

print(table)
print(corr_1)



## Q3. For each book-to-market decile and the long-short portfolio, report the annualized average excess returns, annualized volatility, Sharpe Ratio, and skewness. Also report the correlation between the portfolios that you have constructed (the 10 portfolios and the long-short portfolio) and those from French's website.

FF_BM <- read.csv("C:/Users/mulin/Desktop/MFE Spring 2019/Quant Asset Management/PS4/Portfolios_Formed_on_BE-ME.csv")
FF_BM <- as.data.table(FF_BM)

## some routine cleaning work for FF_size
FF_BM[, date:= paste0(X,"20")]
FF_BM[,date := ymd(date)]
FF_BM[,year:= year(date)] #year column
FF_BM[,month:= month(date)] #month column
FF_BM <- FF_BM[year>1973 & year<2019]
FF_BM <- FF_BM[-c(1:6),]

FF_BM <- FF_BM[,11:23]
FF_BM[,1:10] <- FF_BM[,1:10]/100
colnames(FF_BM) <- c("1","2","3",  "4" , "5" , "6" , "7" , "8" , "9" , "10","date","year","month")
FF_BM <- data.frame(FF_BM)

# stats results for decile - size portfolio 
table3 <- matrix(0,4,11)
corr_3 <-matrix(0,1,11)
for(i in 1:10){
  temp <- Result_Fama[decile==i]
  table3[1,i] <- mean(temp$BtM_ret-temp$RF) * 12
  table3[2,i] <- sd(temp$BtM_ret) * sqrt(12)
  table3[3,i] <- table3[1,i]/table3[2,i]
  table3[4,i] <- skewness(temp$BtM_ret)
  corr_3[i] <- cor(temp$BtM_ret,FF_BM[,i])
}

#size_LS <- Result_Fama[decile==10] - Result_Fama[decile==1]
table3[1,11] <- mean(size_LS$BtM_ret-size_LS$RF) * 12
table3[2,11] <- sd(size_LS$BtM_ret) * sqrt(12)
table3[3,11] <- table3[1,11]/table3[2,11]
table3[4,11] <- skewness(size_LS$BtM_ret)
corr_3[11] <- cor(size_LS$BtM_ret, (FF_BM[,10]-FF_BM[,1]))

colnames(table3) <- c(paste0(rep("decile",10),1:10),"long-short")
rownames(table3) <- c("Average Ex_return", "Volatility", "SR", "Skewness")

colnames(corr_3) <- c(paste0(rep("decile",10),1:10),"long-short")
rownames(corr_3) <- c("Correlation")

print(table3)
print(corr_3)


##### investigation 1973-1987 #################
FF_BM1 <- data.table(FF_BM)[year>1973 & year<1988]
#FF_BM1 <- FF_BM1[-c(1:6),]
Result_Fama1 <- Result_Fama[year %in% c(1972:1987)]
colnames(FF_BM1) <- c("1","2","3",  "4" , "5" , "6" , "7" , "8" , "9" , "10","date","year","month")
FF_BM1 <- data.frame(FF_BM1)
# stats results for decile - size portfolio 
table3_1 <- matrix(0,4,11)
corr_3_1 <-matrix(0,1,11)
for(i in 1:10){
  temp <- Result_Fama1[decile==i]
  table3_1[1,i] <- mean(temp$BtM_ret-temp$RF) * 12
  table3_1[2,i] <- sd(temp$BtM_ret) * sqrt(12)
  table3_1[3,i] <- table3_1[1,i]/table3_1[2,i]
  table3_1[4,i] <- skewness(temp$BtM_ret)
  corr_3_1[i] <- cor(temp$BtM_ret,FF_BM1[,i])
}
size_LS_1 <- Result_Fama1[decile==10] - Result_Fama1[decile==1]
table3_1[1,11] <- mean(size_LS_1$BtM_ret-size_LS_1$RF) * 12
table3_1[2,11] <- sd(size_LS_1$BtM_ret) * sqrt(12)
table3_1[3,11] <- table3_1[1,11]/table3_1[2,11]
table3_1[4,11] <- skewness(size_LS_1$BtM_ret)
corr_3_1[11] <- cor(size_LS_1$BtM_ret, (FF_BM1[,10]-FF_BM1[,1]))
colnames(table3_1) <- c(paste0(rep("decile",10),1:10),"long-short")
rownames(table3_1) <- c("Average Ex_return", "Volatility", "SR", "Skewness")
colnames(corr_3_1) <- c(paste0(rep("decile",10),1:10),"long-short")
rownames(corr_3_1) <- c("Prior 1988 Correlation")


##### investigation 1988-2018 #################
FF_BM2 <- data.table(FF_BM)[year>1987 & year<2019]
#FF_BM1 <- FF_BM1[-c(1:6),]
Result_Fama2 <- Result_Fama[year %in% c(1988:2018)]
colnames(FF_BM2) <- c("1","2","3",  "4" , "5" , "6" , "7" , "8" , "9" , "10","date","year","month")
FF_BM2 <- data.frame(FF_BM2)
# stats results for decile - size portfolio 
table3_2 <- matrix(0,4,11)
corr_3_2 <-matrix(0,1,11)
for(i in 1:10){
  temp <- Result_Fama2[decile==i]
  table3_2[1,i] <- mean(temp$BtM_ret-temp$RF) * 12
  table3_2[2,i] <- sd(temp$BtM_ret) * sqrt(12)
  table3_2[3,i] <- table3_2[1,i]/table3_2[2,i]
  table3_2[4,i] <- skewness(temp$BtM_ret)
  corr_3_2[i] <- cor(temp$BtM_ret,FF_BM2[,i])
}
size_LS_2 <- Result_Fama2[decile==10] - Result_Fama2[decile==1]
table3_2[1,11] <- mean(size_LS_2$BtM_ret-size_LS_2$RF) * 12
table3_2[2,11] <- sd(size_LS_2$BtM_ret) * sqrt(12)
table3_2[3,11] <- table3_2[1,11]/table3_2[2,11]
table3_2[4,11] <- skewness(size_LS_2$BtM_ret)
corr_3_2[11] <- cor(size_LS_2$BtM_ret, (FF_BM2[,10]-FF_BM2[,1]))
colnames(table3_2) <- c(paste0(rep("decile",10),1:10),"long-short")
rownames(table3_2) <- c("Average Ex_return", "Volatility", "SR", "Skewness")
colnames(corr_3_2) <- c(paste0(rep("decile",10),1:10),"long-short")
rownames(corr_3_2) <- c("After 1988 Correlation")

print(table3_1)
print(corr_3_1)

print(table3_2)
print(corr_3_2)



## Q4. Has the value and size anomaly worked in the past few years? Show some empirical evidence. 

## 5. For both HML and SMB portfolios, report the annualized average excess returns, annualized volatility, Sharpe Ratio, and skewness. Report correlations between the replicated factors and the factor from French's website. Have the factors been consistent across time? Show some empirical evidence.

table5 <- matrix(0,4,2)
corr_5 <-matrix(0,1,2)

temp <- data.frame(Result_Fama)
# for HML
table5[1,1] <- mean(temp[,11]-temp$RF) * 12
table5[2,1] <- sd(temp[,11]) * sqrt(12)
table5[3,1] <- table[1,1]/table[2,1]
table5[4,1] <- skewness(temp[,11])
corr_5[1] <- cor(temp[,11],temp[,6])

# for SMB
table5[1,2] <- mean(temp[,12]-temp$RF) * 12
table5[2,2] <- sd(temp[,12]) * sqrt(12)
table5[3,2] <- table5[1,2]/table5[2,2]
table5[4,2] <- skewness(temp[,12])
corr_5[2] <- cor(temp[,12],temp[,5])
rm(temp)

colnames(table5) <- c("HML","SMB")
rownames(table5) <- c("Average Ex_return", "Volatility", "SR", "Skewness")

colnames(corr_5) <- c("HML","SMB")
rownames(corr_5) <- c("Correlation")

print(table5)
print(corr_5)


4_SMB <- ts(cumprod(1*(1+SMB$SMB)), c(1974,7), frequency = 12)
Q4_HML <- ts(cumprod(1*(1+HML$HML)), c(1974,7), frequency = 12)
Q4_mkt <- ts(cumprod(1*(1+FF_data$Mkt.RF)), c(1974,7), frequency = 12)

plot(Q4_mkt,type="l",main="Cumulative Returns for Ex_Mkt, HML, SMB", col="Blue",ylab="Cumulative Return",xlab="Year")
lines(Q4_HML, col="green")
lines(Q4_SMB, col="red")
legend("topleft", c("Market","HML","SMB"),col=c("blue","green","red"),pch=10)
