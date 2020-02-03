A1 <- read.csv("/Users/Viola/Documents/APM/APM466/A1.csv")
library(jrvFinance)
#4a
#convert the classes of settle date and maturity dates from factor to date.
settle <- as.Date(A1$DATE, format = "%m/%d/%Y")
maturity_1 <- as.Date(A1$MaturityDate_1,format = "%m/%d/%Y")
maturity_2 <- as.Date(A1$MaturityDate_2,format = "%m/%d/%Y")
maturity_3 <- as.Date(A1$MaturityDate_3,format = "%m/%d/%Y")
maturity_4 <- as.Date(A1$MaturityDate_4,format = "%m/%d/%Y")
maturity_5 <- as.Date(A1$MaturityDate_5,format = "%m/%d/%Y")
maturity_6 <- as.Date(A1$MaturityDate_6,format = "%m/%d/%Y")
maturity_7 <- as.Date(A1$MaturityDate_7,format = "%m/%d/%Y")
maturity_8 <- as.Date(A1$MaturityDate_8,format = "%m/%d/%Y")
maturity_9 <- as.Date(A1$MaturityDate_9,format = "%m/%d/%Y")
maturity_10 <- as.Date(A1$MaturityDate_10,format = "%m/%d/%Y")
#calculate yield to maturity for each bond with correspondence to ten days.
ytm_1 <- bond.yields(settle, maturity_1, A1$Coupon_1, freq = 2, A1$PRICE_1)
ytm_2 <- bond.yields(settle, maturity_2, A1$Coupon_2, freq = 2, A1$PRICE_2)
ytm_3 <- bond.yields(settle, maturity_3, A1$Coupon_3, freq = 2, A1$PRICE_3)
ytm_4 <- bond.yields(settle, maturity_4, A1$Coupon_4, freq = 2, A1$PRICE_4)
ytm_5 <- bond.yields(settle, maturity_5, A1$Coupon_5, freq = 2, A1$PRICE_5)
ytm_6 <- bond.yields(settle, maturity_6, A1$Coupon_6, freq = 2, A1$PRICE_6)
ytm_7 <- bond.yields(settle, maturity_7, A1$Coupon_7, freq = 2, A1$PRICE_7)
ytm_8 <- bond.yields(settle, maturity_8, A1$Coupon_8, freq = 2, A1$PRICE_8)
ytm_9 <- bond.yields(settle, maturity_9, A1$Coupon_9, freq = 2, A1$PRICE_9)
ytm_10 <- bond.yields(settle, maturity_10, A1$Coupon_10, freq = 2, A1$PRICE_10)
#get ten bonds's yield to maturity on each day.
ytm_day10 <- c(ytm_1[1],ytm_2[1],ytm_3[1],ytm_4[1],ytm_5[1],ytm_6[1],ytm_7[1],ytm_8[1],ytm_9[1],ytm_10[1])
ytm_day9 <- c(ytm_1[2],ytm_2[2],ytm_3[2],ytm_4[2],ytm_5[2],ytm_6[2],ytm_7[2],ytm_8[2],ytm_9[2],ytm_10[2])
ytm_day8 <- c(ytm_1[3],ytm_2[3],ytm_3[3],ytm_4[3],ytm_5[3],ytm_6[3],ytm_7[3],ytm_8[3],ytm_9[3],ytm_10[3])
ytm_day7 <- c(ytm_1[4],ytm_2[4],ytm_3[4],ytm_4[4],ytm_5[4],ytm_6[4],ytm_7[4],ytm_8[4],ytm_9[4],ytm_10[4])
ytm_day6 <- c(ytm_1[5],ytm_2[5],ytm_3[5],ytm_4[5],ytm_5[5],ytm_6[5],ytm_7[5],ytm_8[5],ytm_9[5],ytm_10[5])
ytm_day5 <- c(ytm_1[6],ytm_2[6],ytm_3[6],ytm_4[6],ytm_5[6],ytm_6[6],ytm_7[6],ytm_8[6],ytm_9[6],ytm_10[6])
ytm_day4 <- c(ytm_1[7],ytm_2[7],ytm_3[7],ytm_4[7],ytm_5[7],ytm_6[7],ytm_7[7],ytm_8[7],ytm_9[7],ytm_10[7])
ytm_day3 <- c(ytm_1[8],ytm_2[8],ytm_3[8],ytm_4[8],ytm_5[8],ytm_6[8],ytm_7[8],ytm_8[8],ytm_9[8],ytm_10[8])
ytm_day2 <- c(ytm_1[9],ytm_2[9],ytm_3[9],ytm_4[9],ytm_5[9],ytm_6[9],ytm_7[9],ytm_8[9],ytm_9[9],ytm_10[9])
ytm_day1 <- c(ytm_1[10],ytm_2[10],ytm_3[10],ytm_4[10],ytm_5[10],ytm_6[10],ytm_7[10],ytm_8[10],ytm_9[10],ytm_10[10])
#set time for ytm curve
year <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
#plot 5-year yield curve (ytm curve) corresponding to each day of data
plot(year, ytm_day1,type = "o",col = "red", xlab = "year", ylab = "ytm", main = "yield to maturity")
lines(year, ytm_day2, type = "o", col = "orange")
lines(year, ytm_day3, type = "o", col = "black")
lines(year,ytm_day4, type = "o", col = "green")
lines(year,ytm_day5, type = "o", col = "cyan")
lines(year,ytm_day6, type = "o", col = "blue")
lines(year,ytm_day7, type = "o", col = "purple")
lines(year,ytm_day8, type = "o", col = "brown")
lines(year,ytm_day9, type = "o", col = "navy")
lines(year,ytm_day10, type = "o", col = "grey")
legend("topright", legend=c("JAN 2", "JAN 3","JAN 6","JAN 7","JAN 8","JAN 9","JAN 10","JAN 13","JAN 14","JAN 15"),
       col=c("red","orange","black","green","cyan","blue","purple","brown","navy","grey"),lty=1:2, cex=0.55,
       text.font=4)
#4b
#convert clean price to dirty price 
p_1 <- 0
for (i in 1:10){
  p_1[i] <- A1$PRICE_1[i]+ bond.TCF(settle[i], maturity_1[i], A1$Coupon_1[i], freq = 2)$accrued
}
p_2 <- 0
for (i in 1:10){
  p_2[i] <- A1$PRICE_2[i]+ bond.TCF(settle[i], maturity_2[i], A1$Coupon_2[i], freq = 2)$accrued
}  
p_3 <- 0
for (i in 1:10){
  p_3[i] <- A1$PRICE_3[i]+ bond.TCF(settle[i], maturity_3[i], A1$Coupon_3[i], freq = 2)$accrued
}  
p_4 <- 0
for (i in 1:10){
  p_4[i] <- A1$PRICE_4[i]+ bond.TCF(settle[i], maturity_4[i], A1$Coupon_4[i], freq = 2)$accrued
}  
p_5 <- 0
for (i in 1:10){
  p_5[i] <- A1$PRICE_5[i]+ bond.TCF(settle[i], maturity_5[i], A1$Coupon_5[i], freq = 2)$accrued
}  
p_6 <- 0
for (i in 1:10){
  p_6[i] <- A1$PRICE_6[i]+ bond.TCF(settle[i], maturity_6[i], A1$Coupon_6[i], freq = 2)$accrued
}  
p_7 <- 0
for (i in 1:10){
  p_7[i] <- A1$PRICE_7[i]+ bond.TCF(settle[i], maturity_7[i], A1$Coupon_7[i], freq = 2)$accrued
}  
p_8 <- 0
for (i in 1:10){
  p_8[i] <- A1$PRICE_8[i]+ bond.TCF(settle[i], maturity_8[i], A1$Coupon_8[i], freq = 2)$accrued
}  
p_9 <- 0
for (i in 1:10){
  p_9[i] <- A1$PRICE_9[i]+ bond.TCF(settle[i], maturity_9[i], A1$Coupon_9[i], freq = 2)$accrued
}  
p_10 <- 0
for (i in 1:10){
  p_10[i] <- A1$PRICE_10[i]+ bond.TCF(settle[i], maturity_10[i], A1$Coupon_10[i], freq = 2)$accrued
}  
#calculate spot rates for the first bond with the closet maturity, which has only one cash flow in future
r1 <- 0
for (i in 1:10){
  r1[i] <- -log(p_1[i]/bond.TCF(settle[i], maturity_1[1], A1$Coupon_1[1], freq = 2)$cf)/bond.TCF(settle[i], maturity_1[1], A1$Coupon_1[1], freq = 2)$t
}
#calculate spot rates for the second bond with the second closet maturity, which has two cash flows in future
r2 <- 0
for (i in 1:10){
  fcn <- function(x) (bond.TCF(settle[i]
                               , maturity_2[1], A1$Coupon_2[1], freq = 2)$cf[1])*exp(-r1[i]*bond.TCF(settle[i]
                                                                                                       , maturity_2[1], A1$Coupon_2[1], freq = 2)$t[1]) + (bond.TCF(settle[i]
                                                                                                                                                                    , maturity_2[1], A1$Coupon_2[1], freq = 2)$cf[2])*exp(-x*bond.TCF(settle[i],
                                                                                                                                                                                                                                      maturity_2[1], A1$Coupon_2[1], freq = 2)$t[2]) - p_2[i]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
  r2[i] <- uniroot(fcn,c(0, 1),extendInt="yes")$root
}
r3 <- 0
for (i in 1:10){
  fcn <- function(x) (bond.TCF(settle[i], maturity_3[1], A1$Coupon_3[1], freq = 2)$cf[1])*exp(-r1[i]*bond.TCF(settle[i], maturity_3[1], A1$Coupon3[1], freq = 2)$t[1]) + (bond.TCF(settle[i]
                                                                                                                                                                  , maturity_3[1], A1$Coupon_3[1], freq = 2)$cf[2])*exp(-r2[i]*bond.TCF(settle[i],
                                                                                                                                                                                                                                    maturity_3[1], A1$Coupon_3[1], freq = 2)$t[2]) + (bond.TCF(settle[i]
                                                                                                                                                                                                                                                                                               , maturity_3[1], A1$Coupon_3[1], freq = 2)$cf[3])*exp(-x[i]*bond.TCF(settle[i],maturity_3[1], A1$Coupon_3[1], freq = 2)$t[3])-p_3[i]
  
  r3[i] <- uniroot(fcn,c(0, 1))$root
}
spot <- read.csv("A1 spot rate.csv")
plot(year, spot$r_day1,type = "o",col = "red", xlab = "year", ylab = "spot rate", main = "5-year spot curve")
lines(year, spot$r_day2, type = "o", col = "orange")
lines(year, spot$r_day3, type = "o", col = "black")
lines(year,spot$r_day4, type = "o", col = "green")
lines(year,spot$r_day5, type = "o", col = "cyan")
lines(year,spot$r_day6, type = "o", col = "blue")
lines(year,spot$r_day7, type = "o", col = "purple")
lines(year,spot$r_day8, type = "o", col = "brown")
lines(year,spot$r_day9, type = "o", col = "navy")
lines(year,spot$r_day10, type = "o", col = "grey")
legend("topright", legend=c("JAN 2", "JAN 3","JAN 6","JAN 7","JAN 8","JAN 9","JAN 10","JAN 13","JAN 14","JAN 15"),
       col=c("red","orange","black","green","cyan","blue","purple","brown","navy","grey"),lty=1:2, cex=0.8,
       text.font=4)
#4c
f11 <- 0
for (i in 1:10){
  f11[i] <- (r4[i]*year[4]-r2[i]*year[2])/(year[4]-year[2])
}
f12 <- 0
for (i in 1:10){
  f12[i] <- (r6[i]*year[6]-r2[i]*year[2])/(year[6]-year[2])
}
f13 <- 0
for (i in 1:10){
  f13[i] <- (r8[i]*year[8]-r2[i]*year[2])/(year[8]-year[2])
}
f14 <- 0
for (i in 1:10){
  f14[i] <- (r10[i]*year[10]-r2[i]*year[2])/(year[10]-year[2])
}
time <- c(1,2,3,4)
plot(time, f_day1,type = "o",col = "red", xlab = "time", ylab = "forward rate", main = "forward curve")
lines(time, f_day2, type = "o", col = "orange")
lines(time, f_day3, type = "o", col = "black")
lines(time,f_day4, type = "o", col = "green")
lines(time,f_day5, type = "o", col = "cyan")
lines(time,f_day6, type = "o", col = "blue")
lines(time,f_day7, type = "o", col = "purple")
lines(time,f_day8, type = "o", col = "brown")
lines(time,f_day9, type = "o", col = "navy")
lines(time,f_day10, type = "o", col = "grey")
legend("bottomleft", legend=c("JAN 2", "JAN 3","JAN 6","JAN 7","JAN 8","JAN 9","JAN 10","JAN 13","JAN 14","JAN 15"),
       col=c("red","orange","black","green","cyan","blue","purple","brown","navy","grey"),lty=1:2, cex=0.6,
       text.font=3)
#5
#Calculate each Xi
X1 <- 0
for (j in 1:9){
  X1[j] <- log(ytm_2[j+1]/ytm_2[j])
}
X2 <- 0
for (j in 1:9){
  X2[j] <- log(ytm_4[j+1]/ytm_4[j])
}
X3 <- 0
for (j in 1:9){
  X3[j] <- log(ytm_6[j+1]/ytm_6[j])
}
X4 <- 0
for (j in 1:9){
  X4[j] <- log(ytm_8[j+1]/ytm_8[j])
}
X5 <- 0
for (j in 1:9){
  X5[j] <- log(ytm_10[j+1]/ytm_10[j])
}
#convert Xi's into matrix form
X <- matrix(c(X1,X2,X3,X4,X5),nrow=9,ncol=5)
#calculate the covariance matrix of X
cov_X <-cov(X)
cov_X
#Calculate log returns for forward rates
forward <- read.csv("/Users/Viola/Documents/APM/APM466/A1 forward.csv")
Y1 <- 0
for (j in 1:9){
  Y1[j] <- log(forward$f11[j+1]/forward$f11[j])
}
Y2 <- 0
for (j in 1:9){
  Y2[j] <- log(forward$f12[j+1]/forward$f12[j])
}
Y3 <- 0
for (j in 1:9){
  Y2[j] <- log(forward$f13[j+1]/forward$f13[j])
}
Y4 <- 0
for (j in 1:9){
  Y4[j] <- log(forward$f14[j+1]/forward$f14[j])
}
#convert log returns' into matrix form
Y <- c(Y1,Y2,Y3,Y4)
Y <- matrix(Y,nrow=9,ncol=4)
#calculate the covariance matrix of Y
cov_Y <-cov(Y)
cov_Y
#6
#Calculate eigenvalues and eigenvectors for the covariance matrix 
#for the time series of daily log-returns of yield.
eigenvalue_cov_X <- eigen(cov_X)$values
eigenvalue_cov_X
eigenvector_cov_X <- eigen(cov_X)$vectors
eigenvector_cov_X
#Calculate eigenvalues and eigenvectors for the covariance matrix 
#for the time series of daily log-returns of forward rates.
eigenvalue_cov_Y <- eigen(cov_Y)$values
eigenvalue_cov_Y
eigenvector_cov_Y <- eigen(cov_Y)$vectors
eigenvector_cov_Y
