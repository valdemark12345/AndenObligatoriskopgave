#Handout
getwd()
paydata2017 <- read.table("paydata2017.txt", header=TRUE)

#Opgave1.
paydata2017 <- transform(paydata2017, LogPay = log(Pay))

median(paydata2017$Pay)
mean(paydata2017$Pay)
var(paydata2017$Pay)
sd(paydata2017$Pay)

median(paydata2017$LogPay)
mean(paydata2017$LogPay)
var(paydata2017$LogPay)
sd(paydata2017$LogPay)

#Ogpave2.

##Pay
hist(paydata2017$Pay, prob = TRUE, main = "Histogram af Pay", xlab = "Pay")

mean_pay <- mean(paydata2017$Pay)
sd_pay <- sd(paydata2017$Pay)


f_pay <- function(x) dnorm(x, mean = mean_pay, sd = sd_pay)
curve(f_pay, add = TRUE, col = "red")

##LogPay
hist(paydata2017$LogPay, prob = TRUE, main = "Histogram af LogPay", xlab = "LogPay")

mean_log <- mean(paydata2017$LogPay)
sd_log <- sd(paydata2017$LogPay)

f_log <- function(x) dnorm(x, mean = mean_log, sd = sd_log)
curve(f_log, add = TRUE, col = "red")

#Opgave3.
x <- log(100000)

p_normal <- 1 - pnorm(x, mean = mean_log, sd   = sd_log)
p_normal

p_empirical <- mean(paydata2017$Pay > 100000)
p_empirical