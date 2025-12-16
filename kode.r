#Handout
getwd()
setwd("AndenObligatoriskopgave")
paydata2017 <- read.table("paydata2017.txt", header = TRUE)

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

# LogPay
hist(paydata2017$LogPay, prob = TRUE, main = "Histogram af LogPay",
     xlab = "LogPay")

mean_log <- mean(paydata2017$LogPay)
sd_log <- sd(paydata2017$LogPay)

f_log <- function(x) dnorm(x, mean = mean_log, sd = sd_log)
curve(f_log, add = TRUE, col = "red")

# Opgave3.
x <- log(100000)

p_normal <- 1 - pnorm(x, mean = mean_log, sd  = sd_log)
p_normal

p_empirical <- mean(paydata2017$Pay > 100000)
p_empirical


#Opgave7

#Først laver man X
samling <- rnorm(100000, 0, 1.5)

#Parametre for X
my <- mean(samling)
sigma <- sd(samling)


#De forskellige estimater
funktion1 <- exp(my)
funktion2 <- exp(my - sigma^2)
funktion3 <- exp(my + sigma^2)
funktion4 <- exp(my + (sigma^2) / 2)
funktion5 <- exp(my + sigma)

#Teoretiske mean
empirisk_mean <- mean(samling)

cat("1: exp(μ) =", funktion1,
    "   Forskel:", funktion1 - empirisk_mean, "\n")
cat("2: exp(μ - σ^2) =", funktion2,
    "   Forskel:", funktion2 - empirisk_mean, "\n")
cat("3: exp(μ + σ^2) =", funktion3,
    "   Forskel:", funktion3 - empirisk_mean, "\n")
cat("4: exp(μ + σ^2/2) =", funktion4,
    "   Forskel:", funktion4 - empirisk_mean, "\n")
cat("5: exp(μ + σ) =", funktion5,
    "   Forskel:", funktion5 - empirisk_mean, "\n")

# Opgave 8
mu <- 11.2
sigma <- 0.353

mean_estimate <- exp(mu + sigma^2 / 2)
mean_estimate

# Opgave 9
qqnorm(paydata2017$Pay)
qqnorm(paydata2017$LogPay)