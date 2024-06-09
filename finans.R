## 
## ################################
## ## Set the working directory
## 
## ## In RStudio use conveniently the menu "Session->Set Working
## ## Directory->To Source File Location"
## ## In R use only "/" for separating in paths (i.e. no backslash)
setwd("C:\Users\Default User.DESKTOP-F6CKQMA\OneDrive\Skrivebord\Statistik-projekt")

## 
## ################################
## ## Import the data
## 
## ## Read the finans1_data.csv file containing the data

wr <- read.table("finans1_data.csv", header=TRUE, sep=";", as.is=TRUE)

## 
## #############################
## ## Overview of the data
## 
## ## Dimension of HE (number of rows and columns)
 dim(wr)
## ## Column names
 names(wr)
 ## The first rows
 head(wr)
 ## The last rows
 tail(wr)
 ## Default summary
 summary(wr)
## ## Another summary function also including the data type
 str(wr)

############################
## Descriptive analysis of selected variables
## b)
 
##AGG
sum(!is.na(wr$AGG))
mean(wr$AGG)
sd(wr$AGG)
## ...

## Alternatively, to run a "function" on the selected columns you
## can use the "apply"-command or wrap it in a for-loop. 
## For futher info see ?apply.


par(mfrow = c(2,2))
plot(ecdf(wr$AGG), verticals=TRUE,
     xlab = "Ugentlig udvikling",
     ylab = "Empirisk tæthed",
     col="red")
plot(ecdf(wr$VAW), verticals=TRUE,
     xlab = "Ugentlig udvikling",
     ylab = "Empirisk tæthed",
     col="blue")
plot(ecdf(wr$IWN), verticals=TRUE,
     xlab = "Ugentlig udvikling",
     ylab = "Empirisk tæthed",
     col="green")
plot(ecdf(wr$SPY), verticals=TRUE,
     xlab = "Ugentlig udvikling",
     ylab = "Empirisk tæthed",
     col="pink")

par(mfrow = c(2,2))
boxplot(wr$AGG, col="lightsalmon", main="Avanceret box \n plot for AGG")
boxplot(wr$VAW, col="cyan", main="Avanceret box \n plot for VAW")
boxplot(wr$IWN, col="seagreen2", main="Avanceret box \n plot for IWN")
boxplot(wr$SPY, col="magenta", main="Avanceret box \n plot for SPY")

mean(wr$AGG)
mean(wr$VAW)
mean(wr$IWN)
mean(wr$SPY)

var(wr$AGG)
var(wr$VAW)
var(wr$IWN)
var(wr$SPY)

sd(wr$AGG)
sd(wr$VAW)
sd(wr$IWN)
sd(wr$SPY)

quantile(wr$AGG, probs = c(0.25, 0.5, 0.75), type = 2)
quantile(wr$VAW, probs = c(0.25, 0.5, 0.75), type = 2)
quantile(wr$IWN, probs = c(0.25, 0.5, 0.75), type = 2)
quantile(wr$SPY, probs = c(0.25, 0.5, 0.75), type = 2)
########################


hist(wr$AGG,
     freq = FALSE,
     prob = TRUE,
     col = "light blue",
     nclass = 40,
     main = "Histogram, AGG",
     xlab = "Relativt afkast",
     ylab = "Densitet"
)
abline(v = median(wr$AGG), col = "red", lwd = 2, lty = 2)
legend(
  "topright", 
  legend = "M",
  col ="red",
  lty = 2,
  lwd = 2,
  cex = 1.2,
  bty = "y"
)

hist(wr$VAW,
     freq = FALSE,
     prob = TRUE,
     col = "light blue",
     nclass = 40,
     main = "Histogram, VAW",
     xlab = "Relativt afkast",
     ylab = "Densitet"
)
abline(v = median(wr$VAW), col = "red", lwd = 2, lty = 2)
legend(
  "topright", 
  legend = "M",
  col ="red",
  lty = 2,
  lwd = 2,
  cex = 1.2,
  bty = "y"
)

hist(wr$IWN,
     freq = FALSE,
     prob = TRUE,
     col = "light blue",
     nclass = 40,
     main = "Histogram for IWN",
     xlab = "Relativt afkast",
     ylab = "Densitet"
)
abline(v = median(wr$IWN), col = "red", lwd = 2, lty = 2)
legend(
  "topright", 
  legend = "M",
  col ="red",
  lty = 2,
  lwd = 2,
  cex = 1.2,
  bty = "y"
)

hist(wr$SPY,
     freq = FALSE,
     prob = TRUE,
     col = "light blue",
     nclass = 40,
     main = "Histogram for SPY",
     xlab = "Relativt afkast",
     ylab = "Densitet"
)
abline(v = median(wr$SPY), col = "red", lwd = 2, lty = 2)
legend(
  "topright", 
  legend = "M",
  col ="red",
  lty = 2,
  lwd = 2,
  cex = 1.2,
  bty = "y"
)

##c)


  #######################
## d)
## Determination of the correlation between ETFs 
## and determination of portfolio
cov(wr[ ,c("AGG","VAW","IWN","SPY","EWG","EWW")])


















  
###########################
## Model validation
## f)
## Validation of a model for AGG
qqnorm(wr$AGG, main='Validation of normal distribution assumption for AGG',
       xlab='z-scores', ylab='Weekly returns')
qqline(wr$AGG)
## Do the same for the other ETFs






  
###########################
## Calculations of the 95% confidence intervals
## g)
## t-quantile for the confidence interval for the mean of AGG, 
## since the degrees of freedom for the mean of AGG are 453 
qt(0.975, 453)

## Determination of the confidence interval for the mean parameter in a
## normally distributed random sample

## The 95% confidence interval for AGG
t.test(wr$AGG, conf.level=0.95)$conf.int
## Do the same for the other ETFs


















  
################################
## 
## Import data finans2_data.csv
etfSum <- read.table("finans2_data.csv",header=TRUE, sep=";")
str(etfSum)

## 
## ################################
## ## j)
## ## Determine the empirical correlation for the selected variables and
## ## examine the dependencies
## cor(etfSum_analyse[,2:7], use="everything", method="pearson")
## 
## ## First trim the square around the plot. See more on ?par
## par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
## par(mfrow=c(1,1))
## plot(etfSum_analyse$Volatility, etfSum_analyse$CVaR, pch=16, cex=0.7,
##      xlab="Volatility [Weekly Pct.]",
##      ylab="Conditional Value at Risk [Weekly Pct.]",  cex.lab=0.8,
##      main="Relation between Volatility and CVaR", cex.main=0.8)

## 
## ## For calculations of the correlation between Geo.mean and maxTuW
## ## k)
## cov(etfSum_analyse$Geo.mean, etfSum_analyse$maxTuW)
## var(etfSum_analyse$Geo.mean)
## var(etfSum_analyse$maxTuW)
