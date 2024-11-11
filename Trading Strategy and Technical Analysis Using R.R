install.packages("fPortfolio")
install.packages("timeSeries")
install.packages("quantmod")
install.packages("dplyr")
install.packages("PerformanceAnalytics")
install.packages("ggplot2")
install.packages("readxl")
install.packages("zoo")


library(fPortfolio)
library(timeSeries)
library(quantmod)
library(dplyr)
library(ggplot2)
library(readxl)
library(zoo)
library(writexl)
library(PerformanceAnalytics)
library(PortfolioAnalytics)



##bulk loading##
# Correct the variable name and bulk load the stock data
symbols <- c("ITC.NS", "ADANIENT.NS", "JSWSTEEL.NS", "SBILIFE.NS", "NTPC.NS", 
             "TECHM.NS", "SBIN.NS", "SHRIRAMFIN.NS", "HINDUNILVR.NS", "GRASIM.NS")

# Use lapply to fetch data for each symbol
portfolio_stks <- lapply(symbols, function(X) {
  getSymbols(X, from = "2021-07-31", to = "2024-07-31", auto.assign = FALSE)
})

# Display the first few entries of the first stock data as an example
head(portfolio_stks[[1]])


##

portfolio_stocks <- lapply(symbols,function(X){getSymbols(X,from="2021-08-01", to="2024-07-31", auto.assign=FALSE)})
head(portfolio_stocks)

portfolio_stocks_df <- as.data.frame(portfolio_stocks)
portfolio_stocks_df <- Ad(portfolio_stocks_df)

#Calculating the discrete returns for the stock prices
portfolio_stocks_return <- Return.calculate(portfolio_stocks_df, method = "discrete")

# Omitting any NA values
portfolio_stocks_return <- na.omit(portfolio_stocks_return)

# Calculating the portfolio returns with equal weights assigned to each stock, mean return, covariance matrix and standard deviation(risk)
portfolio_stocks_return_pf <- Return.portfolio(portfolio_stocks_return,weights = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1),geometric = FALSE)

mean(portfolio_stocks_return_pf)
cov(portfolio_stocks_return)
sd(portfolio_stocks_return_pf)

# Converting the returns data to a timeSeries object for portfolio optimization
portfolio_stocks_return <- as.timeSeries(portfolio_stocks_return)

# Generating and plotting the efficient frontier with a risk-free rate of 7% per annum
efficient_frontier <- portfolioFrontier(portfolio_stocks_return,`setRiskFreeRate<-`(portfolioSpec(),.07/252),constraints = "longOnly")

# Plot the efficient frontier along with various portfolio metrics
plot(efficient_frontier, c(1,2,3,5,7,8))

# Determining the minimum variance portfolio and the respective asset weights
min_variance_portfolio <- minvariancePortfolio(portfolio_stocks_return, portfolioSpec(), constraints = "longOnly")
weights_portfolio <- getWeights(min_variance_portfolio)

# Determining the tangency portfolio (optimal portfolio) with the highest Sharpe ratio and the respective asset weights
Optimum_portfolio <- tangencyPortfolio(portfolio_stocks_return, `setRiskFreeRate<-`(portfolioSpec(),.07/252),constraints="longOnly")
weights_optimum_portfolio <- getWeights(Optimum_portfolio)



##single loading##
ITC_1<- getSymbols("ITC.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
ITC_1

ADANIENT_1<- getSymbols("ADANIENT.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
ADANIENT_1

JSWSTEEL_1<- getSymbols("JSWSTEEL.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
JSWSTEEL_1

SBILIFE_1<- getSymbols("SBILIFE.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
SBILIFE_1

NTPC_1<- getSymbols("NTPC.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
NTPC_1

TECHM_1<- getSymbols("TECHM.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
TECHM_1

SBIN_1<- getSymbols("SBIN.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
SBIN_1

SHRIRAMFIN_1<- getSymbols("SHRIRAMFIN.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
SHRIRAMFIN_1

HINDUNILVR_1<- getSymbols("HINDUNILVR.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
HINDUNILVR_1

GRASIM_1<- getSymbols("GRASIM.NS",from = "2021-07-31", to = "2024-07-31",auto.assign = FALSE)
GRASIM_1


#obtaining adjusted prices#
ITC <- Ad(ITC_1)
ADANIENT <- Ad(ADANIENT_1)
JSWSTEEL <- Ad(JSWSTEEL_1)
SBILIFE <- Ad(SBILIFE_1)
NTPC <- Ad(NTPC_1)
TECHM <- Ad(TECHM_1)
SBIN <- Ad(SBIN_1)
SHRIRAMFIN <- Ad(SHRIRAMFIN_1)
HINDUNILVR <- Ad(HINDUNILVR_1)
GRASIM <- Ad(GRASIM_1)


#merging all individual stocks into one
portfolio_stks <- merge.xts(ITC,ADANIENT,JSWSTEEL,SBILIFE,NTPC,TECHM,SBIN,SHRIRAMFIN,HINDUNILVR,GRASIM)
portfolio_stks

portfolio_stks <- as.data.frame(portfolio_stks)
head(portfolio_stks)
class(portfolio_stks)
portfolio_stks$dt <- row.names(portfolio_stks)
row.names(portfolio_stks)

#output
write_xlsx(portfolio_stks,'PORTFOLIO_1.xlsx')


#calculating return
install.packages("PortfolioAnalytics")

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)


portfolio_ret <- Return.calculate(portfolio_stks)

#omit NAs and assigning weights to all the 9 stocks
portfolio_ret <- na.omit(portfolio_ret)
portfolio_pf <- Return.portfolio(portfolio_ret, c(.11,.11,.11,.11,.11,.11,.11,.11,.11),geometric = FALSE)
head(portfolio_ret)

#converting them to data frame
portfolio_ret <- as.data.frame(portfolio_ret)
head(portfolio_ret)
class(portfolio_ret)
portfolio_ret$dt <- row.names(portfolio_ret)

#output
write_xlsx(portfolio_ret,'PORTFOLIO_RETURNS.xlsx')

print(portfolio_ret)

#plotting
library(fPortfolio)
library(timeSeries)
portfolio_RETURN <- Return.calculate(portfolio_stks,method='discrete')
head(portfolio_RETURN)

portfolio_RETURN <- na.omit(portfolio_RETURN)
head(portfolio_RETURN)
class(portfolio_RETURN)

portfolio_returns_plot <- as.timeSeries(portfolio_RETURN)

portfolio_spec <- portfolioSpec()
constraints <- "LongOnly"

efficient_frontier <- portfolioFrontier(portfolio_RETURN,`setRiskFreeRate<-`(portfolioSpec(),.07/252),constraints="longonly")

efficient_frontier <- portfolioFrontier(
  data = portfolio_RETURN,
  spec = portfolio_spec,
  constraints = "LongOnly"
)

plot(efficient_frontier, c(1, 2, 3))

print(efficient_frontier)

plot(efficient_frontier, c(1,2,3,4))
plot(efficient_frontier, c(1,3,7))
plot(efficient_frontier, c(1,3,8))

cov_portfolio <- cov(portfolio_RETURN)

write_xlsx(cov_portfolio,"cov_portfolio_1")

mean(portfolio_RETURN)

print(cov_portfolio)

cov_portfolio_df <- as.data.frame(cov_portfolio)

write_xlsx(cov_portfolio_df, "cov_portfolio_1.xlsx")

print(cov_portfolio)

cov_portfolio_df_1 <- na.locf(cov_portfolio)



stock_prices <- Cl(portfolio_stks) # Closing Prices
# Plot the stock prices to see the trend
plot(stock_prices, main = "ASIANPAINT.NS STOCK PRICES", ylab = "Price", xlab = "Date")
# Fit an Arima Model
arima_model_prices <- auto.arima(stock_prices)
summary(arima_model_prices)
# Forecast for Stock Prices
forecast_prices <- forecast(arima_model_prices, h=300) # Forecast for next 300 days
plot(forecast_prices, main = "Arima forecast for the portfolio")
