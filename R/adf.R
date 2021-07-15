risk_return_data <- read.csv("./sa_risk_return_data.csv", stringsAsFactors = FALSE)

library(urca)
sa_yield_df <- urca::ur.df(risk_return_data$X10y_yield, lags = 1, type = "trend") # adf with lag of 1 and constant and trend
summary(sa_yield_df)

tseries::adf.test(risk_return_data$X10y_yield, k = 1)
