exxaro_price_data <- read.csv("C:/Users/Cesaire Tobias/Documents/Exxaro_price_data.csv", stringsAsFactors = FALSE)

exxaro_price_data[,1] <- as.Date(exxaro_price_data[,1], format = "%Y/%m/%d")
colnames(exxaro_price_data) <- c("date", "price")

monthly_exxaro <- dateUtils::to_monthly(exxaro_price_data)

exxaro_df <- monthly_exxaro %>% 
  dplyr::mutate(ln_price = log(price)) %>% 
  dplyr::mutate(return = price / dplyr::lag(price) - 1) %>% 
  tidyr::drop_na()



plot(x = exxaro_df$date, y = exxaro_df$price, type = "l")
plot(x = exxaro_df$date, y = exxaro_df$return, type = "l") 

# for normal distro
xseq <- seq(-0.4, 0.4, length.out = 1000)
norm_dist <- dnorm(xseq, 0, 0.1)
#plot(xseq, norm_dist, col="blue", type="l")

hist(exxaro_df$return, breaks = 30, probability = TRUE, col="gray", border="white") 
d <- density(exxaro_df$return)
lines(d, col="red")
lines(x = xseq, y = norm_dist, col = "blue")


fUnitRoots::adfTest(monthly_exxaro$price, lags = 0, type = "nc")

price_sd <- stats::sd(monthly_exxaro$price)
price_drift

price_roll_stats <- monthly_exxaro %>% 
  dplyr::mutate(rolling_sd = RcppRoll::roll_sdr(.$price, n = 36)) %>% 
  dplyr::mutate(rolling_drift = )