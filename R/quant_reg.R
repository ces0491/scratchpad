# pgm_wide <- read.csv("C:/Users/Cesaire Tobias/Documents/projects/pgm_prices.csv", fileEncoding = "UTF-8-BOM") %>% 
#   dplyr::mutate(date = as.Date(date))

qs <- c(0.1, 0.5, 0.9)

start.dt <- as.Date("2000-01-31")
end.dt <- as.Date("2019-02-28")

raw_data <- fractalDataImportR::import_market_data(c("AVST-AMS.JO", "AVST-ZAR=X", "Q-LPPM/PLAT"), start.dt, end.dt, periodicity = "monthly")

# spread data to handle yahoo stupidity of mixing rand and cents quotes and to put rand stock price in to usd
raw_data_wide <- raw_data %>% 
  dplyr::filter(variable == "close") %>%
  dplyr::group_by(variable) %>% 
  dplyr::mutate(value = ifelse(value == 0, dplyr::lag(value), value)) %>%
  dplyr::ungroup() %>% 
  tidyr::spread(ticker, value) %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate(AMS.JO = ifelse(AMS.JO >= 1000, AMS.JO / 100, AMS.JO)) %>% 
  dplyr::mutate(AMS.JO = ifelse(AMS.JO <= 100, AMS.JO * 100, AMS.JO)) %>% 
  dplyr::mutate(AMS = AMS.JO / `ZAR=X`) %>%
  dplyr::mutate(AMS = ifelse(!is.finite(AMS), dplyr::lag(AMS), AMS)) %>% 
  dplyr::rename(PT = `LPPM/PLAT`)

# now gather the spread data to get log diffs then spread again
plat_diffl <- raw_data_wide %>% 
  dplyr::select(date, AMS, PT) %>% 
  tidyr::gather(variable, value, -date) %>%
  dplyr::group_by(variable) %>% 
  dplyr::mutate(diffl = log(value / dplyr::lag(value))) %>%
  dplyr::ungroup() %>% 
  dplyr::select(-value) %>% 
  tidyr::spread(variable, diffl) %>% 
  tidyr::drop_na()

plat_scatter <- ggplot2::ggplot(plat_diffl, ggplot2::aes(PT, AMS)) + 
  ggplot2::geom_point()

histo <- ggplot2::ggplot(plat_diffl) +
  ggplot2::geom_histogram(ggplot2::aes(x = AMS))

quant_reg <- quantreg::rq(AMS ~ PT, data = plat_diffl, tau = qs)
ols <- stats::lm(AMS ~ PT, data = plat_diffl)

qr_plot <- ggplot2::ggplot(plat_diffl, ggplot2::aes(PT, AMS)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_quantile(quantiles = qs, colour = "red", size = 2, alpha = 0.5) + 
  ggplot2::geom_smooth(method = "lm", se = FALSE)

coeff_plot <- plot(summary(quant_reg), parm = "PT")

#############################################

plat_lvl <- raw_data_wide %>% 
  dplyr::select(date, AMS, PT)

plat_lvl_scatter <- ggplot2::ggplot(plat_lvl, ggplot2::aes(PT, AMS)) + 
  ggplot2::geom_point()

histo_lvl <- ggplot2::ggplot(plat_lvl) +
  ggplot2::geom_histogram(ggplot2::aes(x = AMS))

quant_reg_lvl <- quantreg::rq(AMS ~ PT, data = plat_lvl, tau = qs)
ols <- stats::lm(AMS ~ PT, data = plat_lvl)

qr_plot_lvl <- ggplot2::ggplot(plat_lvl, ggplot2::aes(PT, AMS)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_quantile(quantiles = qs, colour = "red", size = 2, alpha = 0.5) + 
  ggplot2::geom_smooth(method = "lm", se = FALSE)

coeff_plot <- plot(summary(quant_reg_lvl), parm = "PT")

###################

cor(plat_diffl$AMS, plat_diffl$PT)
# corrplot::corrplot(cor(plat_diffl$AMS, plat_diffl$PT))
