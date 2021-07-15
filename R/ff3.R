
test_asset <- fractalDataImportR::get_yahoo_data(c("MFL.JO"), 
                                                 frequency = "monthly", 
                                                 page = "price", 
                                                 start_date = "2008-01-31", 
                                                 end_date = "2019-03-31")

asset_returns <- test_asset$clean_data[[1]] %>% 
  dplyr::filter(variable == "AdjClose") %>% 
  dplyr::mutate(return = value / dplyr::lag(value) - 1) %>% 
  dplyr::mutate(name = test_asset$ticker[[1]]) %>% 
  dplyr::select(date, name, return)

ff3_data <- fractalDataImportR::get_legae_factor_data(reqd_file = "long_short_alsi", reqd_factor = "ff3", weight = "equal")

all_data <- asset_returns %>% 
  dplyr::left_join(ff3_data, by = "date")

lm_market <- function(df) {
  lm(return ~ Market, data = df)
}

lm_3f <- function(df) {
  lm(return ~ Market + Size + Value, data = df)
}

fit_scatter <- all_data %>% 
  dplyr::group_by(name) %>% 
  dplyr::do(model = lm_3f(.)) %>% 
  broom::augment(model) %>% 
  ggplot2::ggplot(ggplot2::aes(y = .resid, x = .fitted)) +
  ggplot2::geom_point(color = "cornflowerblue")

all_data_nested <- all_data %>% 
  dplyr::group_by(name) %>% 
  tidyr::nest()

multi_mod <- all_data_nested %>%
  dplyr::mutate(model_1f = purrr::map(data, lm_market),
                model_3f = purrr::map(data, lm_3f),
                tidy_1f = purrr::map(model_1f, broom::tidy),
                tidy_3f = purrr::map(model_3f, broom::tidy),
                glanced_1f = purrr::map(model_1f, broom::glance),
                glanced_3f = purrr::map(model_3f, broom::glance),
                augmented_1f = purrr::map(model_1f, broom::augment),
                augmented_3f = purrr::map(model_3f, broom::augment))

model_perf <- multi_mod %>% 
  dplyr::select(name, model_1f, model_3f) %>% 
  tidyr::gather(models, results, -name) %>% 
  dplyr::mutate(glanced_results = purrr::map(results, broom::glance)) %>%
  tidyr::unnest(glanced_results) %>% 
  dplyr::select(name, models, adj.r.squared, AIC, BIC)


comp_mods_plot <- model_perf %>% 
  ggplot2::ggplot(ggplot2::aes(x = reorder(models, adj.r.squared), y = adj.r.squared, color = models)) + 
  ggplot2::geom_point(show.legend = NA) +
  ggplot2::geom_text(ggplot2::aes(label = models), nudge_y = .001) +
  ggplot2::labs(x = "", 
           title = "1 vs 3 Factor Model Comparison") +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank())
