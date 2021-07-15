corr_data <- tmp %>% # tmp <- clipboardR::paste EBITDA % data
  tidyr::spread(unit, value) %>% 
  dplyr::select(-year, -description)

flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
  )
}

corr_mx <- cor(corr_data, use = "pairwise.complete.obs")
  
corr_data_flat <- flattenCorrMatrix(corr_mx)
  
corrplot::corrplot(corr_mx, method = "number", order = "AOE")
