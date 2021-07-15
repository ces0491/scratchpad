sample_pdf <- "C:/Users/Cesaire Tobias/Documents/projects/dash/inst/extdata/CTFOR_21KM_RESULTS_2019.pdf"

header_rows <- 4 # the number of lines of text in the header is n - 1 because the first line will become the df colnames

raw_pdf_data_list <- tabulizer::extract_tables(sample_pdf, pages = c(1:41), output = "data.frame")

n_pg <- length(raw_pdf_data_list)

pg1_sub <- raw_pdf_data_list[[1]] %>% 
  dplyr::slice(-1:-header_rows) %>% 
  dplyr::select_if(~sum(!is.na(.)) > 0) # If the count of NAs in a column is equal to the number of rows, it must be entirely NA
  
colnames(pg1_sub) <- pg1_sub[1,]

pg1_tidy <- pg1_sub %>%
  dplyr::slice(-1) %>%
  dplyr::mutate(POS = as.numeric(POS),
                AGE = as.numeric(AGE),
                TIME = chron::times(TIME))

reqd_cols <- colnames(pg1_tidy)
# raw_pdf_data_df <- tibble::enframe(raw_pdf_data_list, name = "page", value = "data")

tidy_pages <- function(raw_pdf_data_list, pg_no, reqd_cols) {
  
  pgN_sub <- raw_pdf_data_list[[pg_no]] %>% 
    dplyr::select_if(~sum(!is.na(.)) > 0)
  
  
  pgN_colnames <- colnames(pgN_sub) # the colnames of page n are actual data, these need to be converted and appended to the df body
  
  # use df as tbl drops attributes required for chron time object
  pgN_new_row <- data.frame(newrow_vals = pgN_colnames, nrow = c(1:length(pgN_colnames)), stringsAsFactors = FALSE) %>% 
    tidyr::spread(nrow, newrow_vals) %>% 
    dplyr::select_if(~sum(!is.na(.)) > 0)
  
  colnames(pgN_new_row) <- reqd_cols
  colnames(pgN_sub) <- reqd_cols
  
  pgN_sub_reclass <- pgN_sub %>% 
    dplyr::mutate(POS = as.numeric(POS),
                  AGE = as.numeric(AGE))
  
  pgN_tidy <- pgN_new_row %>%
    dplyr::mutate(POS = gsub("X", "", POS),
                  AGE = gsub("X", "", AGE),
                  `LIC No` = gsub("X", "", `LIC No`),
                  TIME = gsub("X", "", TIME),
                  TIME = gsub("\\.", ":", TIME)) %>% 
    dplyr::mutate(POS = as.numeric(POS),
                  AGE = as.numeric(AGE)) %>% 
    dplyr::bind_rows(pgN_sub_reclass) %>% 
    dplyr::mutate(TIME = chron::times(TIME))
  
  pgN_tidy
}

clean_data_list <- list()
for (pg in 2:n_pg) {
  clean_data <- tidy_pages(raw_pdf_data_list, pg_no = pg, reqd_cols)
  clean_data_list[[pg]] <- clean_data
}

clean_data_list[[1]] <- pg1_tidy # because our index begins at 2, the 1st element of the list is NULL. we ascribe pg1 to 1st NULL element of the list

all_data_tidy <- reshape::merge_all(clean_data_list)
