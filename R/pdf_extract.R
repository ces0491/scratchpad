# aligning executive incentives with EP

pdf_dir <- "C:/Users/Cesaire Tobias/Fractal Value Advisors/Fractal Value Advisors - Documents/01 Education/External Materials/Remuneration and Metrics"

#############################################

get_files <- function(data_dir, file_type, fullnames = TRUE, recurse = FALSE) {
  
  file_name <- base::list.files(data_dir, full.names = fullnames, recursive = recurse)
  
  file_names_df <- file_name %>%
    tibble::tibble(file_path = .) %>%
    dplyr::filter(grepl(file_type, file_path)) %>% 
    dplyr::mutate(file_name = gsub(".*[//]([^.]+)[.].*", "\\1", file_path))
  
  file_names_df
}

get_pdf_data <- function(pdf_dir) {
  
  reqd_files <- get_files(pdf_dir, file_type = "pdf")
  
  pdf_data <- reqd_files %>% 
    dplyr::mutate(raw_pdf = purrr::map(file_path, pdftools::pdf_text)) %>% 
    dplyr::mutate(linebreak_pdf = purrr::map(raw_pdf, base::strsplit, "\n")) %>% 
    dplyr::mutate(enframed_pdf = purrr::map(linebreak_pdf, tibble::enframe))
  
  pdf_data_tbl <- pdf_data %>% 
    dplyr::select(file_name, enframed_pdf) %>% 
    tidyr::unnest() %>% 
    dplyr::rename("pg_no" = "name",
                  "content" = "value")
}

get_headers <- function(pdf_data, no_cols) {
  
  raw_headers <- pdf_data %>% 
    dplyr::filter(leading_space) %>% 
    dplyr::select(-white_space, -leading_space)
  
  h_list <- list()
  for (n in 1:nrow(raw_headers)) {
    h <- headings[n, ]
    h_sep <- scan(text = h[[1]], what = "")
    h_list[[n]] <- h_sep  
  }
  
  headers_list <- list()
  for (n in 1:no_cols) {
    h_map <- purrr::map(h_list, n)
    h_unlist <- unlist(h_map)  
    h_full <- paste(h_unlist, collapse = " ")
    headers_list[[n]] <- h_full
  }
  
  headers <- unlist(headers_list)
  headers[headers != ""] 
  
}

get_pdf_table_single <- function(raw_pdf_tbl) {
  
  fractalAssert::assert_true(length(raw_pdf_tbl), "logic error")
  
  pdf_data <- tibble::tibble(pdf_data = raw_pdf_tbl[[1]]) %>% 
    dplyr::mutate(white_space = stringr::str_detect(pdf_data, "   ")) %>% # anything that's a sentence should just have single spacing
    dplyr::mutate(leading_space = startsWith(pdf_data, " ")) %>%  # find leading whitespace 
    dplyr::mutate(pdf_data = stringr::str_remove(pdf_data, "\\r")) # remove row separator
    
  pdf_tbl <- pdf_data %>% 
    dplyr::filter(white_space) %>% # remove sentences
    dplyr::filter(!leading_space) %>% # remove headings
    dplyr::select(-white_space, -leading_space)
      
  pdf_tbl_clean <- pdf_tbl %>% 
    dplyr::mutate(pdf_data = gsub(",", "", pdf_data)) %>% # remove commas 
    dplyr::mutate(pdf_data = gsub("\\$", "", pdf_data)) # remove dollar signs
  
  no_cols <- lengths(strsplit(pdf_tbl_clean[[1]], "\\W+"))[[1]]

  headers <- get_headers(pdf_data, no_cols)
    
  row_id <- pdf_tbl_clean %>% 
    tidyr::separate(pdf_data, "ID", sep = "  ", drop = TRUE)
  
  pdf_tbl_sep <- pdf_tbl_clean %>% 
    dplyr::mutate(pdf_data = gsub("\\s", ",", pdf_data)) %>% 
    tidyr::separate(pdf_data, LETTERS[1:no_cols], sep = "_")
                    


}

# get_pdf_table <- function() {
#   
# }

#pdf_data_tbl <- get_pdf_data(pdf_dir)
#pdf_tables_tbl <- get_pdf_table()



# read <- tm::readPDF(control = list(text = "-layout"))
# pdf_file <- tm::Corpus(tm::URISource(d_pdf_dir), readerControl = list(reader = read))
# pdf_doc <- NLP::content(pdf_file[[1]])
# 
# head(pdf_doc)
# 
# page_breaks <- grep("\\f", pdf_doc)
# pdf_doc[page_breaks[1]]
