library("magrittr")
path <- "J:/Temp/Cesaire/performance/Raw data_LM_offsite.xlsx" #specify path to raw data

untidy.xl.data <- tidyxl::xlsx_cells(path) #read in raw data as a tibble

untidy.xl.data.sorted <- untidy.xl.data %>% 
  dplyr::group_by(sheet) %>% 
  dplyr::arrange(sheet, row, col) %>% #arrange read in data by row and col number rather than the excel cell ref
  dplyr::ungroup()

funds <- unique(untidy.xl.data.sorted$sheet) #get the sheet names - fund names here
#dt.pattern <- "^([0-9]{2})[//]([0-9]{2})[//]([0-9]{4})$"
start.row <- 4 # row data starts in - same across all sheets here

tidy.data <- list() #create a list to write data to in loop

for (f in funds) { # loop through sheets to tidy each respectively
  untidy.xl.data.f <- untidy.xl.data.sorted %>% # get the sheet//fund (f) in read in data that's been sorted
    dplyr::filter(sheet == f)

  dts <- untidy.xl.data.f %>% 
    #dplyr::mutate(date = stringr::str_extract(.data$character, dt.pattern)) %>%
    dplyr::mutate(date = stringr::str_replace(.data$character, "^[^-]*-", "")) %>%  # get dates - these will be returned as strings
    dplyr::mutate(date = as.Date(date, "%d/%m/%Y")) %>% # convert dates to Date objects from character
    dplyr::select(sheet, row, date) %>% 
    tidyr::drop_na()
  
  top_left <- tibble::tibble(row = start.row, col = 1) # row col ref for where data starts in sheet
  
  bottom_right <-   untidy.xl.data.f %>% # row col ref for where data ends in sheet
    dplyr::filter(data_type == "numeric") %>%
    dplyr::summarise(row = max(row),
                     col = max(col)) %>% 
    dplyr::ungroup()
  
  reqd.data <- untidy.xl.data.f %>% # filter out the data we're interested in in our sheet
    dplyr::filter(dplyr::between(row, top_left$row, bottom_right$row),
                  dplyr::between(col, top_left$col, bottom_right$col))
  
  weight_cols <- reqd.data %>% #get weight cols
    dplyr::filter(stringr::str_detect(character, "Weight")) %>% 
    dplyr::filter(row == start.row) %>% 
    dplyr::select(col, character) %>% 
    dplyr::distinct(col, character)
  weights <- reqd.data %>% #get weights
    dplyr::select(sheet, row, col, numeric) %>%
    dplyr::filter(col %in% weight_cols$col) %>% 
    tidyr::drop_na() %>% 
    dplyr::left_join(weight_cols, by = "col") %>% 
    dplyr::select(-col) %>% 
    tidyr::spread(character, numeric)
  
  return_cols <- reqd.data %>% #get return cols
    dplyr::filter(stringr::str_detect(character, "Return")) %>% 
    dplyr::filter(row == start.row) %>% 
    dplyr::select(col, character) %>% 
    dplyr::distinct(col, character)
  returns <- reqd.data %>% #get returns
    dplyr::select(sheet, row, col, numeric) %>%
    dplyr::filter(col %in% return_cols$col) %>% 
    tidyr::drop_na() %>% 
    dplyr::left_join(return_cols, by = "col") %>% 
    dplyr::select(-col) %>% 
    tidyr::spread(character, numeric)
  
  contribution_col <- reqd.data %>% #get contribution cols from spreadsheet
    dplyr::filter(stringr::str_detect(character, "Contribution")) %>% 
    dplyr::filter(row == start.row) %>% 
    dplyr::select(col, character) %>% 
    dplyr::distinct(col, character)
  contrib <- reqd.data %>% #get contribution data
    dplyr::select(sheet, row, col, numeric) %>%
    dplyr::filter(col %in% contribution_col$col) %>% 
    tidyr::drop_na() %>% 
    dplyr::left_join(contribution_col, by = "col") %>% 
    dplyr::select(-col) %>% 
    tidyr::spread(character, numeric)
  
  sec_id_cols <- reqd.data %>% #get security id columns
    dplyr::filter(stringr::str_detect(character, "Security")) %>% 
    dplyr::filter(row == start.row) %>% 
    dplyr::select(col, character) %>% 
    dplyr::distinct(col, character)
  security_names <- reqd.data %>% #get security names - can be character or numeric depending on whether sec code or full name
    dplyr::select(sheet, row, col, character, numeric) %>%
    dplyr::filter(col %in% sec_id_cols$col) %>% 
    dplyr::filter(row != start.row) %>% 
    dplyr::mutate(sec_name = ifelse(is.na(character), numeric, character)) %>% 
    dplyr::mutate(sec_name = stringr::str_trim(sec_name)) %>% 
    dplyr::select(-character, -numeric) %>% 
    dplyr::left_join(sec_id_cols, by = "col") %>% 
    dplyr::select(-col) %>% 
    tidyr::spread(character, sec_name)
  
  sfi_data <- weights %>% #combine weights, returns, contribs and sec names by row number
    dplyr::left_join(returns, by = c("sheet", "row")) %>% 
    dplyr::left_join(contrib, by = c("sheet", "row")) %>% 
    dplyr::left_join(dts, by = c("sheet", "row")) %>%
    dplyr::left_join(security_names, by = c("sheet", "row")) %>%
    dplyr::mutate(sec_name_lgl = ifelse(is.na(date), TRUE, FALSE)) %>% #create helper col to seperate dates from sec id's
    dplyr::mutate(`Security Name` = ifelse(sec_name_lgl, `Security Name`, NA)) %>% 
    dplyr::mutate(date = dplyr::lead(date)) %>% 
    tidyr::fill(`Security Name`) %>% 
    tidyr::drop_na(date) %>% 
    dplyr::select(sheet, date, `Main Security Code`, `Security Name`, 
                  `Weight Beginning`, `Weight End`, `Weight Mean`, 
                  `Return (in ZAR)`, `Return Flat (in ZAR)`, `Contribution (in ZAR)`)
  
  tidy.data[[f]] <- sfi_data
}

tidy.data # here's our list from row 15 that's now populated with the tidied data above

tidy.data.df <- do.call("rbind", tidy.data) #turn list into a data.frame

dupe.check <- tidy.data.df %>% #check for duplicates
  dplyr::group_by(date, `Main Security Code`) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::filter(n > 1)

#write.csv(tidy.data.df, "./tidied_mm_security_returns.csv", row.names = FALSE) # uncomment to write to csv
