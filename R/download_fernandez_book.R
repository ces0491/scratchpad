fernandez_links <- read.csv("./inst/extdata/fernandez_book_links.csv", stringsAsFactors = FALSE)

download_dir <- "C:/temp/chromeDL/fernandez"

dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
ecap_prefs <- list("profile.default_content_settings.popups" = 0L,
                   "safebrowsing.enabled" = FALSE,
                   "plugins.always_open_pdf_externally" = TRUE,
                   "download.prompt_for_download" = FALSE,
                   "download.default_directory" = download_dir)

for (n in 1:nrow(fernandez_links)) {
  
  already_dd <- base::list.files("C:/temp/chromeDL/fernandez", recursive = TRUE)
  
  if (identical(already_dd, character(0))) {
    clean_inputs <- fernandez_links %>% 
      dplyr::mutate(link = trimws(link)) %>% 
      dplyr::rename(ch = 1)  
  } else {
    clean_inputs <- fernandez_links %>% 
      dplyr::mutate(link = trimws(link)) %>% 
      dplyr::rename(ch = 1) %>% 
      dplyr::filter(!ch %in% already_dd)
  }
  
  link <- clean_inputs[n,3]
  ch_no <- clean_inputs[n,1]
  ch_ttl <- clean_inputs[n,1]
  
  # start a selenium server and browser
  remDr <- NULL
  attempt <- 0
  while(is.null(remDr) && attempt <= 3) { # connection may fail on 1st attempt so retry up to 3 times
    attempt <- attempt + 1
    try(
      remDr <- fractalDataImportR::start_remote_session(url = link, ecap_prefs, debug = FALSE)
    )
  }
  
  remDr$setImplicitWaitTimeout()

  remDr$executeScript("return document.readyState == 'complete';")
  
  # click download button
  dwnldbtnElem <- remDr$findElement(using = 'xpath', value = '//*[@id="downloadPdf"]')
  dwnldbtnElem$clickElement()
  
  # click sign in to download
  signinElem <- remDr$findElement(using = 'xpath', value = '//*[@id="signinLink"]')
  signinElem$clickElement()
  
  # enter credentials
  emailElem <- remDr$findElement(using = "xpath", value = '//*[@id="maincontent"]/div/div[1]/div/form/div[1]/input')
  emailElem$clickElement()
  emailElem$clearElement()
  emailElem$sendKeysToActiveElement(list("cesairetobias@hotmail.com"))
  
  passElem <- remDr$findElement(using = "xpath", value = '//*[@id="maincontent"]/div/div[1]/div/form/div[2]/input')
  passElem$clickElement()
  passElem$clearElement()
  passElem$sendKeysToActiveElement(list("Iamrocket21"))
  
  # hit sign in - download should begin
  dwnldElem <- remDr$findElement(using = "xpath", value = '//*[@id="signinBtn"]')
  dwnldElem$clickElement()
  
  prog <- which(n == 1:nrow(clean_inputs))
  
  progress <- round(prog/nrow(clean_inputs), 2) * 100
  message(glue::glue("attempting to retrieve chapter {ch_no} from {link}"))
  
  Sys.sleep(20) # wait 20s for download
  message(glue::glue("{progress}% complete"))
  remDr$close()
  gc()
  
  # file.rename(list.files(download_dir), pattern = "SSRN", ch_no)
}
