library("htmlunit")

url <- "https://www.sharenet.co.za/v3/sharesfound.php?idx=J203&exch=JSE"

# Tell HtmlUnit to work like Chrome:
browsers <- rJava::J("com.gargoylesoftware.htmlunit.BrowserVersion")
jwc <- rJava::J("com.gargoylesoftware.htmlunit.WebClient")
wc <- rJava::new(jwc, browsers$CHROME)

# Tell it to wait for javascript to execute and not throw exceptions on page resource errors:

base::invisible(wc$waitForBackgroundJavaScriptStartingBefore(rJava::.jlong(2000L)))

wc_opts <- wc$getOptions()
wc_opts$setThrowExceptionOnFailingStatusCode(FALSE)
wc_opts$setThrowExceptionOnScriptError(FALSE)

# Acccess the site and get the table:
  
pg <- wc$getPage(url)

doc <- xml2::read_html(pg$asXml())

tmp_tbl <- rvest::html_table(doc)

alsi_securities <- read.csv("./inst/extdata/alsi_constituents.csv", stringsAsFactors = FALSE)
alsi <- alsi_securities %>% 
  dplyr::mutate(yahoo_ticker = paste0(Code, ".JO"))

url_yahoo <- "https://finance.yahoo.com/quote/THA.JO/financials?p=THA.JO"
pg_yahoo <- wc$getPage(url_yahoo)
search_bar <- pg_yahoo$getElementById("fin-srch-assist")
search_bar$setTextContent("THA.JO")
search_btn <- pg_yahoo$getElementById("search-button")
search_btn$click()
pg_ticker <- wc$getCurrentWindow()
current_pg <- pg_ticker$getEnclosedPage()
xml_ticker <- xml2::read_html(pg_yahoo$asXml())
tmp_tbl <- rvest::html_table(xml_ticker)





