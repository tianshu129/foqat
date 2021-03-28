#' get kOH
#'
#' Searches kOH value from 'chemspider.com'.
#'
#' Theoretical values of the species' OH reaction constant kOH at 25 degrees were obtained from 'Chemspider.com'. Value source: US Environmental Protection Agency's EPISuite.\cr
#' Unit is cm3/molecule-sec.\cr
#' Condition is 25 deg C.
#'
#' @param spec chemical specise to be searched. chemical specise's name or CAS Number is acceptable.
#' @return the theoretical value of the species' OH reaction constant kOH at 25 degrees.
#' @export
#' @import magrittr
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom utils download.file read.table
#' @examples
#' koh("CH4")

koh <- function(spec){
  url <- paste0(c("http://www.chemspider.com/Search.aspx?q=", spec), collapse='')
  p1 <- proc.time()
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  web <- read_html("scrapedpage.html")
  result_test<-web%>%html_nodes("h3")%>%html_text()
  if(result_test[1] == "Found 1 result"){
    name <- as.character(web %>% html_nodes(xpath="//pre"))#web%>%html_nodes("//pre")%>%html_text()
    namelist <- read.table(text=gsub("(?<=[a-z])\\s+", "\n", name, perl=TRUE), header=FALSE, col.names = c("name"), stringsAsFactors = FALSE)
    poi_unit<-which(namelist$name %in% "cm3/molecule-sec")
    poi_unit<-poi_unit[1]
    value <- namelist$name[poi_unit-2]
    order <- namelist$name[poi_unit-1]
    print(paste(c(spec, ' ', value, order, ' cm3/molecule-sec (25 deg C) [AopWin v1.92]'),collapse = ""))
  }else if(result_test[1] == "Found 0 result"){
    print("Not result.")
  }else{
    print("More than 1 result, please use CAS Number.")
  }
}
