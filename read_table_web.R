# lit une table dans une page web
#
# usage fiaTableGrabber(adresse web de la page, n°de la table)
# source [Scraping Web Pages With R](http://blog.ouseful.info/2015/04/15/scraping-web-pages-with-r/)
# exemples
# url2 <- "http://fr.wikipedia.org/wiki/Liste_des_communes_du_Bas-Rhin"
# communes67 <- fiaTableGrabber(url2, 1)
#
# url <- "http://www.fia.com/events/formula-1-world-championship/season-2015/qualifying-classification"
# fiaTableGrabber(url, 1)
# fiaTableGrabber(url, 2)
# fiaTableGrabber(url, 4)

fiaTableGrabber=function(url,num){
  #Grab the page
  hh=html(url)
  #Parse HTML
  cc=html_nodes(hh, xpath = "//table")[[num]] %>% html_table(fill=TRUE)
  #TO DO - extract table name
  
  #Set the column names
  colnames(cc) = cc[1, ]
  #Drop all NA column
  cc=Filter(function(x)!all(is.na(x)), cc[-1,])
  #Fill blanks with NA
  cc=apply(cc, 2, function(x) gsub("^$|^ $", NA, x))
  #would the dataframe cast handle the NA?
  as.data.frame(cc)
}