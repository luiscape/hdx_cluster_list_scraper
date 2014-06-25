## Script to scrape the data from HumanitarianResponse.info about
## the presence of clusters in countries around the world.

## HTML Structure
# views-group class for the cluster name
# field-content class for the country that has it

library(XML)
library(RCurl)
library(stringr)  # not sure if it works with ScraperWiki

# base url
url <- 'https://www.humanitarianresponse.info/clusters/countries'
doc <- htmlTreeParse(getURL(url), useInternal = TRUE)
clusters <- xpathApply(doc, "//div[@class='views-group']", xmlValue)

# from stackoverflow
cbind.fill<-function(...){
    nm <- list(...) 
    nm<-lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
        rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

for (i in 1:length(clusters)) {
    a <- str_replace_all(unlist(clusters[i]), "[\r\n]", "")
    b <- str_replace_all(a, "[\r\t]", "")
    c <- str_replace_all(b, "                  ", "\n")
    d <- str_replace_all(c, "    ", "")
    e <- str_replace_all(d, "  ", "")
    f <- str_replace_all(e, "\n", ",")
    
    # Writing a text file for reading the ',' as separators.
    write.table(as.character(e), 'temp.txt', 
                row.names = F, col.names = F, quote = F)
    cluster <- read.table('temp.txt', header = T, strip.white = T, 
                    fill = T, sep = "\n", quote = "")
    if (i == 1) z <- cluster
    else z <- cbind.fill(z, cluster)
}


## Using CPS format.

# indicator

# dataset
scrape_time <- Sys.time()
last_updated <- Sys.time()
dsID <- 'clusters'
dataset <- data.frame(scrape_time, last_updated, dsID)

# value
value <- z

# configuration


## Writing output
write.csv(value, 'data/sample.csv', row.names = F)
