Germany_aaw <- read.csv("E:\\School - work\\RA\\Germany WIP\\latest_DE.csv")

# links string
links <- Germany_aaw[, 1]
links <- gsub("/entity/", "/wiki/Item:", links)
links <- links[1:100]

# web scripting the benifactor names
get_benifactors <- function(links) {
  page <- read_html(links)
  benifactors <- page %>%
    html_elements("body #P841 .wikibase-snakview-value.wikibase-snakview-variation-valuesnak") %>%
    html_text() %>%
    paste(collapse = ",")
  return(benifactors)
}

benifactors <- sapply(links, FUN = get_benifactors)

