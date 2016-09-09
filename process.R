
library(rjson)

#### LOAD PUBLISHER

org.files <- fromJSON(file="iati/orgfiles.json")
results <- org.files$result$results


pub <- data.frame(name = character(0),
                 iati_id = character(0), 
                 country = character(0),
                 source_type = character(0),
                 org_type = integer(0), stringsAsFactors = FALSE)

nz <- function(s) if(is.null(s)) NA else s

for(i in seq_along(results)) {
  result <- results[[i]]
  extras <- result$extras
  names(extras) <- sapply(result$extras, function(e) e$key)
  
  pub[i, "name"] <- nz(result$title)
  pub[i, "iati_id"] <- nz(extras$publisher_iati_id$value)
  pub[i, "country"] <- nz(extras$publisher_country$value)
  pub[i, "source_type"] <- nz(extras$publisher_source_type$value)
  pub[i, "org_type"] <- nz(extras$publisher_organization_type$value)
}

# Remove duplicates
pub <- pub[!duplicated(pub$iati_id), ]

# Remove those without names
pub <- pub[!is.na(pub$name), ]
pub$name <- gsub(pub$name, pattern = "\\s*organi[sz]ation file", replacement = "", ignore.case = TRUE)

### LOAD SCORES

scores <-readr::read_delim("iati/scores.tsv", delim="\t")
names(scores) <- c("iati_id", "activity_id", "score1", "ref_score", "tx_eur", "tx_count")

pubscore <- aggregate(ref_score ~ iati_id, data = scores, FUN = mean, na.action = na.omit)

### Combine scores and publishers

sumtab <- merge(pub, pubscore, all.x = TRUE)
sumtab$ref_score[ is.na(sumtab$ref_score) ] <- 0

countries <- sort(unique(pub$country))

for(country in countries) {
  cat(sprintf("%s\n", country))
  country.scores <- sumtab[sumtab$country == country, ]
  country.scores <- country.scores[!is.na(country.scores$name), ]
  write.csv(file = sprintf("data/%s.csv", country), 
            row.names = FALSE,
            country.scores)
}
