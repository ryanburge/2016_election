vote <- read.csv("D:/2016_election/pres16results.csv", stringsAsFactors = FALSE)
vote$fips <- gsub("(?<![0-9])0+", "", vote$fips, perl = TRUE)

pres12 <- read.csv("twelve.csv", stringsAsFactors = FALSE)
pres12$obamapct <- as.numeric(pres12$obamapct)
pres12$romneypct <- as.numeric(pres12$romneypct)
pres12$fips <- pres12$FIPS
pres12$FIPS <- NULL
merge <- merge(vote, pres12, by=c("fips"))

trump <- filter(merge, cand_name == "Donald Trump")
df <- trump
df$obamapct <- df$obamapct * .01
df$romneypct <- df$romneypct * .01

df$trumppct <- df$pct
df$pct <- NULL


df$diff <- df$trumppct - df$romneypct
df$region <- df$fips
df$value <- df$diff

choro = CountyChoropleth$new(df)
choro$title = "                         How Did Trump Do in Rural Counties?                                "
choro$set_num_colors(1)
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Trump Share", colours = palette_rev)
choro$render()