
library(ggplot2)
library(dplyr)
library(foreign)
library(gridExtra)

vote <- read.csv("C:/pres16results.csv", stringsAsFactors = FALSE)

census <- read.dta("C:/relcensus.dta", convert.factors = FALSE)

#census <- read.csv("C:/trump_elect/cenfix.csv", stringsAsFactors = FALSE)


census <- select(census, fips, evanrate, stname, cathrate)

#check <- rbind(census, fix)


vote$fips <- gsub("(?<![0-9])0+", "", vote$fips, perl = TRUE)


merge <- merge(census, vote, by=c("fips"))

pres <- filter(merge, cand_name == "Donald Trump" | cand_name == "Hillary Clinton")

trump <- filter(merge, cand_name == "Donald Trump")
clinton <- filter(merge, cand_name == "Hillary Clinton")

my_theme <- function() {
  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]
  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") +
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    # Format grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    # Format legend
    theme(legend.position="right") +
    theme(legend.background = element_rect(fill=color.panel)) +
    theme(legend.text = element_text(size=10,color=color.axis.title)) +
    # Format title and axes labels these and tick marks
    theme(plot.title=element_text(color=color.title, size=20, vjust=0.5, hjust=0, face="bold")) +
    theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=-1, face="italic")) +
    theme(axis.title.y=element_text(size=12,color=color.axis.title, vjust=1.8, face="italic")) +
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}



trump$diff <- trump$pct - clinton$pct
ggplot(trump, aes(x=cathrate, y=pct))+
  my_theme()+
  geom_point(shape=1) +
  geom_smooth()+
  labs(title= "", y="Trump Vote Share", x="Evangelical Adherence Rate")+
  ggtitle(expression(atop(bold("Trump and Evangelicals"), atop(italic("Association between Trump Vote Share and Evangelical Adherence"),""))))+
  theme(plot.title = element_text(size = 16, face = "bold", colour = "black", vjust = 0.5, hjust=0.5))


trump$region <- trump$fips
trump$value <- trump$diff
palette_rev <- rev(brewer.pal(8, "RdBu"))

choro = CountyChoropleth$new(trump)
choro$title = "Trump Vote Share"

choro$set_num_colors(1)




choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = palette_rev)
choro$render()


trump$region <- trump$fips
trump$value <- trump$evanrate
palette_rev <- rev(brewer.pal(8, "RdBu"))

choro = CountyChoropleth$new(trump)
choro$title = "Trump Vote Share"

choro$set_num_colors(1)




choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = palette_rev)
choro$render()