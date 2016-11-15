s.census <- select(census, evanrate, bprtrate, cathrate, ldsrate, orthrate, mprtrate)
s.census[is.na(s.census)]<- 0
s.census$max <- apply(s.census, 1, max)

s.census$fips <- census$fips
s.census$test <- 1308.69 - s.census$max
s.census$test <- s.census$test/1308.69


vote <- read.csv("pres16results.csv", stringsAsFactors = FALSE)
vote$fips <- gsub("(?<![0-9])0+", "", vote$fips, perl = TRUE)


cmerge <- merge(s.census, vote, by=c("fips"))

trump <- filter(cmerge, cand_name == "Donald Trump")

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

ggplot(trump, aes(x=test, y=pct))+
  my_theme()+
  geom_point(shape=1) +
  geom_smooth()+
  labs(title= "", y="Trump Vote Share", x="Religious Diversity")+
  ggtitle(expression(atop(bold("Trump and Religious Diversity"), atop(italic("Higher Values = More Diversity"),""))))+
  theme(plot.title = element_text(size = 16, face = "bold", colour = "black", vjust = 0.5, hjust=0.5))



div_filter <- filter(trump, test >= .35)

ggplot(div_filter, aes(x=test, y=pct))+
  my_theme()+
  geom_point(shape=1) +
  geom_smooth()+
  labs(title= "", y="Trump Vote Share", x="Religious Diversity")+
  ggtitle(expression(atop(bold("Trump and Religious Diversity"), atop(italic("Higher Values = More Diversity"),""))))+
  theme(plot.title = element_text(size = 16, face = "bold", colour = "black", vjust = 0.5, hjust=0.5))


