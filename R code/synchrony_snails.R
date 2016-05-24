library(tidyr)
library(dplyr)
library(codyn)
library(ggplot2)
library(fields)

# Snail data from Mike Willig (Luquillo)
# only use data from the wet season
dat <- read.csv("~/Dropbox/LTER_synch_data/Long-term snail MNKA data.csv") %>%
  tbl_df() %>%
  filter(season == "w") 

# Create a key linking each Point with its X Y coordiates
key <- dat  %>%
  select(Point, X, Y) %>%
  unique()

# Gather species, make year numeric
dat2 <- dat %>%
  gather(species, abundance, Aulalt:Vagocc) %>%
  mutate(year = as.numeric(as.character(year)))

# Visualize the time series
ggplot(dat2, aes(x=year, y=abundance, color=species)) + geom_line() + facet_grid(X~Y)

# Calculate synchrony among species in each Point over time 
snail_synch <- synchrony(dat2, time.var = "year", 
                         species.var = "species",
                         abundance.var = "abundance",
                         metric = "Loreau",
                         replicate.var = "Point")

# Merge with X, Y coordinates
synch2 <- merge(key, snail_synch) %>%
  tbl_df()

# Plot synchrony on the X, Y coordinate plane
quilt.plot(synch2$X, synch2$Y, synch2$synchrony, nx=20, ny=20)

# Calculate synchrony on the aggregate site
dat3 <- dat2 %>%
  group_by(year, species) %>%
  summarize(abundance = sum(abundance))

synch_sitelevel <- synchrony(dat3, "year", "species", "abundance")
  

# Calculate synchrony over time (i.e., on consecutive, overlapping 10 year intervals)
# Also calculate total temporal richness, and total temporal abundance of all individuals
out <- as.data.frame(cbind(step = as.numeric(), Plot = as.character(),
                           synchrony = as.numeric(),
                           richness = as.numeric(),
                           totabund = as.numeric()))
yrs <- unique(dat2$year)
for (i in 1:15) {
  subber <- subset(dat2, (1990 + i) < year &  (1999 + i) > year)

  # Summarize by richness and abundance
  subber2 <- subber %>%
    group_by(Point, species) %>%
    summarize(tempabund = sum((abundance))) %>%
    tbl_df() %>%
    filter(tempabund > 0 ) %>%
    mutate(rich = 1) %>%
    group_by(Point) %>%
    summarize(richness = sum(rich), totabund= sum(tempabund))
    
  # Remove Points with one or fewer species over the time series
  subber3 <- merge(subber, subber2) %>%
    filter(richness > 1)
  
  # Calculate synchrony
  subsynch <- synchrony(subber3, "year", "species", "abundance", "Loreau", "Point" )
  subsynch2 <- merge(subsynch, subber2)
  subsynch2$step = i

  out <- rbind(out, subsynch2)
}

# Visualize synchrony over time
out2 <- merge(key, out)

# Synchrony in relation to abundance
ggplot(out2, aes(x=totabund, y=synchrony)) + geom_point() + geom_smooth()
ggplot(out2, aes(x=totabund, y=synchrony)) + geom_point() + facet_wrap(X~Y, scales= "free") + geom_smooth()

# Abundance in relation to richness
ggplot(out2, aes(x=totabund, y=richness)) + geom_point() + geom_smooth()
ggplot(out2, aes(x=totabund, y=richness)) + geom_point() + facet_wrap(X~Y, scales= "free") + geom_smooth()

# Synchrony in relation to richness
ggplot(out2, aes(x=richness, y=synchrony)) + geom_point() + geom_smooth()
ggplot(out2, aes(x=richness, y=synchrony)) + geom_point() + facet_wrap(X~Y, scales= "free") + geom_smooth()

# Synchrony in relation to timestep
ggplot(out2, aes(x=step, y=synchrony)) + geom_point()  + geom_smooth(aes(group = Point), se=F) + geom_smooth(color="black")
ggplot(out2, aes(x=step, y=synchrony)) + geom_point() + facet_grid(X~Y) + geom_smooth() + theme_bw()

## Nina and Lauren musings:
## does the mean of the synchrony equal the synchrony of the mean?
## does synchrony decay over space? obviously you can test for a population, but how should we test for a community?

##PDF to share with group
pdf("Snail_synchrony_preliminary.pdf")

# create a reverse-order X to have facet_grid match the quilt.plot
out2$X2 <- (-out2$X)
dat2$X2 <- (-dat2$X)

ggplot(dat2, aes(x=year, y=abundance, color=species)) + geom_line() + facet_grid(X2~Y) + theme_bw() + labs(x = "Year", y= "Abundance", 
title = "Snails abundance (count) over time
on a sampling grid (m) at Luquillo")

quilt.plot(synch2$Y, synch2$X, synch2$synchrony, nx=20, ny=20, main = "Synchrony (from 0-1) of snails
           on a sampling grid (m) at Luquillo")
 
ggplot(out2, aes(x=step, y=synchrony)) + geom_point() + facet_grid(X2~Y) + geom_smooth() + theme_bw() +
  labs(title = "Synchrony in moving 10-year windows
on the Luquillo grid", x = "Time step (year)", y = "Synchrony")

ggplot(out2, aes(x=step, y=totabund)) + geom_point() + facet_grid(X2~Y) + geom_smooth() + theme_bw() + 
  labs(title = "Total abundance in moving 10-year windows
on the Luquillo grid", x = "Time step (year)", y = "Total snail abundance")

dev.off()
