---
  title: "R Notebook"
output: html_notebook
---

  

#{r}
# Meetup Events by Big Data Groups in Western States

# Load packages

library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)

# Import data set
#setwd("~/Documents/git_repositories/meetup-insights/")
Meetup_Past_Events_Summary <- read_excel("Meetup_Past_Events.xlsx")

# Remove outliers
Meetup_Past_Events <- filter(Meetup_Past_Events_Summary, RSVP < 5000)

# Total RSVP for events by date
ggplot(Meetup_Past_Events, aes(x = Date)) +
  geom_histogram()

# Trend in events RSVP by State 
p5 <- ggplot(Meetup_Past_Events, aes(x = Date, y = RSVP))
p5 + geom_line(aes(color = State)) +
  labs(title="title")

# Same with separate graph per state
(p5 <- p5 + geom_line() +
    facet_wrap(~State, ncol = 4))

# Agreegate data and get mean RSVP per event
Meetup_Past_Events.sum <- 
  aggregate(Meetup_Past_Events["RSVP"], 
            Meetup_Past_Events["State"], 
            FUN=mean)
rbind(Meetup_Past_Events.sum)
ggplot(Meetup_Past_Events.sum, aes(x=State, y=RSVP)) + 
  geom_bar(stat="identity")

# narrow data set to CA,CO,UT, compare RSVP to events by state 
ggplot(subset(Meetup_Past_Events, 
              State %in% c("CA","CO","UT")),
       aes(x=Date,
           y=RSVP,
           color=State))+
  geom_point()

# Trend on Utah Events
UT_events = filter(Meetup_Past_Events, State == "UT")

pc1 <- ggplot(UT_events, aes(x = Date, y = RSVP, color = State))
#pc1 + geom_point()
(pc2 <- pc1 +
    geom_smooth(aes(group = 1),
                method = "lm",
                formula = y ~ log(x),
                se = FALSE,
                color = "green")) +
  geom_point() +
  #geom_text(aes(label=ifelse(RSVP>175,as.character(Event),'')),hjust=0, vjust=0)+
  #geom_text_repel(aes(label = rownames())) +
  labs(title="Meetup Trends by Big Data Groups in Utah")

Meetup_Past_Events %>%
  mutate(is_utah = factor(ifelse(State == "UT", 1, 0))) %>%
  ggplot(aes(Date, RSVP, color = State)) +
  #geom_point(alpha = .01) +
  geom_line(aes(alpha = is_utah), stat = "smooth", method = "lm", se = F) +
  coord_cartesian(ylim = c(0,50)) +
  scale_alpha_discrete(range = c(0.5, 1)) +
  scale_color_brewer(palette = "Accent") +
  labs(title = "Utah Meetups are growing faster than other states",
       y = "Number of RSVPs for each Meetup") +
  theme_dark()
 





