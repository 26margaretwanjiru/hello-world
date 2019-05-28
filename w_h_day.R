#Loading the data

rm(list = ls())

w_h_day <- read.csv("C:/Users/MARGRET/Downloads/w_h_day.csv")
View(w_h_day)


#Data Exploration
attach(w_h_day)
str(w_h_day)
summary(w_h_day)
head(w_h_day)
tail(w_h_day, n=10)

#Renaming background characteristic

library(dplyr)

w_h_day <- rename(w_h_day, Mothers_first_post_natal_checkup = Background.characteristic)

#Data visualisations
library(ggplot2)
library(gganimate)
library(transformr)

attach(w_h_day)

#First

mother_first <- ggplot(w_h_day, aes(Mothers_first_post_natal_checkup, Percentage.distribution))+
  geom_boxplot(aes(fill = Background.characteristic))+
  ggtitle("Boxplot: Post natal checkup of mothers: 2008/9 - 2014")+
  coord_flip()

ggsave("mother_first.png")

#Second

age_mother <- ggplot(w_h_day, aes(Mother.s..age.at.birth, Percentage.distribution))+
  geom_boxplot(aes(fill = Mother.s..age.at.birth))+
  ggtitle("Boxplot: Mother's age at birth % distribution of post natal checkup")+
  coord_flip()

age_mother

#Third
#Bar plot

hist_percent <- ggplot(w_h_day, aes(x = Percentage.distribution))+
  geom_histogram(aes(fill = "red"))+
  ggtitle("Histogram post natal checkup percentage distribution ")

hist_percent

#Fourth

w_h_day1 <- w_h_day[order(Mothers_first_post_natal_checkup), ]

p<-ggplot(data = w_h_day1) + 
  geom_point(mapping = aes(x = Mothers_first_post_natal_checkup, y = Percentage.distribution, color = factor(Year))) + 
  facet_wrap(Mother.s..age.at.birth ~ .)+
  theme(axis.text.x = element_text(angle = 85, vjust = 0.6, color = "black"))+
  labs(title="Percentage distribution of mother's first post natal check up", x = "Mother's first post natal checkup", y = "Percentage distribution")
p

p + transition_time(Percentage.distribution) +
  labs(title = "Percentage.distribution: {frame_time}")


#Fifth
#Delete

five_today <- ggplot(w_h_day, aes(Year, Mothers_first_post_natal_checkup, group = Mother.s..age.at.birth, color = Mother.s..age.at.birth)) +
  geom_line(size = 0.5) +
  xlab("Year") +
  ylab("Mothers first post natal checkup") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.7, color = "black"))+
  transition_reveal(along = as.numeric(Year))+
  enter_grow()+
  ggtitle("Interactive line graph of GDP: 2003 - 2014")+
  theme(plot.title = element_text(face = "bold"))+
  theme_bw()
# gganimate code
five_today

animate(five_today, fps = 10, width = 750, height = 450)
anim_save("five_today.gif")  


#Sixth
#Pie chart

fourteen1 <- w_h_day %>%
  filter(Year == 2014)




bp<- ggplot(fourteen1, aes(x=" ", y=Percentage.distribution, group = Mothers_first_post_natal_checkup, fill= Mothers_first_post_natal_checkup))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap( ~ Mother.s..age.at.birth)
bp

rm(pie1)

pie1 = bp + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0((Percentage.distribution), "%")), position = position_stack(vjust = 0.1))
pie1

fourteen_t <- w_h_day %>%
  filter(Year == 2014)

avg_grp_fourteen_t <- fourteen_t %>%
  group_by(Mothers_first_post_natal_checkup) %>%
  summarise(avg_rate = mean(Percentage.distribution), count = n()) %>% 
  arrange(desc(avg_rate), count)

avg_grp_fourteen_t

eight <- w_h_day %>%
  filter(Year == 2008)

avg_grp_eight<- eight %>%
  group_by(Mothers_first_post_natal_checkup) %>%
  summarise(avg_rate = mean(Percentage.distribution), count = n()) %>% 
  arrange(desc(avg_rate), count)

avg_grp_eight

par(mfrow = c(1, 2))

#One
# Create a basic bar
pie33 = ggplot(avg_grp_fourteen_t, aes(x="", y=avg_rate, fill=Mothers_first_post_natal_checkup)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie3 = pie33 + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(avg_rate), "%")), position = position_stack(vjust = 0.5))
pie3

# Remove labels and add title
pie333 = pie3 + labs(x = NULL, y = NULL, fill = NULL, title = "Mother's first post natal care - 2014")
pie333

#Two
# Create a basic bar
pie44 = ggplot(avg_grp_eight, aes(x="", y=avg_rate, fill=Mothers_first_post_natal_checkup)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie4 = pie44 + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(avg_rate), "%")), position = position_stack(vjust = 0.5))
pie4

# Remove labels and add title
pie444 = pie4 + labs(x = NULL, y = NULL, fill = NULL, title = "Mother's first post natal care - 2008")
pie444


###Shiny
library(shiny)
ui<-fluidPage()
server <- function(input, output) {}
shinyApp(ui = ui, server = server)


