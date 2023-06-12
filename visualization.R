
library(tidyverse)
guns <- read_csv("C:/Users/user/Desktop/AD/guns.csv")
incidents <- read_csv("C:/Users/user/Desktop/AD/incidents.csv")
participants <- read_csv("C:/Users/user/Desktop/AD/participants.csv")

fite <- filter(participants, participant_type == "Victim"& killed )
result <- group_by(fite, participant_age_group) %>%
  summarize(count = n())

library(ggplot2)

x <- ggplot(result, aes(x=participant_age_group, y=count, fill=participant_age_group)) + 
  geom_bar(stat='identity') + labs(x = "People", y = "ln(Count)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 7))


#0 гістограми колонок
hist(participants$participant_age)
hist(incidents$n_killed)
hist(incidents$n_injured)
hist(incidents$n_guns_involved)

#учасники інцидентів: кількість жінок\чоловіків
a <- table(participants$participant_gender)
library(ggplot2)

tmp <- data.frame(
  name=c("Female", "Male") ,  
  value=c(unlist(a[[1]]), unlist(a[[2]]))
)

p <- ggplot(tmp, aes(x=name,y=value)) + 
  geom_bar(stat = "identity")

#cередня кількість порахених і вбитих
incidents%>%
  summarize(meanInjured = mean(n_injured), meanKilled = mean(n_killed))



#1.1 підрахунок кількості інцидентів в різних штатах

z <- table(incidents$state)

x <- as.data.frame(z)

library(maps)
library(mapproj)
library(tidyverse)

states <- map_data('state')
arrests <- x

names(arrests) <- tolower(names(arrests))
arrests$region <-tolower(x[['Var1']])

arrests.geo <- merge(states, arrests, sort = FALSE, by = "region")
arrests.geo <- arrests.geo[order(arrests.geo$order), ]

k <- ggplot(arrests.geo, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = freq)) + 
  coord_map()
k

#1.2 підрахунок кількості інцидентів на особу в різних штатів
Populations <- read_csv("C:/Users/user/Desktop/AD/population_ratio.csv")


library(maps)
library(mapproj)
library(tidyverse)

states <- map_data('state')
arrests <- Populations

names(arrests) <- tolower(names(arrests))
arrests$region <-tolower(Populations[['state']])

arrests.geo <- merge(states, arrests, sort = FALSE, by = "region")
arrests.geo <- arrests.geo[order(arrests.geo$order), ]

k <- ggplot(arrests.geo, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = per_capita)) + 
  coord_map()
k



#2 підрахунок зброї якої використовувалась
z2 <- table(guns$gun_type)
x2 <- as.data.frame(z2)
k2 <- ggplot(x2, aes(x = factor(Var1), y = log(Freq))) +
  geom_point() +
  labs(x = "Вид зброї", y = "ln(Кількість)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 7))


#3 підрахунок кількості якому штаті найбільше інцидентів та потім в якому місяці скільки інцидентів

incidents_by_state <- incidents %>%
  group_by(state) %>%
  summarize(total_incidents = n()) %>%
  arrange(desc(total_incidents))
state_max_incidents <- incidents_by_state$state[1]

incidents_by_state_max <- incidents %>%
  mutate(month = lubridate::month(date)) %>%
  filter(state == state_max_incidents) %>%
  group_by(state, month) %>%
  summarize(total_incidents = n()) %>%
  arrange(desc(total_incidents))

month_max_incidents <- incidents_by_state_max$month[1]
incidents_by_state_max
cat("Штат з найбільшою кількістю інцидентів: ", state_max_incidents, "\n")
cat("Місяць з найбільшою кількістю інцидентів у штаті ", state_max_incidents, ": ", month_max_incidents, "\n")


incidents_by_state <- aggregate(incidents$incident_id, by = list(incidents$state), FUN = length)
names(incidents_by_state) <- c("state", "count")


#4 підрахунок кількості якому штаті найнайменьше інцидентів та потім в якому місяці скільки інцидентів
# Знаходимо штат з мінімальною кількістю інцидентів
state_with_min_incidents <- incidents_by_state[which.min(incidents_by_state$count), "state"]

# Фільтруємо дані за штатом з мінімальною кількістю інцидентів
incidents_by_state <- incidents[incidents$state == state_with_min_incidents,]

# Додаємо стовпець з місяцем
incidents_by_state$month <- format(as.Date(incidents_by_state$date), "%m")

# Групуємо дані за місяцем та рахуємо кількість інцидентів в кожному місяці
incidents_by_state_min <- aggregate(incidents_by_state$incident_id, by = list(incidents_by_state$month), FUN = length)
names(incidents_by_state_min) <- c("month", "count")



#5 кількість інцидентів в різні вікові групи
z5 <- table(participants$participant_age_group)
x5 <- as.data.frame(z5)

library(ggplot2)

ggplot(x5, aes(x=Var1, y=log(Freq), fill=Var1)) + 
  geom_bar(stat='identity') + labs(x = "People", y = "ln(Count)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 7))

#6 штати та скільки там інцидентів по віковій групі

merged <- merge(participants, incidents, by = "incident_id")

library(dplyr)
result <- merged %>%
  group_by(state, participant_age_group) %>%
  summarize(count = n()) %>%
  arrange(state, participant_age_group)

result

library(tidyverse)
library(hrbrthemes)
library(babynames)
library(viridis)


result %>%
  ggplot( aes(x=participant_age_group, y=log(count), group=state, fill=state)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("States and how many incidents there are by age group") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(size = 1),
    plot.title = element_text(size=8)
  ) +
  facet_wrap(~state, scale="free_y")


#7 кількість убитих у кожному штаті
z <- table(incidents$state)

deaths_by_state <- aggregate(n_killed ~ state, data = incidents, sum)

data <- as.data.frame(matrix( sample( deaths_by_state[[2]] , 51 ,
                                      replace=T) ,
                              ncol=51))

colnames(data) <- c(deaths_by_state[[1]])
  
library(fmsb)
data <- rbind(rep(5562,51) , rep(0,51) , data)
radarchart(data,  axistype=1 , 
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 
)

#7.2 кількість поранених у кожному штаті
z <- table(incidents$state)

deaths_by_state <- aggregate(n_injured ~ state, data = incidents, sum)

data <- as.data.frame(matrix( sample( deaths_by_state[[2]] , 51 ,
                                      replace=T) ,
                              ncol=51))

colnames(data) <- c(deaths_by_state[[1]])




library(fmsb)
data <- rbind(rep(13514,51) , rep(0,51) , data)
radarchart(data,  axistype=1 , 
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.8 
)


#8 кількість інцидентів кожного місяця кожного року

library(ggplot2)

incidents$date <- as.Date(incidents$date)
incidents$month <- format(incidents$date, "%m")
incidents$year <- format(incidents$date, "%Y")

injured_by_month_year <- aggregate(incidents$n_injured, 
                                   by = list(year = incidents$year, month = incidents$month), 
                                   FUN = sum)


injured_by_month_year %>% 
  ggplot(aes(x =month, y = x, group = year, color = year)) +
  geom_point() +
  geom_line() +
  ggtitle("Кількість інцидентів кожного місяця кожного року") +
  labs(x = "Місяць", y = "Кількість") +
  facet_wrap(~year)
