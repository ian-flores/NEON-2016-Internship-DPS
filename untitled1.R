cbn <- table(Ocassion)

Ocassion <- data$date
Session <- data$eventID
olo <- table(Session)

filter()
group_by(data, eventID)%>% 
  summarise(n())

data %>% group_by(eventID, date) %>% tally()
data %>% group_by(eventID, date) %>% summarise()
jugo$eventID  
jugo
?factor

