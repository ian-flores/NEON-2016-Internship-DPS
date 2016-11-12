library(dplyr) 
ItemId <- c(1,2,3)
StartDate <- c(ymd("2014-01-01"),ymd("2014-02-01"),ymd("2014-03-01"))
EndDate <- c(ymd("2014-02-15"),ymd("2014-02-07"),ymd("2014-03-03"))

jim <- data.frame(ItemId,StartDate,EndDate)

# One technique that's often useful especially in R, is to take your 
# iterator, and define it as a variable.  You can use that to implement
# a vectorised version of whatever you were thinking of doing.*/

ed <- data.frame(rng = seq(min(jim$StartDate), max(jim$EndDate), by = 'day'))
 
popo<- merge(jim, ed, all=TRUE) %>% 
  filter(rng >= StartDate, rng <= EndDate) %>%
  group_by(rng) %>% 
  summarise(n())

by_cyl <- group_by(mtcars, cyl)
summarise(by_cyl, mean(disp), mean(hp))
filter(by_cyl, disp == max(disp))
