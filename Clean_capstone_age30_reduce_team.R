str(athlete_events)
library(dplyr)
#check which columns have NA 
colnames(athlete_events)[colSums(is.na(athlete_events)) > 0]
#Clean up Medal fields with NA
athlete_events <- mutate(athlete_events, Medal = ifelse(is.na(Medal), "No Medal earned", Medal))
#create new column for merging Age & Sport
athlete_events$Age_Sport <- paste(athlete_events$Age,athlete_events$Sport)
athlete_events_filter <- filter(athlete_events, is.na(Age))
#Test the mean age for those with NA
athlete_events_clean_age <- mutate(athlete_events, Age = ifelse(is.na(Age) & Sport == "Gymnastics", mean(Age,na.rm = TRUE), Age))
athlete_events_clean_age <- mutate(athlete_events, Age = ifelse(is.na(Age) & Sport == "Boxing", mean(Age,na.rm = TRUE), Age))
#Filter to see only Age >= 35
athlete_events_35_yrs_up<- filter(athlete_events, Age >= 35)
#Remove Age rows with NA
athlete_events_Age <- filter(athlete_events, complete.cases(Age))
## test plots to see patterns
library(ggplot2)
ggplot(athlete_events_35_yrs_up, aes(x=Age, y=Sport)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
ggplot(athlete_events, aes(x=Age, y=Sport)) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4)     # 1/4 opacity
ggplot(athlete_events, aes(x=Age, y=Sport, color >= 35(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Team))     # 1/4 opacity
ggplot(athlete_events, aes(x=Sport, y=Team, color >= 35(Age))) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4, aes(color = Age))     # 1/4 opacity
ggplot(athlete_events_Age, aes(x=Team, y=Sport)) +
  geom_point(aes(color = Age))
ggplot(athlete_events_Age, aes(x=Year, y=Sport)) +
  geom_point()
ggplot(athlete_events_Age, aes(x=Year, y=Team)) +
  geom_point()
#testing different ways to filter info
plyr::ddply(athlete_events_Age, .(Year), summarize, num = length(Year), totalSport = sum(Sport))
sum(Sport[athlete_events_Age])
sum(athlete_events_Age, as.character('Sport'))
which( colnames(athlete_events_Age)=="Sport" )
count(athlete_events_Age, "Speed Skating")
#create column blending Sport and Year
athlete_events_Age$Sport_Year <- paste(athlete_events_Age$Sport,athlete_events_Age$Year)
#Look at count of Sport_Year
count(athlete_events_Age, "Sport_Year")
#Table of Sport_year
table(athlete_events_Age$Sport_Year)
#Table of Sport
table(athlete_events_Age$Sport)
athlete_events_Age_test <- filter(athlete_events_Age, Sport == c("Alpine Skiing", "Archery"))
#Filter out those sports with less than 3000 totals in Table
athlete_events_Age <- filter(athlete_events_Age, Sport == c("Alpine Skiing", " Athletics ", "Basketball", "Biathlon", "Boxing", "Canoeing", "Cross Country Skiing", "Cycling", "Equestriansm","Fencing", "Football", "Gymnastics", "Hockey", "Ice Hockey", "Rowing", "Sailing", "Shooting", "Speed Skating", "Swimming", "Wrestling"))
#Table of Teams
table(athlete_events_Age$Team)
#Filter out Teams with less than 20 in table
athlete_events_Age <- filter(athlete_events_Age, Team == c("Argentina", "Australia","Austria", "Belarus", "Belgium", "Brazil", "Bulgaria", "Canada", "Chile", "China", "Colombia", "Croatia", "Cuba", "Czech Republic", "Czechoslovakia", "Denmark", "East Germany", "Egypt", "Estonia", "Finland", "France", "Germany", "Great Britain", "Greece", "Hungary", "India", "Iran", "Ireland", "Isreal", "Italy", "Japan", "Kazakhstan", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Mongolia", "Netherlands", "New Zealand", "North Korea", "Norway", "Philippines", "Poland", "Portugal", "Puerto Rico", "Romania", "Russia", "Slovakia", "Slovenia", "South Africa", "South Korea", "Soviet Union", "Spain", "Sweden", "Switzerland", "Thailand", "Turkey", "United States", "Uruguay", "Venezuela", "West Germany", "Yugoslavia"))
# remove NA for Age
athlete_events_Age_Sport_Team <- filter(athlete_events, complete.cases(Age))
# remove Teams with less than 20 entries
athlete_events_Team <- filter(athlete_events_Age_Sport_Team, Team == c("Argentina","Australia","Austria","Belarus","Belgium","Brazil","Bulgaria","Canada","Chile","China","Colombia","Croatia","Cuba","Czech Republic","Czechoslovakia","Denmark","East Germany","Egypt","Estonia","Finland","France","Germany","Great Britain","Greece","Hungary","India","Iran","Ireland","Isreal","Italy","Japan","Kazakhstan","Latvia","Lithuania","Luxembourg","Mexico","Mongolia","Netherlands","New Zealand","North Korea","Norway","Philippines","Poland","Portugal","Puerto Rico","Romania","Russia","Slovakia","Slovenia","South Africa","South Korea","Soviet Union","Spain","Sweden","Switzerland","Thailand","Turkey","United States","Uruguay","Venezuela","West Germany","Yugoslavia"))
# remove Sports with less than 3000 entries
athlete_events_Sport <- filter(athlete_events_Age_Sport_Team, Sport == c("Alpine Skiing", " Athletics ", "Basketball", "Biathlon", "Boxing", "Canoeing", "Cross Country Skiing", "Cycling", "Equestriansm","Fencing", "Football", "Gymnastics", "Hockey", "Ice Hockey", "Rowing", "Sailing", "Shooting", "Speed Skating", "Swimming", "Wrestling"))
count(athlete_events_Age_Sport_Team, "Age" >=35)
count(athlete_events_Age_Sport_Team, "Age" <=35)
table(athlete_events_Age_Sport_Team$Age)
table(athlete_events_Age_Sport_Team$Age >=30)
table(athlete_events_Age_Sport_Team$Age >=30, athlete_events_Age_Sport_Team$Sport)
table(athlete_events_Age_Sport_Team$Age >=30, athlete_events_Age_Sport_Team$Season)
table(athlete_events_Age_Sport_Team$Age >=30, athlete_events_Age_Sport_Team$Sex)
table(athlete_events_Age_Sport_Team$Age >=30, athlete_events_Age_Sport_Team$Medal)
ggplot(athlete_events_Age_Sport_Team, aes(x=Sex, y=Age)) +
  geom_point()
athlete_events_Age_Sport_Team$Age_over_30 <- ifelse(athlete_events_Age_Sport_Team$Age >= 30,'Yes','No')
str(athlete_events_Age_Sport_Team)
athlete_events_Age_Sport_Team$Age_Num <- lapply(athlete_events_Age_Sport_Team [Age] as.numeric)
table(athlete_events_Age_Sport_Team$Age_over_30)
ggplot(athlete_events_Age_Sport_Team, aes(x=Medal, y=Sex)) +
  geom_point(aes(Age_over_30))
ggplot(athlete_events_Age_Sport_Team, aes(x=Sport, y=Medal)) +
  geom_point()
ggplot(athlete_events_Age_Sport_Team, aes(x=Age_over_30)) + geom_bar()
ggplot(athlete_events_Age_Sport_Team, aes(x=Medal)) + geom_bar()
ggplot(athlete_events_Age_Sport_Team, aes(x=Medal)) +geom_bar(aes(fill = Age_over_30))
ggplot(athlete_events_Age_Sport_Team, aes(x=Sport)) +geom_bar(aes(fill = Age_over_30))
ggplot(athlete_events_Age_Sport_Team, aes(x=Season)) +geom_bar(aes(fill = Age_over_30))
ggplot(athlete_events_Age_Sport_Team, aes(x=Sex)) +geom_bar(aes(fill = Age_over_30))
ggplot(athlete_events_Age_Sport_Team, aes(x=Year)) +geom_bar(aes(fill = Age_over_30))
ggplot(athlete_events_Age_Sport_Team, aes(x=Age, y=Medal)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
table(athlete_events_Age_Sport_Team$Age >=30, athlete_events_Age_Sport_Team$Year)
ggplot(athlete_events_Age_Sport_Team, aes(x=Age, y=Year)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
ggplot(athlete_events_Age_Sport_Team, aes(x=Year, y=Age)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
ranks <- order(athlete_events_Age_Sport_Team$Age)
plot(athlete_events_Age_Sport_Team$Age, athlete_events_Age_Sport_Team$Sport, 
     pch = 16, col = 2,
     xlab = "age",
     ylab = "Sport")
     abline(coef(lm_Age), lwd =2)
     lines(athlete_events_Age_Sport_Team$Age[ranks], athlete_events_Age_Sport_Team$Sport[ranks])
summary(athlete_events_Age_Sport_Team)     
model1 <- lm(Age ~ Sport + Team, data = athlete_events_Age_Sport_Team)
summary(model1)
cor (athlete_events_Age_Sport_Team$Age
athlete_events_50_years <- subset(athlete_events, Year >1966)
ggplot(athlete_events_50_years, aes(x=Age, y=Year)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)    # Add linear regression line 
ggplot(athlete_events_Age_Sport_Team, aes(Year, Age)) +
  geom_line()  
ggplot(athlete_events_Age_Sport_Team, aes(Year, Age, group=Age_over_30)) +
  geom_line(color = "blue")+
  geom_point() 
ggplot(athlete_events_Age_Sport_Team, aes(Year, Age)) +
  geom_line(aes(linetype=Age_over_30),color = "blue") 
ggplot(athlete_events_50_years, aes(Year, Age, color = 'color')) +
  geom_line(alpha = 0.4)
focus_sport <- c("Athletics", "Biathlon", "Bobsled", "Cross Country Skiing", "Cycling", "Equestrian", "Fencing", "Gymnastics", "Ski Jumping", "Swimming", "Speed Skating", "Tennis", "Volleyball")
athlete_events_Age_Sport_Team$Sport <- as.factor(athlete_events_Age_Sport_Team$Sport)
Reduce_Sprt_Tm <- filter(athlete_events_Age_Sport_Team, Sport == "Athletics" &
                         Sport == "Biathlon" & Sport == "Bobsled" & Sport == "Cross Country Skiing" & Sport == "Cycling" & 
                         Sport = "Equestrian" & Sport == "Fencing" & Sport =="Gymnastics" & Sport == "Ski Jumping" & Sport =="Swimming" & 
                         Sport == "Speed Skating" & Sport == "Tennis" & Sport == "Volleyball" & Sport =="Alpine Skiing" & 
                         Sport == "Boxing" & Sport == "Canoeing"& Sport == "Basketball" & Sport == "Football" & Sport == "Ice Hockey" & 
                         Sport =="Rowing" & Sport == "Shooting" & Sport =="Sailing" & Sport == "Wrestling" )
Reduce_Sprt_Tm <- filter(athlete_events_Age_Sport_Team, count(athlete_events_Age_Sport_Team$Sport) > 4000)
