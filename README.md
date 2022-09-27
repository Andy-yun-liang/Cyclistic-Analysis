# Cyclistic Bike Share Analysis (June 2021 to May 2022)

An analysis of a fictional bikeshare company located in Chicago. The analysis is done using R and SQL with Google's BigQuery.

## Table of Contents
   - [Background](#background)
   - [Task](#task)
   - [Analysis](#analysis)




## Background
Cyclistic has been a successful bike-share program since launching in 2016. The initiative has grown to a fleet of 5,824 bicycles that are geotracked and locked intoa network of 692 stations throughout Chicago. The variety of bikes can be unlocked at any station and returned to any other station at anytime.

## Task
To be able to derive actionable insights to convert the casual riders into annual members but in order to do so we need to understand the: 

1. differences in the group's biking patterns

2. factors that would encourage a conversion

## Analysis


Questions:

### 1.Which months are the peak seasons for the different groups?

```r
sqlqry = dbGetQuery(connection,"SELECT member_casual,
                                EXTRACT(Month FROM start_date) as Month,
                                COUNT(*) as Count 
                                FROM `cyclisticbikeproject.BikeData.complete_tbl` 
                                GROUP BY 1,2 
                                ORDER BY 2")


ggplot(data = sqlqry,aes(x=Month,y=Count,color = member_casual)) + geom_line() + 
labs(title = "Distribution of Users (June 2021 to May 2022)",x="Month",y="Count of Members",color = "Groups") + 
scale_y_continuous(labels = comma) + scale_x_discrete(limits=month.abb) + theme_bw()
```

### 2.What's the average borrow duration for the different groups on a yearly, monthly basis?

All bikes are returned within a 24 hr interval however when the date do not match 1440 minutes are added (11:59pm -> 12am)
```r
#Yearly basis

qry2 = dbGetQuery(connection,"SELECT member_casual,
                              ROUND(AVG(borrow_duration),2) as AverageBorrowDuration 
                              FROM(SELECT member_casual,
                              CASE
                                 WHEN start_date != end_date THEN TIME_DIFF(end_time,start_time,MINUTE) + 1440
                                 ELSE TIME_DIFF(end_time,start_time,MINUTE)
                              END AS borrow_duration
                              FROM `cyclisticbikeproject.BikeData.complete_tbl`)
                              GROUP BY 1")

qry2 %>% mutate(member_casual = as.factor(member_casual)) %>% 
ggplot(aes(member_casual,AverageBorrowDuration,fill = member_casual)) + 
stat_summary(geom = "bar", fun = 'identity') + 
labs(title = "Average Borrow Duration in Minutes (June 2021 to May 2022)",x="Groups",y="Minutes",fill = "Groups") + 
geom_text(aes(label = AverageBorrowDuration), vjust = -0.2) + theme_bw()
```


```r
#Monthly basis

qry3 = dbGetQuery(connection,"SELECT member_casual,month,
                              AVG(borrow_duration) as AvgBorrowDuration FROM(
                              SELECT member_casual,
                              EXTRACT(MONTH FROM start_date) as Month,
                                 CASE
                                    WHEN start_date != end_date THEN TIME_DIFF(end_time,start_time,MINUTE) + 1440
                                    ELSE TIME_DIFF(end_time,start_time,MINUTE)
                                 END AS borrow_duration
                              FROM `cyclisticbikeproject.BikeData.complete_tbl`)
                              GROUP BY 1,2")

qry3 %>% mutate(member_casual = as.factor(member_casual)) %>% 
ggplot(aes(x=month,y=AvgBorrowDuration,fill = member_casual,width =.5)) + 
geom_bar(stat="identity",position="dodge") + scale_x_discrete(limits=month.abb) + 
labs(title = "Average Borrow Duration in Minutes by Month (June 2021 to May 2022)",x="Month",y="Minutes",fill = "Groups") + 
geom_text(aes(label = round(AvgBorrowDuration,1)), vjust = -0.3) + theme_bw()
```




### 3.Find the most frequent starting and ending stations and plot the results 

### 4.What are the top 10 routes taken by the different groups and what are the average borrow duration for each of these routes?
```r
#Casual Riders
qry3a = dbGetQuery(connection,"SELECT route, 
                               COUNT(*) as Freq 
                               FROM `cyclisticbikeproject.BikeData.complete_tbl` 
                               WHERE member_casual = 'casual'
                               GROUP BY 1 
                               ORDER BY 2 DESC
                               LIMIT 10")

qry3a %>% ggplot(aes(x=reorder(route,(Freq)),y=Freq)) + geom_bar(stat='identity') 
+ coord_flip() + theme_bw() + theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 7)) + 
labs(title = "Top 10 Routes by Count (Casual Riders)",y="Count",x="Routes") +
geom_text(aes(label = Freq), hjust = 1.2,colour = "white",size = 3)
```

```r
#Cyclistic Member
qry3b = dbGetQuery(connection,"SELECT route, 
                               COUNT(*) as Freq 
                               FROM `cyclisticbikeproject.BikeData.complete_tbl` 
                               WHERE member_casual = 'member'
                               GROUP BY 1 
                               ORDER BY 2 DESC
                               LIMIT 10")

qry3b %>% ggplot(aes(x=reorder(route,(Freq)),y=Freq)) + 
geom_bar(stat='identity') + coord_flip() + theme_bw() + 
theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 7)) + 
labs(title = "Top 10 Routes by Count (Member Riders)",y="Count",x="Routes") +
geom_text(aes(label = Freq), hjust = 1.2,colour = "white",size = 3)
```

### 5.When are the peak hours on a daily basis?
```r
qry5 = dbGetQuery(connection," SELECT member_casual,
                               CASE 
                                 WHEN DayOfWeek = 1 OR DayOfWeek =7 THEN 'Weekend'
                                 ELSE 'Weekday'
                               END AS DayOfWeek,
                               Hour, COUNT(*) AS Count 
                               FROM(SELECT member_casual, 
                                    EXTRACT(DAYOFWEEK FROM start_date) as DayOfWeek,
                                    EXTRACT(HOUR FROM start_time) AS Hour 
                                    FROM `cyclisticbikeproject.BikeData.complete_tbl` )
                                    GROUP BY 1,2,3")

#Cyclistic Members
qry5 %>% mutate(member_casual = as.factor(member_casual),DayOfWeek = as.factor(DayOfWeek)) %>% 
filter(member_casual == "member") %>% ggplot(aes(Hour,Count,fill=DayOfWeek,width =.5)) + 
geom_bar(stat="identity",position="dodge") + 
labs(title="Distribution of Most Frequent Cycling Hours (Members)",
x="Time (Hours) ",fill="Day of Week",y="Count of Cyclists")+ scale_x_continuous(labels = comma) + theme_bw()

#Casual Riders
qry5 %>% mutate(member_casual = as.factor(member_casual),DayOfWeek = as.factor(DayOfWeek)) %>% 
filter(member_casual == "casual") %>% ggplot(aes(Hour,Count,fill=DayOfWeek,width =.5)) + 
geom_bar(stat="identity",position="dodge") + 
labs(title="Distribution of Most Frequent Cycling Hours (Casual Riders)",
x="Time (Hours) ",fill="Day of Week",y="Count of Cyclists") + scale_y_continuous(labels = comma) + theme_bw()


```


### 6.What's the distribution of the bike type by group?
```r
qry5 = dbGetQuery(connection,"SELECT member_casual,
                              rideable_type,
                              COUNT(*) as Count  
                              FROM `cyclisticbikeproject.BikeData.complete_tbl` 
                              GROUP BY 1,2")

#Plotting the bar plot
qry5 %>% mutate(member_casual = as.factor(member_casual),rideable_type = as.factor(rideable_type),
rideable_type=recode(rideable_type,classic_bike = "Classic Bike", 
docked_bike = "Docked Bike",electric_bike = "Electric Bike")) %>% 
ggplot(aes(rideable_type,Count,fill=member_casual)) + geom_bar(stat='identity',position="dodge")+
labs(title="Distribution of Bike Types",x="Types of Bikes",fill="Groups") + 
scale_y_continuous(labels = comma)  + theme_bw()
```


