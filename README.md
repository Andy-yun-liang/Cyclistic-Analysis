# Cyclistic Bike Share Analysis (June 2021 to May 2022)

An analysis of a fictional bikeshare company located in Chicago. The analysis is done using R and SQL with Google's BigQuery.

## Table of Contents
   - [Background](#background)
   - [Task](#task)
   - [Questions](#questions)




## Background
Cyclistic has been a successful bike-share program since launching in 2016. The initiative has grown to a fleet of 5,824 bicycles that are geotracked and locked intoa network of 692 stations throughout Chicago. The variety of bikes can be unlocked at any station and returned to any other station at anytime.

The data can be found here: 
https://divvy-tripdata.s3.amazonaws.com/index.html

## Task
To be able to derive actionable insights to convert the casual riders into annual members but in order to do so we need to understand the: 

1. differences in the group's biking patterns

2. factors that would encourage a conversion

Link to dashboard: 
https://public.tableau.com/app/profile/andy.liang1614/viz/CyclisticsBike-ShareProgram_16567299015020/Dashboard1

## Questions


### 1.Which months are the peak seasons for the different groups?

```r
sqlqry = dbGetQuery(connection,"SELECT member_casual,
                                EXTRACT(Month FROM start_date) as Month,
                                COUNT(*) as Count 
                                FROM `path_to_table_name` 
                                GROUP BY 1,2 
                                ORDER BY 2")


sqlqry %>% 
ggplot(aes(x=Month,y=Count,color = member_casual)) + 
geom_line() + geom_text(data = . %>% group_by(member_casual) %>% 
filter(Count == max(Count)), aes(label = Count,group=member_casual),
show.legend = FALSE,vjust=-0.15) +
labs(title = "Distribution of Users (June 2021 to May 2022)",x="Month",
y="Count of Members",color = "Groups") + 
scale_y_continuous(labels = comma) + scale_x_discrete(limits=month.abb) + theme_bw()
```
![q1](https://user-images.githubusercontent.com/73871814/193787876-795d9654-8172-4e46-b9d4-000b5b267910.PNG)



### 2.What's the average borrow duration for the different groups on a yearly, monthly basis?

All bikes are returned within a 24 hr interval however when the date do not match 1440 minutes are added (11:59pm -> 12am)
```r
#Yearly basis

qry2 = dbGetQuery(connection,
                              "SELECT member_casual,
                              ROUND(AVG(borrow_duration),2) as AverageBorrowDuration 
                              FROM(SELECT member_casual,
                              CASE
                                WHEN start_date != end_date THEN TIME_DIFF(end_time,start_time,MINUTE) + 1440
                                ELSE TIME_DIFF(end_time,start_time,MINUTE)
                              END AS borrow_duration
                              FROM `path_to_table_name`)
                              GROUP BY 1")

qry2 %>% mutate(member_casual = as.factor(member_casual)) %>% 
ggplot(aes(member_casual,AverageBorrowDuration,fill = member_casual)) + 
stat_summary(geom = "bar", fun = 'identity') + 
labs(title = "Average Borrow Duration in Minutes (June 2021 to May 2022)",
x="Groups",y="Minutes",fill = "Groups") + 
geom_text(aes(label = AverageBorrowDuration), vjust = -0.2) + theme_bw()
```
![q2a](https://user-images.githubusercontent.com/73871814/193787899-3f3f1f7c-2df0-4fd4-a68a-e73ee9150ff9.PNG)


```r
#Monthly basis

qry2b = dbGetQuery(connection,
                              "SELECT member_casual,month,
                              AVG(borrow_duration) as AvgBorrowDuration FROM(
                              SELECT member_casual,
                              EXTRACT(MONTH FROM start_date) as Month,
                                 CASE
                                   WHEN start_date != end_date THEN TIME_DIFF(end_time,start_time,MINUTE) + 1440
                                   ELSE TIME_DIFF(end_time,start_time,MINUTE)
                                 END AS borrow_duration
                              FROM `path_to_table_name`)
                              GROUP BY 1,2")

qry2b %>% mutate(member_casual = as.factor(member_casual)) %>% 
ggplot(aes(x=month,y=AvgBorrowDuration,fill = member_casual,width =.5)) + 
geom_bar(stat="identity",position="dodge") + scale_x_discrete(limits=month.abb) + 
labs(title = "Average Borrow Duration in Minutes by Month (June 2021 to May 2022)",x="Month",y="Minutes",fill = "Groups") + 
geom_text(aes(label = round(AvgBorrowDuration,1)), vjust = -0.3) + theme_bw()
```

![q2b](https://user-images.githubusercontent.com/73871814/193788063-3400a324-c96d-4f0e-9aee-4723abc25a9c.PNG)



### 3.Find the most frequent starting and ending stations and plot the results 
```r
qry3a = dbGetQuery(connection,"SELECT start_station_name,
                               COUNT(*) as Count
                               FROM `path_to_table_name` 
                               GROUP BY 1 
                               ORDER BY 2 
                               DESC LIMIT 10")

qry3a %>% ggplot(aes(x=reorder(start_station_name,(Count)),y=Count,fill=palette("Paired"))) + 
geom_bar(stat = 'identity') + labs(title = "Top 10 Starting Stations",x="Groups",y="Count",fill = "Groups") + 
coord_flip() + geom_text(aes(label = Count), hjust = 1.2,colour = "white",size = 3) + theme_bw() + theme(legend.position="none")
```
![q3a](https://user-images.githubusercontent.com/73871814/193788453-9bf07080-d2c8-4672-8cb5-81d06e511dc9.PNG)


```r
qry3b = dbGetQuery(connection,"SELECT end_station_name,
                               COUNT(*) as Count
                               FROM `path_to_table_name` 
                               GROUP BY 1 
                               ORDER BY 2 
                               DESC LIMIT 10")

qry3b %>% ggplot(aes(x=reorder(end_station_name,(Count)),y=Count,fill=brewer.pal("Spectral",n=10))) + 
geom_bar(stat = 'identity') + labs(title = "Top 10 Ending Stations",x="Groups",y="Count",fill = "Groups") + 
coord_flip() + geom_text(aes(label = Count), hjust = 1.2,colour = "white",size = 3) + theme_bw() + theme(legend.position="none")

```
![q3b](https://user-images.githubusercontent.com/73871814/193788478-6bf0e285-372e-4f48-96fc-040e4459edc2.PNG)


### 4.What are the top 10 routes taken by the different groups and what are the average borrow duration for each of these routes?
```r
#Casual Riders
qry4a = dbGetQuery(connection,"SELECT route, 
                               COUNT(*) as Freq 
                               FROM `path_to_table_name` 
                               WHERE member_casual = 'casual'
                               GROUP BY 1 
                               ORDER BY 2 DESC
                               LIMIT 10")

qry4a %>% ggplot(aes(x=reorder(route,(Freq)),y=Freq)) + geom_bar(stat='identity') 
+ coord_flip() + theme_bw() + theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 7)) + 
labs(title = "Top 10 Routes by Count (Casual Riders)",y="Count",x="Routes") +
geom_text(aes(label = Freq), hjust = 1.2,colour = "white",size = 3)
```
![q4a](https://user-images.githubusercontent.com/73871814/193788147-45dfc867-805b-4440-8277-2070d179083b.PNG)

```r
#Cyclistic Member
qry4b = dbGetQuery(connection,"SELECT route, 
                               COUNT(*) as Freq 
                               FROM `path_to_table_name` 
                               WHERE member_casual = 'member'
                               GROUP BY 1 
                               ORDER BY 2 DESC
                               LIMIT 10")

qry4b %>% ggplot(aes(x=reorder(route,(Freq)),y=Freq)) + 
geom_bar(stat='identity') + coord_flip() + theme_bw() + 
theme(axis.text.y = element_text(size = 6),axis.text.x = element_text(size = 7)) + 
labs(title = "Top 10 Routes by Count (Member Riders)",y="Count",x="Routes") +
geom_text(aes(label = Freq), hjust = 1.2,colour = "white",size = 3)
```
![q4b](https://user-images.githubusercontent.com/73871814/193788203-dc067fb9-1ec0-47b8-9755-806bbd7f9f3a.PNG)


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
                                    FROM `path_to_table_name` )
                                    GROUP BY 1,2,3")

#Cyclistic Members
qry5 %>% mutate(member_casual = as.factor(member_casual),DayOfWeek = as.factor(DayOfWeek)) %>% 
filter(member_casual == "member") %>% ggplot(aes(Hour,Count,fill=DayOfWeek,width =.5)) + 
geom_bar(stat="identity",position="dodge") + 
labs(title="Distribution of Most Frequent Cycling Hours (Members)",
x="Time (Hours) ",fill="Day of Week",y="Count of Cyclists")+ scale_x_continuous(labels = comma) + theme_bw()
```
![q5](https://user-images.githubusercontent.com/73871814/193788271-388b7b49-086d-4b57-be2f-128a2fdf0ea9.PNG)


```r
#Casual Riders
qry5 %>% mutate(member_casual = as.factor(member_casual),DayOfWeek = as.factor(DayOfWeek)) %>% 
filter(member_casual == "casual") %>% ggplot(aes(Hour,Count,fill=DayOfWeek,width =.5)) + 
geom_bar(stat="identity",position="dodge") + 
labs(title="Distribution of Most Frequent Cycling Hours (Casual Riders)",
x="Time (Hours) ",fill="Day of Week",y="Count of Cyclists") + scale_y_continuous(labels = comma) + theme_bw()


```
![q5b](https://user-images.githubusercontent.com/73871814/193788295-6a3d1a08-cef7-411a-aa63-ba0b37977045.PNG)


### 6.What's the distribution of the bike type by group?
```r
qry5 = dbGetQuery(connection,"SELECT member_casual,
                              rideable_type,
                              COUNT(*) as Count  
                              FROM `path_to_table_name` 
                              GROUP BY 1,2")

#Plotting the bar plot
qry5 %>% mutate(member_casual = as.factor(member_casual),rideable_type = as.factor(rideable_type),
rideable_type=recode(rideable_type,classic_bike = "Classic Bike", 
docked_bike = "Docked Bike",electric_bike = "Electric Bike")) %>% 
ggplot(aes(rideable_type,Count,fill=member_casual)) + geom_bar(stat='identity',position="dodge")+
labs(title="Distribution of Bike Types",x="Types of Bikes",fill="Groups") + 
scale_y_continuous(labels = comma)  + theme_bw()
```
![q6](https://user-images.githubusercontent.com/73871814/193788312-a43eaeb3-1727-47d6-8295-7a6e2dc79b16.PNG)


