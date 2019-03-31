library(ggplot2)
library(dplyr)
library(lubridate)

###### LOAD DATA #######
df <- read.csv("data/311_Service_Requests_from_2010_to_Present.csv", header=TRUE)
#keep only HPD entries between 1/1/2018 & 12/31/2018 and cleanup factor levels 
df<- df %>% subset(Agency=="HPD" & mdy_hms(Created.Date)>=mdy("1/1/2018") & mdy_hms(Created.Date)<=mdy("12/31/2018")) %>% droplevels()


###### TOTAL HPD COMPLAINTS #######
#total HPD complaints
(ttl_comps <- length(unique(df$Unique.Key)))


###### COMPLAINTS PER BOROUGH #######
#create subset dataframe with type and borough columns, then summarize
comp_types <- df %>% subset(select=c(Complaint.Type,Borough)) %>% 
    group_by(Borough) %>% 
    summarize(Complaints = length(Complaint.Type)) %>% arrange(desc(Complaints))
knitr::kable(comp_types)


###### COMPLAINT DURATION #######
comp_diff <- mdy_hms(df$Closed.Date)-mdy_hms(df$Created.Date) #calculate time deltas
comp_diff <- round(as.numeric(comp_diff, units="days"),2) #convert to days
df$comp_diff <- comp_diff #add column to main dataframe
summary(df$comp_diff) #generate summary statistics

#create boxplot:
ggplot(df, aes(x=Agency, y=comp_diff)) + geom_boxplot() + 
    ylim(0,35) + ylab("Days") + 
    ggtitle("Request Duration")+
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip()


###### MONTHLY DISTRIBUTION OF COMPLAINTS #######
#total complaints in 2018
date_range_requests <- df %>% subset(select=c(Unique.Key,Created.Date,Complaint.Type)) %>%
    mutate(Month=month(mdy_hms(Created.Date))) %>%
    group_by(Month) %>% summarize(Requests=n()) %>% arrange(desc(Month))
date_range_requests$Month <- as.factor(month.abb[date_range_requests$Month]) #convert month num to name and factor
date_range_requests$Month <- factor(date_range_requests$Month, levels = date_range_requests$Month) #lock in order for ggplot
ggplot(date_range_requests, aes(x=Month, y= Requests)) + 
    xlab("Month") +
    geom_col() + 
    coord_flip() +
    labs(title="311 HPD Complaints in 2018")+
    theme(plot.title = element_text(hjust = 0.4))


###### COMPLAINT TYPE DISTRIBUTION #######
#comp_types <- prop.table(table(df$Complaint.Type))
comp_types <- df %>% subset(select=c(Complaint.Type,Borough)) %>% 
    group_by(Complaint.Type, Borough) %>% 
    summarize(Complaints = length(Complaint.Type))

#get rid of "agency" type outlier
comp_types <- comp_types[comp_types$Complaint.Type!="AGENCY" & 
                             comp_types$Complaint.Type!="HPD Literature Request",]

ggplot(comp_types, aes(x=reorder(Complaint.Type, Complaints), y=Complaints, fill=Borough)) + 
    xlab("Category") +
    geom_bar(stat="identity") + 
    coord_flip() +
    ggtitle("HPD Complaints by Type")

###### 311 ACTIVITY HEATMAP #######
#complaints breakdown by weekday and hour, by agency between date range
agencies <- c(as.character(unique(df$Agency)),"ALL") #get list of agencies for error handling

heatmap <- function(dept,start,end){
    startDate <- mdy(start)
    endDate <- mdy(end)
    
    #Analyze overall data or limited to a specific agency
    if(dept=="ALL"){
        times <- df[,c("Unique.Key","Created.Date")] %>% #keep just complaint ID and date
            mutate(datetime=mdy_hms(Created.Date)) %>% #convert string date to date object
            subset(datetime>=startDate & datetime<=endDate) #keep just those complaints within selected date range
    } else {
        times <- df[df$Agency==dept,c("Unique.Key","Created.Date")] %>% #keep just complaint ID and date
            mutate(datetime=mdy_hms(Created.Date)) %>% #convert string date to date object
            subset(datetime>=startDate & datetime<=endDate) #keep just those complaints within selected date range
    }
    
    times$day <- weekdays(as.Date(times$datetime)) #get the day of the week
    times$hour <- hour(times$datetime) #get the hour of the day
    dayHour <- plyr::ddply(times, c( "hour", "day"), 
                           summarise,N=length(datetime)) #create new summary dataframe with counts by day and hour
    dayHour$day <- factor(dayHour$day, 
                          levels = c("Sunday","Saturday","Friday","Thursday","Wednesday","Tuesday","Monday")) #ordering for graph
    
    col1 = "#C7D8B5"
    col2 = "#B22222"
    htitle=paste(dept," 311 Complaints by Day between ",start, " - ", end)
    ggplot(dayHour, aes(hour, day)) +
        geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
        scale_fill_gradient(low = col1, high = col2) +
        guides(fill=guide_legend(title="Total Incidents")) +
        theme_bw() + theme_minimal() +
        labs(title = htitle,
             x = "Incidents By Hour", y = "Day of Week") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

#call heatmap function with desired parameters
heatmap("HPD","1/1/2018","12/31/2018")