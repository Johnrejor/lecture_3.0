library(readr)
Stats_obligasjoner_long_term <- read_csv("R/Stats obligasjoner long term.csv")
library(tidyverse)
library(mosaic) 
library(splitstackshape)
library(dplyr)
install.packages("zoo")
library(zoo)
install.packages("scales")
library(scales)


Stats_obligasjoner_long_term <- as.data.frame(sapply(Stats_obligasjoner_long_term, function(x) gsub("\"", "", x)))

Stats_obligasjoner_long_term <- splitstackshape::cSplit(Stats_obligasjoner_long_term, names(Stats_obligasjoner_long_term)) 

names(Stats_obligasjoner_long_term)[1] <- paste("Location") 
names(Stats_obligasjoner_long_term)[2] <- paste("Indicator")
names(Stats_obligasjoner_long_term)[3] <- paste("Subject")
names(Stats_obligasjoner_long_term)[4] <- paste("Measure")
names(Stats_obligasjoner_long_term)[5] <- paste("Freq")
names(Stats_obligasjoner_long_term)[6] <- paste("Time")                                                                     
names(Stats_obligasjoner_long_term)[7] <- paste("Value")
names(Stats_obligasjoner_long_term)[8] <- paste("flag codes")

Stats_obligasjoner_long_term$Indicator = NULL
Stats_obligasjoner_long_term$Freq = NULL 
Stats_obligasjoner_long_term$Subject = NULL 
Stats_obligasjoner_long_term$Measure = NULL 
Stats_obligasjoner_long_term$Measure = NULL 
Stats_obligasjoner_long_term$Location <- as.character(Stats_obligasjoner_long_term$Location)  
Stats_obligasjoner_long_term$Time <- as.Date(as.yearmon(Stats_obligasjoner_long_term$Time))

Irland2 <- filter(Stats_obligasjoner_long_term, Location == "IRL")

ggplot(data = Irland) + geom_point(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))
Greece2 <- filter(Stats_obligasjoner_long_term, Location == "GRC")

ggplot(data = Greece) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Norway2 <- filter(Stats_obligasjoner_long_term, Location == "NOR")

ggplot(data = Norway) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Spain2 <- filter(Stats_obligasjoner_long_term, Location == "ESP")

ggplot(data = Spain) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Italy2 <- filter(Stats_obligasjoner_long_term, Location == "ITA")

ggplot(data = Italy) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Canada2 <- filter(Stats_obligasjoner_long_term, Location == "CAN")

ggplot(data = Canada) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

USA2 <- filter(Stats_obligasjoner_long_term, Location == "USA")

ggplot(data = USA) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Island2 <- filter(Stats_obligasjoner_long_term, Location == "ISL")


ggplot(data = Island) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))







