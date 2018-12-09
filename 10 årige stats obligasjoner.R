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

Stats_obligasjoner_long_term <- read.csv("https://raw.githubusercontent.com/Johnrejor/lecture_3.0/master/Stats%20obligasjoner%20long%20term.csv")
Stats_obligasjoner_long_term <- as.data.frame(sapply(Stats_obligasjoner_long_term, function(x) gsub("\"", "", x)))

Stats_obligasjoner_long_term <- splitstackshape::cSplit(Stats_obligasjoner_long_term, names(Stats_obligasjoner_long_term)) 

#Removing uninteresting columns
Stats_obligasjoner_long_term<- Stats_obligasjoner_long_term[, -c(2:5)]

#Changing colnames
colnames(Stats_obligasjoner_long_term) <- c("Location", "Time", "Value")



Stats_obligasjoner_long_term$Location <- as.character(Stats_obligasjoner_long_term$Location)  
Stats_obligasjoner_long_term$Time <- as.Date(as.yearmon(Stats_obligasjoner_long_term$Time))

Irland2 <- filter(Stats_obligasjoner_long_term, Location == "IRL")

ggplot(data = Irland2) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))
Greece2 <- filter(Stats_obligasjoner_long_term, Location == "GRC")

ggplot(data = Greece2) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Norway2 <- filter(Stats_obligasjoner_long_term, Location == "NOR")

ggplot(data = Norway2) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Spain2 <- filter(Stats_obligasjoner_long_term, Location == "ESP")

ggplot(data = Spain2) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Italy2 <- filter(Stats_obligasjoner_long_term, Location == "ITA")

ggplot(data = Italy2) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Canada2 <- filter(Stats_obligasjoner_long_term, Location == "CAN")

ggplot(data = Canada2) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

USA2 <- filter(Stats_obligasjoner_long_term, Location == "USA")

ggplot(data = USA2) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))

Island2 <- filter(Stats_obligasjoner_long_term, Location == "ISL")


ggplot(data = Island2) + geom_line(mapping = aes(x = Time, y = Value)) + scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))







