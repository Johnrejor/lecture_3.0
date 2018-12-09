library(readr)
library(tidyverse)
library(mosaic) 
library(splitstackshape)
library(dplyr)
hushold_gjeld <- read_csv("https://raw.githubusercontent.com/Johnrejor/lecture_3.0/master/hushold%20gjeld.csv")

#Removing quotation marks, does not remove it from colnames
hushold_gjeld <- as.data.frame(sapply(hushold_gjeld, function(x) gsub("\"", "", x)))

#Split up colnames
hushold_gjeld <- splitstackshape::cSplit(hushold_gjeld, names(hushold_gjeld))


#Removing uninteresting columns
hushold_gjeld <- hushold_gjeld[, -c(2:5)]

#Changing colnames
colnames(hushold_gjeld) <- c("Location", "Time", "Value")

#Setting column Location as.character
hushold_gjeld$Location <- as.character(hushold_gjeld$Location)








GDP_percap1 <- read_csv("https://raw.githubusercontent.com/Johnrejor/lecture_3.0/master/GDP%20per%20capita%20from%20OECD.csv")

GDP_percap1 <- as.data.frame(sapply(GDP_percap1, function(x) gsub("\"", "", x)))

GDP_percap1 <- splitstackshape::cSplit(GDP_percap1, names(GDP_percap1)) 

#Removing uninteresting columns
GDP_percap1<- GDP_percap1[, -c(2:5, 8)]

#Changing colnames
colnames(GDP_percap1) <- c("Location", "Time", "Value")

#Setting column Location as.character
GDP_percap1$Location <- as.character(GDP_percap1$Location)

merged2$Time <- as.Date(as.yearmon(Stats_obligasjoner_long_term$Time))


merged2 <- left_join(hushold_gjeld, GDP_percap1, by=c("Location", "Time"))

norway <- filter(merged2, Location == "NOR")

ggplot(data = norway) + geom_line(mapping = aes(x = Time, y = Value.x)) 
ggplot(data = norway) + geom_line(mapping = aes(x = Time, y = Value.y))

Irland <-  filter(merged2, Location == "IRL")

ggplot(data = Irland) + geom_line(mapping = aes(x = Time, y = Value.x))
ggplot(data = Irland) + geom_line(mapping = aes(x = Time, y = Value.y))

USA <- filter(merged2, Location == "USA")

ggplot(data = USA) + geom_line(mapping = aes(x = Time, y = Value.x))

ggplot(data = USA) + geom_point(mapping = aes(x = Time, y = Value.y))

Greece <- filter(merged2, Location == "GRC")

ggplot(data = Greece) + geom_line(mapping = aes(x = Time, y = Value.x))
ggplot(data = Greece) + geom_line(mapping = aes(x = Time, y = Value.y))

Australia <- filter(merged2, Location == "AUS")

ggplot(data = Australia) + geom_line(mapping = aes(x = Time, y = Value.x))

ggplot(data = Australia) + geom_line(mapping = aes(x = Time, y = Value.y))
Spain <- filter(merged2, Location == "ESP")
ggplot(data = Spain) + geom_line(mapping = aes(x = Time, y = Value.x))
ggplot(data = Australia) + geom_line(mapping = aes(x = Time, y = Value.y))

Italy <- filter(merged2, Location == "ITA")

ggplot(data = Italy) + geom_line(mapping = aes(x = Time, y = Value.x))
ggplot(data = Italy) + geom_line(mapping = aes(x = Time, y = Value.y))




cor.test(merged2$Value.x, merged2$Value.y)
