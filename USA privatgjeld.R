library(readr)
library(tidyverse)
library(mosaic) 
library(splitstackshape)
library(dplyr)
hushold_gjeld <- read_csv("https://raw.githubusercontent.com/Derraz91/Data-prosjekt/master/Unemployment2000-2018.csv")

#Removing quotation marks, does not remove it from colnames
Unemp_OECD <- as.data.frame(sapply(Unemp_OECD, function(x) gsub("\"", "", x)))

#Split up colnames
Unemp_OECD <- splitstackshape::cSplit(Unemp_OECD, names(Unemp_OECD))


#Removing uninteresting columns
Unemp_OECD <- Unemp_OECD[, -c(2:5, 8)]

#Changing colnames
colnames(Unemp_OECD) <- c("Location", "Time", "Value")

#Setting column Location as.character
Unemp_OECD$Location <- as.character(Unemp_OECD$Location)

#Changing Qartile to month and day
Unemp_OECD$Time <- as.Date(as.yearqtr(Unemp_OECD$Time, format = "%Y-Q%q"))



hushold_gjeld <- read_csv("R/hushold gjeld.csv")

hushold_gjeld <- as.data.frame(sapply(hushold_gjeld, function(x) gsub("\"", "", x)))

hushold_gjeld <- splitstackshape::cSplit(hushold_gjeld, names(hushold_gjeld)) 

names(hushold_gjeld)[1] <- paste("Location") 
names(hushold_gjeld)[2] <- paste("Indicator")
names(hushold_gjeld)[3] <- paste("Subject")
names(hushold_gjeld)[4] <- paste("Measure")
names(hushold_gjeld)[5] <- paste("Freq")
names(hushold_gjeld)[6] <- paste("Time")                                                                     
names(hushold_gjeld)[7] <- paste("Value")
names(hushold_gjeld)[8] <- paste("flag codes")

hushold_gjeld$Indicator = NULL
hushold_gjeld$Freq = NULL 
hushold_gjeld$Subject = NULL 
hushold_gjeld$Measure = NULL 
hushold_gjeld$Measure = NULL 
hushold_gjeld$Location <- as.character(hushold_gjeld$Location)
australia <- filter(hushold_gjeld, Location == "AUS")


ggplot(data = australia) + geom_point(mapping = aes(x = Time, y = Value))



GDP_percap1 <- read_csv("R/BNP.csv")

GDP_percap1 <- as.data.frame(sapply(GDP_percap1, function(x) gsub("\"", "", x)))

GDP_percap1 <- splitstackshape::cSplit(GDP_percap1, names(GDP_percap1)) 

names(GDP_percap1)[1] <- paste("Location") 
names(GDP_percap1)[2] <- paste("Indicator")
names(GDP_percap1)[3] <- paste("Subject")
names(GDP_percap1)[4] <- paste("Measure")
names(GDP_percap1)[5] <- paste("Freq")
names(GDP_percap1)[6] <- paste("Time")                                                                     
names(GDP_percap1)[7] <- paste("Value")
names(GDP_percap1)[8] <- paste("fC")

GDP_percap1$Indicator = NULL
GDP_percap1$Freq = NULL 
GDP_percap1$Subject = NULL 
GDP_percap1$Measure = NULL 
GDP_percap1$fC = NULL 

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
