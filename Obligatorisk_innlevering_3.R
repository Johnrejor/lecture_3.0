library(readr)
install.packages("httr")
library(httr)

install.packages("rjstat")
library(rjstat)
library(tidyverse)
library(dplyr)

set1json <- "http://data.ssb.no/api/v0/dataset/95274.json?lang=no"

dftemp <- GET(set1json)
 
dfjson <- fromJSONstat(content(dftemp, "text"))

df1 <- dfjson[[1]]

df1 <- separate(df1, måned, c("år", "måned"), sep = "M")

set2json <- "http://data.ssb.no/api/v0/dataset/95276.json?lang=no"
dftemp2 <- GET(set2json)
dfjson2 <- fromJSONstat(content(dftemp2, "text"))
df2 <- dfjson2 [[1]]
#Her separer vi kolonen måned, så vi får 2 nye, år og måned.
df2 <- separate(df2, måned, c("år", "måned"), sep = "M")
# Her merger vi datasettene
dfmerged <- left_join(df1, df2, by = c("år", "måned", "statistikkvariabel"))
# Her filtrer vi får å få vekk 0 verdier.
dfmerged <- dfmerged %>%
  filter(!(value.x <= 0))
#her filtrer vi får å få et dataset bare med rompris, får så å regne gjennomsnittet.
rompris <- dfmerged %>%
  filter(statistikkvariabel == "Pris per rom (kr)")
 mean(rompris$value.x)                
mean(rompris$value.y)
#Her grupperer vi med fylke, for å finne gjennomsnittlig rompris per fylke
 gjennomsnitt_x <- rompris %>%
  group_by(region.x) %>%
  summarise(gjennomsnitt_verdi = mean(value.x))

gjennomsnitt_x > mean(rompris$value.y)
 # Her lager vi et nytt dataframe for å sammenligne gjennomsnittet per fylke, med landets gjennomsnitt.
større_en_gjennomsnitt <- gjennomsnitt_x$gjennomsnitt_verdi >= mean(rompris$value.y)
cbind(gjennomsnitt_x, større_en_gjennomsnitt)

kappasitet_seng <- dfmerged %>%
   filter(statistikkvariabel == "Kapasitetsutnytting av senger (prosent)") %>%
   filter( år >= 1992)

kappasitet_rom <- dfmerged %>%
  filter(statistikkvariabel == "Kapasitetsutnytting av rom (prosent)")  %>%
  filter( år >= 1992)

cor.test(kappasitet_seng$value.x, rompris$value.x)
cor.test(kappasitet_rom$value.x, rompris$value.x)
# svake korrelasjoner på begge, men litt sterkere på kappasitet rom 