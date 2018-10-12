library(readr)
library(tidyverse)
library(dplyr)
library(plyr)
getwd()
nat_2017 <- read_fwf("R/Nat2017PublicUS.c20180516.r20180808.txt", 
                     fwf_positions(start = c(13, 475, 504),
                                   end = c(14, 475, 507),
                                   col_names = c("birth_month", "sex", "birth_weight")
                                   )
   # får frekvensen mellom kjønnene                                )
nat2017freq <- count(nat_2017$sex)

nat2017proportion <- nat2017freq$freq[2]/nat2017freq$freq[1] 


# gjør fødselsvekt om til numeric
nat_2017$birth_weight <- as.numeric(nat_2017$birth_weight)

# filtrer ut ekstreme observasjoner og finner gjennomsnittet
nat_2017 <- filter(nat_2017, birth_weight < 7000 & birth_weight > 1000)
mean_nat_2017 <- mean(nat_2017$birth_weight)
# lager et density plott
ggplot(nat_2017, aes(x = birth_weight)) + geom_density() + facet_grid(sex ~ .) + 
  labs(title = "Birthweight plot", x = "birthweight", y = "density")
# varier ikke mye per måned
average_month <- aggregate(birth_weight ~ birth_month, nat_2017, mean)

