library(tidyverse)
library(readr)
library(plyr)

# Oppgave 1 
DS = tribble(
  ~fk_account_code, ~Ansvar, ~fk_function_code, ~fk_project_code, ~amount,
  101030,40220,"Det",3432,1493.00,
  101030,40220,"Met",3586,2827.00,
  101030,40320,"Det",3456,49440.00,
  101030,40330,"Sal",NA,870716.00,
  101030,40350,"Met",NA,559928.00,
  101030,40360,"Sal",NA,125534.00,
  101030,40280,"Pol",NA,251611.00)

  #tar vekk fk_project_code
DS <- DS[-4]

  # Fjerner noen av sifrene, får å bare få de 3 siste.
DS$Ansvar <- substr(DS$Ansvar, 1, 3)

 # går tilbake til numeric
DS$Ansvar <- as.numeric(DS$Ansvar)

 # tar summen av ansvar
sum(DS$Ansvar)


 # så lager vi nye labels for "fk_function_code"
DS$fk_function_code <- ifelse(DS$fk_function_code == "Det", "supplies",
                              ifelse(DS$fk_function_code == "Sal", "supplies",
                                     ifelse(DS$fk_function_code == "Met", "inventories",
                                            ifelse(DS$fk_function_code == "Pol", "other expenses", NA))))


 # Oppgave 2
df <- data.frame(Product=gl(3,10,labels=c("A","B", "C")), 
                 Year=factor(rep(2002:2011,3)), 
                 
                 Sales=1:30)
   # Gjør sånn at summen av observasjonene blir 100
funksjon1 <- function(x) (x*100)/sum(x)


df <- plyr :: ddply(df, "Year", transform, Share=funksjon1(Sales))

 # Plot
ggplot() +
  geom_line(data = df, aes (x = Year, y = Sales, group = Product, color = Product)) +
  labs(title = "Salg vert år, per selskap",
       x = "år",
       y = "Salg")