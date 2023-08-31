install.packages("dplyr")
install.packages("rvest")
library(rvest)
library(dplyr)

link = "https://www.mygov.in/corona-data/covid19-statewise-status/"
page = read_html(link)

State = page %>% html_nodes(".field-type-list-text .even") %>% html_text()
Total_Cases = page %>% html_nodes(".field-name-field-total-confirmed-indians .even") %>% html_text() %>% as.integer()
Recovered = page %>% html_nodes(".field-name-field-cured .even") %>% html_text() %>% as.integer()
Deaths = page %>% html_nodes(".field-name-field-deaths .even") %>% html_text() %>% as.integer()

covid = data.frame(State, Total_Cases, Recovered, Deaths, stringsAsFactors = FALSE)

----------------------------------------------------------------------

install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
library(tidyverse)
library(dplyr)
library(tidyr)

covid$population <- c(399001,91702478,1711947,35998752,128500364,1158040,32199722,
  453008,19301096,1521992,70400153,28900667,7503010,14999397,40100376,69599762,
  34698876,301000,66001,124904071,3436948,3772103,1308967,85002417,2073074,47099270,
  1646050,30501026,79502477,658019,83697770,38157311,4184959,231502578,11700099,
  100896618)

covid$Vaccination <- c(88 , 99 , 54 , 63 , 55, 82 , 69 , 64 , 76 , 83 , 77 , 67 , 
                        86 , 88 , 46 , 83 , 71 , 69 , 89 , 62 , 42 , 33 , 62 , 70 , 
                        34 , 72 , 55 , 69 , 64 , 82 , 75 , 84 , 62 , 73 , 77 , 68)

covid <- covid %>% mutate(Cases_per_Million =Total_Cases*1000000/population)
covid <- covid %>% mutate(Deaths_per_Million =Deaths*1000000/population)

glimpse(covid)

covid$Total_Cases_Spread <- ifelse(covid$Total_Cases > 1500000 , 'Very High',
                       ifelse(covid$Total_Cases > 500000 & covid$Total_Cases <= 1500000 , "High",
                              ifelse(covid$Total_Cases <= 500000 , "Average", NA)))

covid$Spread <- ifelse(covid$Cases_per_Million > 50000, 'Very High',
                       ifelse(covid$Cases_per_Million > 25000 & covid$Cases_per_Million <= 50000, "High",
                              ifelse(covid$Cases_per_Million <= 25000, "Average", NA)))

covid %>% group_by(Spread) %>% drop_na() %>% summarize(mean_spread=mean(Cases_per_Million))
covid %>% group_by(Total_Cases_Spread) %>% drop_na() %>% summarize(mean_Total_Cases_Spread=mean(Total_Cases))

view(covid)

install.packages("skimr")
library(skimr)
skim_without_charts(covid)



write.csv(covid, "covid.csv", row.names = FALSE)

-----------------------------------------------------------------------
