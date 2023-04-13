library(tidyverse)
library(rvest)
library(tibble)

base_url <- "http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=2000-01-01&EndDate=2023-04-12&InjuriesChkBx=yes&Submit=Search"

base_webpage <- read_html(base_url)

new_urls <- "http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=2000-01-01&EndDate=2023-04-12&InjuriesChkBx=yes&Submit=Search&start=%s"

table_base <- html_table(base_webpage)[[1]] %>% 
  as_tibble(.name_repair = "unique")

tabtable_new <- data.frame()
df <- data.frame()

i <- 0

while (i < 25426) {
  print(i)
  new_webpage <- read_html(sprintf(new_urls,i))
  table_new <- rvest::html_table(new_webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  df<- rbind(df,table_new)
  i=i+25
}

df <- df %>% 
  rename(
    Date = X1,
    Team = X2,
    Acquired = X3,
    Name = X4,
    Notes = X5
  )

j <- 1

while (j < 30000) {
  if (!(df$Acquired[j] == "")) {
    df <- df[-j,]
  }
  j <- j + 1
}

injuries <- df %>% 
  select(Date, Team, Name, Notes) %>% 
  mutate(Name = gsub("• ", "", Name))

injuries_freq <- injuries %>% 
  count(Name)

injuries_freq <- injuries_freq[-1,]
