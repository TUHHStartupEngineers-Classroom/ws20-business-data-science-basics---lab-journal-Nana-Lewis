#1. Libraries ----
#Lets load some libraries
#Better load a lot of libraries than run into some errors :)
#Dont be surprised when I dont use some

library(glue)
library(httr)
library(purrr)
library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(stringi)  
library(jsonlite)
library(readxl)
library(lubridate)
library("writexl")
library(vroom)
library(data.table)
library(tictoc)
library(magrittr)




# Question  Table
# 1 assignee, patent_assignee
# 2 assignee, patent_assignee, patent
# 3 assignee, patent_assignee, uspc

#2.1 Assignee Data ----
assignee_col_types <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)
assignee_tbl <- vroom(
  file       = "~/R_scripts/patent_data/Patent_data_reduced/assignee.tsv", 
  delim      = "\t", 
  col_types  = assignee_col_types,
  na         = c("", "NA", "NULL")
)

#2.2 Patent_assignee Data ----
patent_assignee_col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)
patent_assignee_tbl <- vroom(
  file       = "~/R_scripts/patent_data/Patent_data_reduced/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = patent_assignee_col_types,
  na         = c("", "NA", "NULL")
)

#2.3 Patent Data ----
patent_col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "~/R_scripts/patent_data/Patent_data_reduced/patent.tsv", 
  delim      = "\t", 
  col_types  = patent_col_types,
  na         = c("", "NA", "NULL")
)
#2.4 USPC data
uspc_col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "~/R_scripts/patent_data/Patent_data_reduced/uspc.tsv", 
  delim      = "\t", 
  col_types  = uspc_col_types,
  na         = c("", "NA", "NULL")
)



#3.0 Check for class and Set as data.table
#Check class
class(assignee_tbl)





#SetDT
setDT(assignee_tbl)
setDT(patent_assignee_tbl)
setDT(uspc_tbl)
setDT(patent_tbl)



#Step1: Join data(assignee and patent assignee)
#data.table code

# patent_dominance_tbl <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
#                        by    = c("id" = "assignee_id"), 
#                        all.x = TRUE, 
#                        all.y = FALSE)
# patent_dominance_tbl %>% glimpse()
# 
patent_dominance_tbl <- assignee_tbl %>%
  left_join(patent_assignee_tbl, by = c("id" = "assignee_id"))
patent_dominance_tbl %>% glimpse()



#Step2: Summarize data
patent_dominance_tbl %>%
  group_by(organization) %>%
  summarise(patent_dominance = n()) %>%
  ungroup() %>%
  arrange(desc(patent_dominance))



#CHANGE COLUMN_NAME IN PATENT_TBL, ID=PATENT_ID
setnames(patent_tbl, "id", "patent_id")

recent_patent_activity_tbl <-  patent_dominance_tbl%>%
  left_join(patent_tbl, by = "patent_id")

#Step2: Summarize data by organization and Year
recent_patent_activity_date_tbl <- recent_patent_activity_tbl %>%
  separate(col  = date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE)

recent_patent_activity_date_tbl %>%
  filter(month == 10) %>%
  group_by(organization) %>%
  summarise(recent_patent_activity = n()) %>%
  ungroup() %>%
  arrange(desc(recent_patent_activity))




#Step: Join data(assignee, patent_assignee(patent_dominance_tbl), uspc)
innovation_in_tech_tbl <-  patent_dominance_tbl%>%
  left_join(uspc_tbl, by = "patent_id")

top_10_patents <- innovation_in_tech_tbl %>%
  select(organization, patent_id, mainclass_id) %>%
  group_by(organization,mainclass_id) %>%
  summarise(top_10 = n()) %>%
  ungroup() %>%
  arrange(desc(top_10)) 

top_10_patents %>%
  select(organization,mainclass_id)%>%
  group_by(mainclass_id) %>%
  summarise(top_5_USPO = n()) %>%
  ungroup %>%
  arrange(desc(top_5_USPO))





