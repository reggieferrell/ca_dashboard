#Reggie Ferrell
#Project - CDE DA Dashboard Creation

#Load packages
packages <- c("haven", "ggplot2", "gapminder", "tidyverse", "dplyr", "stringr", 
              "tidyr", "Cairo", "devtools", "RODBC", "RColorBrewer", "foreign", "knitr", "markdown", 
              "rmarkdown", "tinytex", "kableExtra", "stargazer", "xtable", "readxl", "tidyr", "reshape2",
              "lubridate", "viridis", "haven", "janitor", "wesanderson", "cowplot", "forcats", "ggrepel", 
              "hrbrthemes", "ggalt", "scales", "psych", "corrplot", "gtools", "gapminder", "sf",
              "tigris", "censusapi","tmap", "tidycensus", "mapview","ggmap","lattice","leafpop",
              "vapoRwave", "maps","spData","magick")
invisible(lapply(packages, library, character.only = TRUE))

setwd("/Users/rferrel/Documents/CDE/Data Analysis/Data")

#Import excel files 
files <- list.files(path = "/Users/rferrel/Documents/CDE/Data Analysis/Data", 
                    full.names = F, pattern='*.xlsx') 
names <- str_remove(files, pattern = ".xlsx")
dataframe <- NULL

# Create a structured data frame to fill in instances of missing status level and change level varaibles
statuslevel <- rep(1:5, each=5) 
changelevel <- seq(1:5)
color <- ""
district_count <- ""
studentgroup <- ""
dummy_set <- data.frame(studentgroup,statuslevel, changelevel, color, district_count)  %>% 
  mutate(statuslevel = as.character(statuslevel),
         changelevel = as.character(changelevel))

#Loop for adding all files 
for (f in files) {
  data <- read_excel(f) %>% clean_names() %>%
    mutate(change = as.numeric(change),
           priorstatus = as.numeric(priorstatus),
           currstatus = as.numeric(currstatus),
           changeactual = currstatus-priorstatus,
           changecheck = changeactual-change,
           currdenom = as.numeric(currdenom),
           priordenom = as.numeric(priordenom),
           all_flag = 1) %>%
    filter(rtype == "D",
           color != "0",
           currdenom > 30,
           priordenom > 30) %>%
    select(c(studentgroup,districtname,statuslevel,changelevel,color)) 
  data$indicator <- unlist(strsplit(f,split=".",fixed=T))[1]
  dataframe <- rbind(dataframe, data)
}

###################### Combine all data frames
combo_df <- dataframe %>% 
            mutate(statuslevel = as.character(statuslevel),
                  changelevel = as.character(changelevel),
                  color = as.character(color),
                  all_flag = 1) %>%
  group_by(districtname,studentgroup,statuslevel,changelevel, color, indicator) %>%
  summarise(district_count = sum(all_flag))

###################### List values of loop
student_groups = unique(dataframe$studentgroup)
indicators = unique(dataframe$indicator)
for (g in student_groups) {
for (i in indicators) {
  
cde <- combo_df %>% 
        filter(studentgroup == g,
                  indicator == i) %>% 
  mutate(all_flag = 1) %>% 
  group_by(studentgroup,statuslevel,changelevel, color,indicator) %>%
  summarise(district_count = sum(all_flag)) 

joined_cde <- dummy_set %>% full_join(cde,by = c("studentgroup", "statuslevel", "changelevel")) %>%
  select("studentgroup", "statuslevel", "changelevel", "color.y","district_count.y", "indicator") %>%
  rename("color"="color.y",
         "district_count"="district_count.y") %>%
  mutate(color = ifelse(is.na(color),0,color), #0 is placeholder for "No Color"
         district_count = ifelse(is.na(district_count),0,district_count),
         studentgroup = ifelse(studentgroup == "", g,studentgroup)) %>%
  group_by(studentgroup,statuslevel,changelevel, color, indicator) %>%
  summarise(district_count = max(district_count)) %>% 
  ungroup() %>%
  group_by(studentgroup, statuslevel, changelevel) %>%
  mutate(max_color = max(color)) %>%
  ungroup() %>%
  filter(color == max_color) %>%
  mutate(color = as.character(color)) %>%
        # color = ifelse(color == "0", "No color",color)) %>%
  select(-c(max_color))


cde_clean <- joined_cde %>% mutate(changelevel = ifelse(changelevel == "1", "Declined Significantly \n from Prior Year",changelevel),
                                changelevel = ifelse(changelevel == "2","Declined \n from Prior Year",changelevel),
                                changelevel = ifelse(changelevel == "3","Maintained \n from Prior Year",changelevel),
                                changelevel = ifelse(changelevel == "4","Increased \n from Prior Year",changelevel),
                                changelevel = ifelse(changelevel == "5","Increased Significantly \n from Prior Year",changelevel),
                                statuslevel = ifelse(statuslevel == "1","Very Low",statuslevel),
                                statuslevel = ifelse(statuslevel == "2","Low",statuslevel),
                                statuslevel = ifelse(statuslevel == "3","Medium",statuslevel),
                                statuslevel = ifelse(statuslevel == "4","High",statuslevel),
                                statuslevel = ifelse(statuslevel == "5","Very High",statuslevel)) %>%
  mutate(changelevel = factor(changelevel, levels = c("Declined Significantly \n from Prior Year",
                                                      "Declined \n from Prior Year",
                                                      "Maintained \n from Prior Year","Increased \n from Prior Year",
                                                      "Increased Significantly \n from Prior Year"))) %>%
  mutate(statuslevel = factor(statuslevel, levels = c("Very Low","Low","Medium","High","Very High")),
         indicator = ifelse(is.na(indicator),i,indicator),
         district_count = as.character(district_count),
         district_count = ifelse(district_count == "0","No districts",district_count)) %>%
  na.omit()

colors <- c("No districts" = "#9e9c9d",
            "1" = "#a20000",
            "2" = "#f6a503",
            "3" = "#ffff01",
            "4" = "#216500",
            "5" = "#0700ff")

plot <- ggplot(data = subset(cde_clean, studentgroup == g & indicator == i),
        aes(x = changelevel, y = statuslevel, fill = color)) +
          geom_tile(colour = "black", size = .1, stat = "identity") +
        geom_text(aes(label = district_count),color = "black",fontface = "bold") +
        scale_fill_manual(values = colors) +
        theme( plot.title = element_text(hjust = 0.5, face = "bold"), 
               legend.position = "right", plot.subtitle = element_text(hjust = .5)) +
        labs(title = paste0(g, " ",i),
        subtitle = "2019 - LEA Counts", x = "", y = "",fill = "Performance", caption = g) + 
        scale_x_discrete(position = "top")

  ggsave(plot, filename = paste(g, i,".png",sep = " ")
         , width = 8
         , height = 8
         , type = "cairo-png")
  
}
}




