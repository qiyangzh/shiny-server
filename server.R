########################################################################################################
# E4E_EGM.R
########################################################################################################
# Create process for producing EGM for E4E with a shiny app (development stage)
#
# Steps:
# 1. Load full original data (once per use, save as original) [load_SEL()]
# 2. Load default settings (all data) [apply_choices(design)]
# 3. Format data for EGM [prep_egm(data)]
# 4. Create EGM [make_egm(data)]
#
# Re-run 2-4 as new settings are chosen by user
#
# Saved each step as a function.

########################################################################################################
# Initial Set-up
########################################################################################################
# Clear Variables
rm(list=ls(all=TRUE))

# Load packages
test<-require(janitor)   #remove_empty()
if (test == FALSE) {
  install.packages("janitor")
  require(janitor)
}
test<-require(plyr)   # rename()
if (test == FALSE) {
  install.packages("plyr")
  require(plyr)
}
test<-require(metafor)   #rma(): regular meta-analysis
if (test == FALSE) {
  install.packages("metafor")
  require(metafor)
}
test<-require(clubSandwich)   #coeftest(): adjust for RVE
if (test == FALSE) {
  install.packages("clubSandwich")
  require(clubSandwich)
}
test<-require(readxl) #read_excel
if (test == FALSE) {
  install.packages("readxl")
  require(readxl)
}
test<-require(officer)
if (test == FALSE) {
  install.packages("officer")
  require(officer)
}
test<-require(flextable) 
if (test == FALSE) {
  install.packages("flextable")
  require(flextable)
}
test<-require(huxtable)
if (test == FALSE) {
  install.packages("huxtable")
  require(huxtable)
}
rm(test)

load_E4E <- function() {
  ########################################################################################################
  # Load data
  ########################################################################################################
  full <- read_excel("E4E_20200722.xlsx")

  ########################################################################################################
  # Clean data
  ########################################################################################################
  # remove any empty rows & columns
  full <- remove_empty(full, which = c("rows", "cols"))

  # limit to relevant columns
  full <- full[c("name", "topic", "rating", "gradeband", "url","gradestudied", "nStudies",
                         "nStudents", "ES")] 
  
  # rename columns
  #old name = new name
  full <- plyr::rename(full, c("topic" = "topic", "ES" = "Effect Size", "nStudents" = "Sample Size", "rating" = "Evidence Tier Rating"))

  # remove exactly duplicated rows
  full <- unique(full)
  
  # limit to relevant rows
    ########### IF YOU DO THIS, YOU LOSE ALL THE NO EVIDENCE STUDIES
  # # # drop if ES is missing
  # full <- subset(full, is.na(full$"Effect Size") == FALSE)
  # # # drop if category isn't coded
  # full <- subset(full, is.na(full$topic) == FALSE)
  # # # drop if nStudents is missing
  # full <- subset(full, is.na(full$"Sample Size") == FALSE)
  # # drop those where Program is not available or duplicated
  full <- subset(full, full$"Evidence Tier Rating" != "Program unavailable or duplicated")
  full <- subset(full, full$"Evidence Tier Rating" != "No studies")
  # limit to just reading and math
  full <- subset(full, full$topic == "MATH" | full$topic == "READING")

  
  # reformat columns as necessary
  fact<-c("name")
  full[fact] <- lapply(full[fact], as.factor)
  rm(fact)
  
  # # any numbers with commas become NA, so remove commas
  full$`Sample Size` <- gsub(",", "", full$`Sample Size`)
  # # a few ESs are strings...limit to first full ES
  full$`Effect Size`[which(substr(full$`Effect Size`, 1, 11) == "+0.21 (PK),")] <- "+0.21"
  full$`Effect Size`[which(substr(full$`Effect Size`, 1, 8) == "+0.33 (K")] <- "+0.33"
  ### MORE ELEGANT TO HAVE IT JUST 'BREAK' AT THE FIRST SPACE
  
  num <- c("Sample Size", "Effect Size", "nStudies")
  full[num] <- lapply(full[num], as.numeric)
  rm(num)
  return(full)
}

### take user input, limit data to relevant observations
apply_choices <- function(full, Rating, Topic, Grade){
  subset <- full
  # limit to relevant studies
  # if(Evidence == "Evidence Absent"){
  #   subset <- subset(subset, subset$"Evidence Tier Rating" == "No studies" | subset$"Evidence Tier Rating" == "No significant results or conflicting results")
  # }
  # if(Evidence == "Evidence Present"){
  #   subset <- subset(subset, subset$"Evidence Tier Rating" != "No studies" & subset$"Evidence Tier Rating" != "No significant results or conflicting results")
  # }
  if(Rating == "Promising"){
    subset <- subset(subset, subset$"Evidence Tier Rating" == "PROMISING")
  }
  if(Rating == "Strong"){
    subset <- subset(subset, subset$"Evidence Tier Rating" == "STRONG")
  }
  if(Rating == "Moderate"){
    subset <- subset(subset, subset$"Evidence Tier Rating" == "MODERATE")
  }
  if(Rating == "No significance"){
    subset <- subset(subset, subset$"Evidence Tier Rating" == "No significant results or conflicting results")
  }
  # if(Topic=="Attendance"){
  #   subset <- subset(subset, subset$topic=="ATTENDANCE")
  # }
  if(Topic == "Math"){
    subset <- subset(subset, subset$topic == "MATH")
  }
  if(Topic == "Reading"){
    subset <- subset(subset, subset$topic == "READING")
  }
  # if(Topic=="Social-emotional Learning"){
  #   subset <- subset(subset, subset$topic=="SOCIAL-EMOTIONAL")
  # }
  if(Grade == "Kindergarten"){
    subset <- subset(subset, subset$"gradeband" == "PREK - K" )
  }
  if(Grade == "Elementary"){
    subset <- subset(subset, subset$"gradeband" == "1 - 6" | subset$"gradeband" == "3 - 6" | 
                     subset$"gradeband" == "1 - 2" | subset$"gradeband" == "PREK - 6" |
                     subset$"gradeband" == "PREK - 2")
  }
  if(Grade == "Middle"){
    subset <- subset(subset, subset$"gradeband" == "MIDDLE" )
  }
  if(Grade == "High"){
    subset <- subset(subset, subset$"gradeband" == "HIGH" )
  }
  return(subset)
}

########################################################################################################
# Analyze data: create summaries
########################################################################################################
# process data for EGM
prep_egm <- function(data){
  ### WANT COLORS BY EVIDENCE TIER RATING
  
  data$color <- "white"
  data$color[which(data$`Evidence Tier Rating` == "STRONG")] <- "green4"
  data$color[which(data$`Evidence Tier Rating` == "MODERATE")] <- "orange"
  data$color[which(data$`Evidence Tier Rating` == "PROMISING")] <- "pink"
  data$color[which(data$`Evidence Tier Rating` == "No significant results or conflicting results")] <- "blue"
  
  data$size <- NA
  data$size[which(data$`Sample Size` < 100)] <- 1
  data$size[which(data$`Sample Size` >= 100 & data$`Sample Size` < 250)] <- 2
  data$size[which(data$`Sample Size` >= 250 & data$`Sample Size` < 500)] <- 3
  data$size[which(data$`Sample Size` >= 500 & data$`Sample Size` < 1000)] <- 4
  data$size[which(data$`Sample Size` >= 1000)] <- 5
  
 #  data$order <- as.numeric(paste(as.numeric(as.factor(data$topic)), as.numeric(data$name), sep = ""))
 # 
 # source("https://raw.githubusercontent.com/janhove/janhove.github.io/master/RCode/sortLvls.R")
 # library(magrittr)
 # 
 #  data$name <- sortLvlsByVar.fnc(as.factor(data$name), data$order)
 # 
  # create new dataframe, to shift dots, and organize categories/labels
  df <- data
  df$a <- as.numeric(df$topic)
  df$b <- as.numeric(df$name)
  df$c <- df$color
  #df$d <- sqrt(df$`Sample Size`)
  df$d <- df$"nStudies"
  dst <- .2
  
  # create new dataframe, to shift dots, and organize categories/labels
  df.mod <- df
  # # avals <- unique(df$a)   # get number of categories
  # avals <- unique(df$a)   # get number of categories
  # bvals <- unique(df$b)   # get number of Programs
  # for (k1 in seq_along(avals)) {   # for each category
  #   for (k2 in seq_along(bvals)) {   # for each program
  #     subk <- (df$a == avals[k1] & df$b == bvals[k2])   # check data frame for that program in that subcategory
  #     if (sum(subk) > 1) {   # if there is more than 1 datapoint for that combination,
  #       subdf <- df[subk,]
  #       angsk <- seq(0,2*pi,length.out=nrow(subdf)+1)
  #       ak <- subdf$a + cos(angsk[-1]) * dst
  #       bk <- subdf$b + sin(angsk[-1]) * dst
  #       df.mod[subk,c("a","b")] <- cbind(ak,bk)
  #     }
  #   }
  # }
  return(df.mod)
}

# function to take relevant data slice and return the egm
make_egm <- function(data){
  # for local testing
  # data <- egm_df
  
  library(ggplot2)
  library(stringr)
  data$name <- str_wrap(data$name, width = 25)
  data$topic <- str_wrap(data$topic, width = 30)
  
  egm <- ggplot(data, aes(x = size, y = "Effect Size", colour = c, size = d)) + 
    geom_jitter(width = 0.3, height = 0.3, color = data$color) + scale_size(range = c(2, 6)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_vline(xintercept=c(seq(1.5, length(unique(data$size)), 1)),color = "grey") + 
    geom_hline(yintercept = c(seq(1.5, length(unique(data$"Effect Size")), 1)), color = "grey") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
    #theme(axis.title.x = "Sample Size Proxy", axis.title.y = "Effect Size", legend.position = "none") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none") + 
    theme(axis.text.y = element_text(size = 16)) +
    # facet_grid(Type ~ topic, scales="free", space = "free") +
    theme(panel.spacing = unit(0,"cm")) + 
    theme(strip.text.x = element_text(size = 16)) + 
    theme(strip.text.y = element_text(size = 16))
    
  # ggsave("egm.png", egm, width = 6, height = 9, units = c("in"))
  return(egm)
}

# for saving output for troubleshooting
# savedata <- function(data){
#   # data <- data
#   agg_color <- data[c("name", "topic", "color")]
#   agg_sample <- data[c("name", "topic", "color","Sample Size")]
#   
#   library(reshape2)
#   wide_color <- dcast(agg_color, name ~ topic)
#   wide_sample <- dcast(agg_sample, name ~ topic)
#   
#   write.csv(wide_color, "EGM_color.csv")
#   write.csv(wide_sample, "EGM_size.csv")
#   write.csv(data, "EGM_data.csv")
# }
