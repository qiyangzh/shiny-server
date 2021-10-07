########################################################################################################
# WebScraping.R
########################################################################################################
# creates a dataset of pages on the E4E website using a list of the pages.

########################################################################################################
# Initial Set-up
########################################################################################################
# Clear Variables
rm(list=ls(all=TRUE))

# Load packages
test<-require(readxl)   #read_excel()
if (test == FALSE) {
  install.packages("readxl")
  require(readxl)
}
test<-require(dplyr)   #bind_rows()
if (test == FALSE) {
  install.packages("dplyr")
  require(dplyr)
}
rm(test)

library(RSelenium)
library(rvest)

########################################################################################################
# Load data
########################################################################################################
pages <- read_excel("PageList_20200722.xlsm")

########################################################################################################
# Clean data
########################################################################################################
# change links to the actual website, not the content site
pages$Link <- gsub(pattern = "content.evidenceforessa", replacement = "www.evidenceforessa", x = pages$Link)
pages$Link <- gsub(pattern = "#", replacement = "", x = pages$Link)

########################################################################################################
# Collect data from website
########################################################################################################

# for local testing
# no studies
# name <- "1:1 iPads"
# url <- "https://www.evidenceforessa.org/programs/reading/11-ipads"
# no significant results
# name <- "Reading Street"
# url <- "https://www.evidenceforessa.org/programs/reading/reading-street"
# strong
# name <- "Reading Partners"
# url <- "https://www.evidenceforessa.org/programs/reading/reading-partners"
# strong2
# name <- "Success for All — Whole Class"
# url <- "https://www.evidenceforessa.org/programs/reading/success-all-whole-class"

# create limited list (for testing)
# pages <- pages[1:10,]

# create placeholder for results
e4e <- data.frame(name = character(0), url = character(0), program = character(0), topic = character(0),
                  rating = character(0), summary = character(0), gradeband = character(0), gradestudied = character(0), 
                  nStudies = character(0), nStudents = character(0), ES = character(0), 
                  groups = character(0), comm = character(0), parent = character(0))

# close any open java tasks (it doesn't always close older ones correctly)
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

# open the browser
rd <- rsDriver(browser = "chrome", chromever = "83.0.4103.39")

for(i in 1:length(pages$TITLE)) {
  # for local testing
  # i <- 44
  
  name <- pages$TITLE[i]
  url <- pages$Link[i]
  
  # got to the url
  rd$client$navigate(url)
  
  ### NEEDS TO WAIT FOR PAGE TO LOAD BEFORE GOING ON
  Sys.sleep(2.5)
  
  # extract content
  program <- rd$client$findElement(using = 'css selector', value = "h1")$getElementText()[[1]]
  topic <- rd$client$findElement(using = 'css selector', value = ".type-label")$getElementText()[[1]]
  rating <- rd$client$findElement(using = 'css selector', value = "p")$getElementText()[[1]]
  
  # check program rating - that determines rest of extraction
  if (substr(rating, 1, 24) == "No studies met inclusion" | rating == "No studies meet inclusion requirements." | rating == "No studies are known to have evaluated this program." | rating =="The current version of this program has no qualifying studies that meet inclusion requirements. Qualifying studies for an earlier iteration found no significant results") {
    ### No studies
    # no more data, so fill the rest with missing and move on
    summary <- rating
    rating <- "No studies"
    results <- data.frame(name, url, program, topic, rating, summary)
    rm(name, url, program, topic, rating)
  } else if (substr(rating, 1, 31) == "Qualifying studies found no sig" | rating == "Qualifying studies showed significantly positive and significantly negative results." | 
             rating == "Qualifying studies involving first graders found no significant positive outcomes. There were positive effects for prekindergarten.") {
    ### No significant results
    # no more data, so fill the rest with missing and move on
    summary <- rating
    rating <- "No significant results or conflicting results"
    results <- data.frame(name, url, program, topic, rating, summary)
    rm(name, url, program, topic, rating)
  } else if (rating == "This program is no longer being disseminated." | rating == "This intervention is no longer being disseminated." | 
             rating == "Breakthrough to Literacy is no longer being disseminated." | substr(rating, 1, 11) == "Please see ") {
    ### Program unavailable
    # no more data, so fill the rest with missing and move on
    summary <- rating
    rating <- "Program unavailable or duplicated"
    results <- data.frame(name, url, program, topic, rating, summary)
    rm(name, url, program, topic, rating)
  } else if (topic == "SOCIAL-EMOTIONAL") {
    ### If SEL, much of info is in am image - need to ask SD.
    summary <- rating
    rating <- "Evidence-Based"
    results <- data.frame(name, url, program, topic, rating, summary)
    rm(name, url, program, topic, rating)
  } else {
    ### Program has a rating (strong/moderate/promising)
    summary <- rating
    gradeband <- rd$client$findElement(using = 'css selector', value = ".grade-levels")$getElementText()[[1]]
    gradestudied <- rd$client$findElements(using = 'css selector', value = ".grades-studied p")
    if(length(gradestudied) > 0) {
      gradestudied <- gradestudied[[1]]$getElementText()[[1]]
    } else {
      gradestudied <- NA
    }
    rating <- rd$client$findElement(using = 'css selector', value = ".essa-rating")$getElementText()[[1]]
    # Badge
    nStudies <- rd$client$findElement(using = 'css selector', value = ".number-of-studies")$getElementText()[[1]]
    nStudents <- rd$client$findElement(using = 'css selector', value = ".number-of-students")$getElementText()[[1]]
    ES <- rd$client$findElement(using = 'css selector', value = ".average-effect-size")$getElementText()[[1]]
    
    groups <- rd$client$findElements(using = 'css selector', value = ".grades-studied+ .groups-studied")
    if(length(groups) > 0) {
      groups <- groups[[1]]$getElementText()[[1]]
    } else {
      groups <- NA
    }
    comm <- rd$client$findElements(using = 'css selector', value = ".groups-studied+ .groups-studied")
    if(length(comm) > 0) {
      comm <- comm[[1]]$getElementText()[[1]]
    } else {
      comm <- NA
    }
    # Parent Engagement Tools
    parent <- rd$client$findElements(using = 'css selector', value = ".text")
    if (length(parent) > 0) {
      parent <- parent[[1]]$getElementText()[[1]]
    } else {
      parent <- NA
    }
    results <- data.frame(name, url, program, topic, rating, gradeband, gradestudied, nStudies, nStudents, 
                          ES, groups, comm, parent)
    rm(name, url, program, topic, rating, summary, gradeband, gradestudied, nStudies, nStudents, 
       ES, groups, comm, parent)
  }
  # print(results)
  
  e4e <- bind_rows(e4e, results)
  rm(results)
}

# close browser and close connection
rd$client$close()
rd$server$stop()
rm(rd)

table(e4e$rating, e4e$topic)
########################################################################################################
# Save Output
########################################################################################################
library(xlsx)
write.xlsx(x = e4e, file = "e4e.xlsx", row.names = FALSE, showNA = FALSE)

beepr::beep()



