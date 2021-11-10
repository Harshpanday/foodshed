setwd("C:/Users/user/Desktop/RA")
#install.packages(c("googlesheets4","googledrive"))

library(devtools)
#devtools::install_github("tidyverse/googlesheets4")

library(googlesheets4)
gs4_auth()

#loading the INFACT google sheet
infact <- read_sheet("https://docs.google.com/spreadsheets/d/1g4zlXncQ2L0t90TUZ_WleXRPy_ok0cIrpI4i5Q-odPk")
#deleting the first 5 rows
infact <- infact[-c(1,2,3,4,5),]

#loading main pepod sheets
masterpeo <- read_sheet("https://docs.google.com/spreadsheets/d/1RGXDzUerTf5NBpVTP4EailD2_Q6IPPaWDEurXgC1T3s/edit#gid=1990303639")
masterorg <- read_sheet("https://docs.google.com/spreadsheets/d/1RGXDzUerTf5NBpVTP4EailD2_Q6IPPaWDEurXgC1T3s/edit#gid=1126966587")

#loading the people and organization google sheet
people <- gs4_get("https://docs.google.com/spreadsheets/d/1rTO-Zn7tuEcThxSeBYCEQuokXzfe0O-lEuJCXp2anLM")
organizations <- gs4_get("https://docs.google.com/spreadsheets/d/1qp5XRbFY0QmVEYptJ7co5c40PZ2hoki_DPQilMNkpVE")




#CREATING LOCAL PEOPLE SHEET TO UPLOAD TO OFFICIAL PEOPLE SHEET
#creating a data frame to add to people
peo <- data.frame(NA)
#copying the columns from the INFACT dataframe
peo <- cbind(peo,infact[c(1,2)])
peo$NA. <- NULL 
colnames(peo) <- c('First_name','Last_name')
library('stringr')
#combining First name and Last name
peo$Combined <- str_c(peo$First_name, ' ', peo$Last_name)
#adding the email column to peo
peo <- cbind(peo,infact[c(3)])
names(peo)[names(peo)=='...3'] <- 'Email'

#changing column name
names(peo)[names(peo)=='Combined'] <- 'Full Name'

#changing column location
peo <- peo[,c(3,1,2,4)]


#adding columns
columns_peo = c("Phone","Issues","Notes")
peo[columns_peo] <- NA

peo <- peo[-c(1,2),]
#Writing on the Google sheet
range_write(
  people,
  peo,
  sheet = NULL,
  range = NULL,
  col_names = TRUE,
  reformat = TRUE
  
)
library(dplyr)

#CREATING LOCAL ORGANIZATION SHEET
org <- data.frame(NA)
org <- cbind(org,infact[c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,29,30,31,32,33,34,35,36,37,38,39,40,84,85,86,87,88,89,90,91,92,93)])
org$NA. <- NULL
#org <- org[-c(1),]
#columns_org <- c("Alias","isPartOf","isMemberOf","County","Ecoregion","hasGeography","hasOrgType","Partners","Funding","hasOrgActivity","Issues","URL","Contact","Taxa","Land Cover - CWHR","Ecological Process")
names(org)[names(org)=='...4'] <- 'Organizations'
names(org)[names(org)=='...5'] <- 'ONE1 Columbus'
names(org)[names(org)=='...6'] <- 'RGP Northwest Ohio'
names(org)[names(org)=='...7'] <- 'Team neo'
names(org)[names(org)=='...8'] <- 'Ohio Southeast Economic Development'
names(org)[names(org)=='...9'] <- 'REDI Cincinnati'
names(org)[names(org)=='...10'] <- 'ddc|dayton development coalition'
names(org)[names(org)=='...29'] <- 'International'
names(org)[names(org)=='...30'] <- 'Federal'
names(org)[names(org)=='...31'] <- 'State'
names(org)[names(org)=='...32'] <- 'City'
names(org)[names(org)=='...33'] <- 'County'
names(org)[names(org)=='...34'] <- 'Agriculture Agency'
names(org)[names(org)=='...35'] <- 'Environmental Agency'
names(org)[names(org)=='...36'] <- 'Natural Resources Agency'
names(org)[names(org)=='...37'] <- 'Health Agency'
names(org)[names(org)=='...38'] <- 'Native American/ Tribal'
names(org)[names(org)=='...39'] <- 'Elected Official/ Policy maker'
names(org)[names(org)=='...40'] <- 'Quasi Government'
names(org)[names(org)=='...11'] <- 'Faculty'
names(org)[names(org)=='...12'] <- 'Staff'
names(org)[names(org)=='...13'] <- 'Extension Specialist/ Educator/ Advisor/ Director'
names(org)[names(org)=='...14'] <- 'Student'
names(org)[names(org)=='...15'] <- 'Ohio'
names(org)[names(org)=='...16'] <- 'National'
names(org)[names(org)=='...17'] <- 'International_partner'

names(org)[names(org)=='...84'] <- 'Linkage & Leverage Grant'
names(org)[names(org)=='...85'] <- 'Executive Committee'
names(org)[names(org)=='...86'] <- 'Faculty Hire'
names(org)[names(org)=='...87'] <- 'OSA'
names(org)[names(org)=='...88'] <- 'ISA'
names(org)[names(org)=='...89'] <- 'Food Panel'
names(org)[names(org)=='...90'] <- 'NSF RCN'
names(org)[names(org)=='...91'] <- 'Ethics of Food'
names(org)[names(org)=='...92'] <- 'Challenge of Change'
names(org)[names(org)=='...93'] <- 'Ohio Microfarm Project'

columns_org <- c("Ecoregion","Goverment","Internal_Partner","External_Partner","InFACT_Project_Affiliation")
org[columns_org] <- NA
org <- org %>% 
  mutate(Ecoregion = case_when(`ONE1 Columbus` == "x" ~'ONE1 Columbus',
                               `RGP Northwest Ohio` == "x" ~'RGP Northwest Ohio',
                               `Team neo` == "x" ~'Team neo',
                               `Ohio Southeast Economic Development` == "x" ~'Ohio Southeast Economic Development',
                               `REDI Cincinnati` == "x" ~'REDI Cincinnati',
                               `ddc|dayton development coalition` == "x" ~'ddc|dayton development coalition'))

org <- org %>% 
  mutate(Goverment = case_when(`International` == "x" ~'International',
                               `Federal` == "x" ~'Federal',
                               `State` == "x" ~'State',
                               `City` == "x" ~'City',
                               `County` == "x" ~'County',
                               `Agriculture Agency` == "x" ~'Agriculture Agency',
                              `Environmental Agency` == "x" ~'Environmental Agency',
                              `Natural Resources Agency` == "x" ~'Natural Resources Agency',
                              `Health Agency` == "x" ~'Health Agency',
                              `Native American/ Tribal` == "x" ~'Native American/ Tribal',
                              `Elected Official/ Policy maker` == "x" ~'Elected Official/ Policy maker',
         `Quasi Government` == "x" ~'Quasi Government',
         ))


org <- org %>% 
  mutate(Internal_Partner = case_when(`Faculty` == "x" ~'Faculty',
                               `Staff` == "x" ~'Staff',
                               `Extension Specialist/ Educator/ Advisor/ Director` == "x" ~'Extension Specialist/ Educator/ Advisor/ Director',
                               `Student` == "x" ~'Student'))

org <- org %>% 
  mutate(External_Partner = case_when(`Ohio` == "x" ~'Ohio',
                                      `National` == "x" ~'National',
                                      `International_partner` == "x" ~'International'))


org <- org %>% 
  mutate(InFACT_Project_Affiliation = case_when(`Linkage & Leverage Grant` == "x" ~'Linkage & Leverage Grant',
                                                `Executive Committee` == "x" ~'Executive Committee',
                                                `Faculty Hire` == "x" ~'Faculty Hire',
                                                `OSA` == "x" ~'OSA',
                                                `ISA` == "x" ~'ISA',
                                                `Food Panel` == "x" ~'Food Panel',
                                                `NSF RCN` == "x" ~'NSF RCN',
                                                `Ethics of Food` == "x" ~'Ethics of Food',
                                                `Challenge of Change` == "x" ~'Challenge of Change',
                                                `Ohio Microfarm Project` == "x" ~'Ohio Microfarm Project'
                                                
  ))


remove <- c("ONE1 Columbus","RGP Northwest Ohio","Team neo","Ohio Southeast Economic Development","REDI Cincinnati","ddc|dayton development coalition",
            'International','Federal','State','City','County','Agriculture Agency','Environmental Agency','Environmental Agency','Natural Resources Agency','Health Agency','Native American/ Tribal','Elected Official/ Policy maker','Quasi Government',
            'Faculty','Staff','Extension Specialist/ Educator/ Advisor/ Director','Student','Ohio','National','International_partner',
            'Linkage & Leverage Grant','Executive Committee','Faculty Hire','OSA','ISA','Food Panel','NSF RCN','Ethics of Food','Challenge of Change','Ohio Microfarm Project')

org <- org[,!(names(org) %in% remove)]

org <- org[-c(1,2),]
range_write(
  organizations,
  org,
  sheet = NULL,
  range = NULL,
  col_names = TRUE,
  reformat = TRUE
  
)


#Getting metadata
library("dataverse")
metadata1 <- data.frame(dataset_metadata("doi:10.5072/FK2/X3NUUT",
                                         server="https://od.commons.osu.edu/dataverse/foodshed"))
metadata2 <- data.frame(dataset_metadata("doi:10.5072/FK2/Z9YVVQ",
                                         server="https://od.commons.osu.edu/dataverse/foodshed"))
library(RCurl)
#rm(commons)
dataset_id <- list("doi:10.5072/FK2/GQMRXA","doi:10.5072/FK2/Z9YVVQ")
i=1
while (i<= length(dataset_id)) {
  meta <- data.frame(dataset_metadata("doi:10.5072/FK2/X3NUUT",
                           server="https://od.commons.osu.edu/dataverse/foodshed"))
  da <- meta$fields$value[[4]]
  ea <- da[1,]
  i=i+1
  
}
da <- data.frame(NA)
da <- data.frame(dataset_metadata("doi:10.5072/FK2/GQMRXA",
                                  server="https://od.commons.osu.edu/dataverse/foodshed"))



library(rvest)
#ids = list("doi:10.5072/FK2/1KPBDE","doi:10.5072/FK2/RT8ZCJ","doi:10.5072/FK2/NM88CP","doi:10.5072/FK2/LFHLIO",
#           "doi:10.5072/FK2/9QLLC3","doi:10.5072/FK2/QQ7JCO","doi:10.5072/FK2/V2V18F","doi:10.5072/FK2/BKDNRI",
#           "doi:10.5072/FK2/3OX3YI","doi:10.5072/FK2/AWVZBJ","doi:10.5072/FK2/FHSZGO","doi:10.5072/FK2/JPGNJQ",
#           "doi:10.5072/FK2/9208JK","doi:10.5072/FK2/TRLK7I","doi:10.5072/FK2/3S2HBR","doi:10.5072/FK2/0DHQXU",
#           "doi:10.5072/FK2/POT9YT","doi:10.5072/FK2/BXYHF3","doi:10.5072/FK2/JK4MVE","doi:10.5072/FK2/TOFVRO",
#           "doi:10.5072/FK2/OFFNQA","doi:10.5072/FK2/DDMC2A","doi:10.5072/FK2/XWGL5X","doi:10.5072/FK2/BLZ2MS",
#           "doi:10.5072/FK2/XBDUT9","doi:10.5072/FK2/VE4CXB","doi:10.5072/FK2/OSLSS0","doi:10.5072/FK2/Q9Y3Y9",
#           "doi:10.5072/FK2/WRNJNC","doi:10.5072/FK2/ZPIF1V","doi:10.5072/FK2/UK4XLN","doi:10.5072/FK2/X3NUUT",
#           "doi:10.5072/FK2/GQMRXA","doi:10.5072/FK2/Z9YVVQ")

Sys.setenv("DATAVERSE_SERVER" = "https://od.commons.osu.edu/dataverse/foodshed")
dv <- dataverse_contents("foodshed")
#dv1 <- data.frame(dv[[1]])
id=list()

for (i in 1:length(dv)){
  identifier <- dv[[i]]$identifier
  id <- append(id,identifier)
}

author <- data.frame(NA)
title <- data.frame(NA)
url <- data.frame(NA)
for (i in id){
  link = sprintf("https://od.commons.osu.edu/dataset.xhtml?persistentId=doi:10.5072/%s",i)
  page = read_html(link)
  table = page %>% html_nodes("table.metadata") %>%
    html_table() %>% .[[2]]
  a <- which(table$X1 == "Author")
  t <- which(table$X1 == "Title")
  title_no <- table[c(t),]$X2
  author_name <- table[c(a),]$X2
  author = rbind(author,author_name)
  title = rbind(title,title_no)
  url <- rbind(url,link)
}
dataset_sheet <- cbind.data.frame(title,author,url)
names(dataset_sheet)[1] <- 'Title'
names(dataset_sheet)[2] <- 'Organizations'
names(dataset_sheet)[3] <- 'URL'

dataset_sheet <- dataset_sheet[-c(1),]
dataset <- gs4_get("https://docs.google.com/spreadsheets/d/16RZEewsB3-xpWAGZRh8lDuLPhMorNWoSRN9cUj92_Nc/edit#gid=0")
range_write(
  dataset,
  dataset_sheet,
  sheet = NULL,
  range = NULL,
  col_names = TRUE,
  reformat = TRUE
  
)


#String matching
library(stringdist)
stringdist("USDA", "USDA-APHIS National Wildlife Research Center", method = "lv")
st <- c("USDA Food Surveys Research Group",
        "USDA-APHIS National Wildlife Research Center",
        "USDA-APHIS National Wildlife Research Center",
        "USDA-APHIS National Wildlife Research Center",
        "Johns Hopkins University",
        "USDA Office of the Chief Economist",
        "USDA Economic Research Service",
        "Michigan State University",
        "Michigan State University")
grep('USDA ERS', st)

f <- Vectorize(function(x, y) {
  xx <- strsplit(tolower(x), '')[[1]]
  grepl(paste0(xx, collapse = '[a-z]*?'), y)
  ## add this if you only want to consider letters in y
  grepl(paste0(xx, collapse = sprintf('[%s]*?', tolower(y))), y)
}, vectorize.args = 'x')

f(c("cdc","michigan state university"),"office of academic affairs, infact")
x <- data.frame((f(c("usda","michigan state university"),"michigan state university")))
ind <- which(x[,1]==TRUE)
y <- rownames(x)[ind]

st <- tolower(st)

u <- org$"Organizations"
u <- data.frame(u)

low <- data.frame((dataset_sheet$"Organizations"))
z <- data.frame(NA)
data<- dataset_sheet
x<-data.frame("c")
qwe <- data.frame(NA)
rty <- data.frame(NA)

for (i in org$"Organizations"){
  for (j in dataset_sheet$"Organizations"){
    if ((f(c(tolower(j)),tolower(i))) == TRUE){
      #x <- data.frame(j)
      #y <- data.frame(i)
      hp <- which(org$"Organizations" == i)
      np <- which(dataset_sheet$"Organizations" == j)
      mp <- (org[c(hp),]$"Organizations")
      rp <- (dataset_sheet[c(np),]$"Organizations")
      qwe <- rbind(qwe,mp)
      rty <- rbind(rty,rp)
      #new <- c(org[c(hp),]$"Organizations",dataset_sheet[c(np),]$"Organizations")
      #qwe[nrow(qwe)+1, ] <- new
      #balle <- org[c(hp),]$"Organizations"
      
    }
  }
}
data <- cbind.data.frame(qwe,rty)

abcd <- list("ode","osp")
efgh <- list("ohio department of health","ohio sand pop","michigan state","penn state")

for (i in efgh){
  for(j in abcd){
    if ((f(c(j),i)) == TRUE){
      hp <- 
      qwe <- rbind(qwe,efgh[c(i)])
    }
  }
}



#FUZZY MATCHING
z <- org
r <- c("Ecoregion","Goverment","Internal_Partner","External_Partner","InFACT_Project_Affiliation")
z <- z[,!(names(z) %in% r)]
z <- data.frame(z)
names(z)[names(z)=='z'] <- 'Organizations'
w<- dataset_sheet
re <- c("Title","URL")
w <- w[,!(names(w) %in% re)]
w<-data.frame(w)
names(w)[names(w)=='w'] <- 'Organizations'
library(fuzzyjoin); library(dplyr);
x<- data.frame(stringdist_join(z, w, 
                by = "Organizations",
                mode = "left",
                ignore_case = TRUE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(Organizations.x) %>%
  slice_min(order_by = dist, n = 1))


org_alias <- read_sheet("https://docs.google.com/spreadsheets/d/1fsIl40p4Exjy3_xr7c1v0xbu3vEZoc18GPc-3WQ4Tbc/edit#gid=0")
o_a <- data.frame()
alias_name <- data.frame()
alias_title <- data.frame()
alias_url <- data.frame()


for (i in 1:length(dataset_sheet$Organizations)){
  for (j in 1:length(org_alias$Alias)){
    sp <- data.frame(strsplit(org_alias$Alias[[j]],","))
    for (k in sp[,c(1)]){
      if((dataset_sheet$Organizations[[i]] == k)){
        on <- (org_alias[c(j),]$"Organizations")
        ourl <- (dataset_sheet[c(i),]$"URL")
        otitle <- (dataset_sheet[c(i),]$"Title")
        alias_name <- rbind(alias_name,on)
        alias_title <- rbind(alias_title,otitle)
        alias_url <- rbind(alias_url,ourl)
    
    }
  }
}
}

alias <- cbind.data.frame(alias_title,alias_name,alias_url)


org_n <- data.frame()
a_n <- data.frame()
d2 <- data.frame()

for (i in 1:length(org_alias$Alias)){
  or <- (org_alias[c(i),]$"Organizations")
  sp <- data.frame(strsplit(org_alias$Alias[[i]],","))
  for (j in sp[,c(1)]){
    d <- data.frame(or,j)
    d2 <- rbind(d2,d)
  }
}


o <- data.frame(strsplit(org_alias$Alias[[2]],","))
