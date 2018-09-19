# Problem Statment
# XYX, an asset management company wants to make investments in a few companies. 
# The CEO of XYX wants to understand the global trends in investments so that investment
# decisions can be made effectively.

# Objective
# The objective is to identify the best sectors, countries, and a suitable investment type 
# for making investments. The overall strategy is to invest where others are investing, 
# implying that the best sectors and countries are the ones where most investments are happening.

# Constraints
# Investment amount should be between 5 to 15 million USD per round of investment
# Investment should only be done in English-speaking countries because of the ease 
# of communication with the companies 
# XYX wants to invest where most other investors are investing. 


# CHECKPOINT 1
# Load companies and round2 data into two data frames
companies <- read.delim("companies.txt", header = TRUE)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)

#Lets see how many unique companies are present in rounds2
unique_rnd2_companies <- length(unique(tolower(rounds2$company_permalink)))

#Lets see how many unique companies are present in companies?
unique_companies <- length(unique(tolower(companies$permalink)))

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
#Name the merged frame master_frame. How many observations are present in master_frame?
rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)
master_frame <- merge(x=rounds2, y=companies, by.x="company_permalink", by.y="permalink")


# CHECKPOINT 2
install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)
groupbyRoundType <- group_by(master_frame, funding_round_type)
summarise(groupbyRoundType, mean(raised_amount_usd, na.rm = TRUE))
#Average funding amount of venture type	11748949
#Average funding amount of angel type	958694
#Average funding amount of seed type	719818
#Average funding amount of private equity type	73308593
#Considering that XYX wants to invest between 5 to 15 million USD per investment round,
#"venture" investment type is the most suitable 	



# CHECKPOINT 3
#XYX wants to see the top nine countries which have received the highest total funding
#(across ALL sectors for the chosen investment type)
#For the chosen investment type, make a data frame named top9 with the top nine countries 
#(based on the total investment amount each country has received)

# from the 'master_frame' filter the data where funding type is 'venture'
VentureTypeDataSet <- subset(master_frame, master_frame$funding_round_type=="venture")
#Group the country code based on the total investment
groupByCountry <- arrange(aggregate(raised_amount_usd~country_code, data = VentureTypeDataSet, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))
# remove the country code where country code is empty
groupByCountry <- groupByCountry [- which(groupByCountry$country_code ==""),]
# Compare these top 9 values with the pdf provided to find out the english speaking countries
# Do note that CHN i.e. China tops the 3, however english is not official language in China and hence ignoring china
top9 <- groupByCountry[1:9,]



#Checkpoint 4
#Load mapping file
mappingFile <- read.csv("mapping.csv", check.names = FALSE)
# Data clean up: clean the category list column of mapping file - replace 0 with na to have proper meaning
mappingFile$category_list <- sapply(mappingFile$category_list, function(x) gsub("0","na",x,ignore.case=T))

# convert wide to long 
mappingFile_long <- gather(mappingFile, main_sector, main_sector_val,2:10)
# Filter only the rows where value is not 0
mappingFile_long <- mappingFile_long[!(mappingFile_long$main_sector_val == 0), ]
# remove the last column as it is always going to be 1 - redundant column
mappingFile_long <- mappingFile_long[, -3]

# split the category_list column to two separate columns as 'primary sector' and 'sub-sectors' with separator as |
# Do note that | is special character and hence need to use escape character as \\
master_frame_separated <- separate(master_frame, category_list, into=c("primary sector", "sub-sectors"), sep="\\|", extra = "merge", fill = "right")

#below is merged data frame with each primary sector mapped to its main sector
merged_data <- merge(x = master_frame_separated, y = mappingFile_long, by.x = "primary sector", by.y = "category_list")


#Checkpoint 5
# create 3 data frames with funding type of venture, amount between 5M to 15M and for each country of USA, GBR, IND
D1 <- filter(merged_data, country_code == "USA" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
D2 <- filter(merged_data, country_code == "GBR" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
D3 <- filter(merged_data, country_code == "IND" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)

#Total number of investments in D1
D1TotalCountInvested <- nrow(D1)
#Total amount of investment in D1
D1TotalAmountInvested <- sum(D1$raised_amount_usd)
#group by D1's main sector based on number of investments
D1CountInvested <- arrange(aggregate(raised_amount_usd~main_sector, data = D1, FUN= length ), desc(raised_amount_usd))
#group by D1's main sector based on amount of investment
D1AmountInvested <- arrange(aggregate(raised_amount_usd~main_sector, data = D1, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))

#Total number of investments in D2
D2TotalCountInvested <- nrow(D2)
#Total amount of investment in D2
D2TotalAmountInvested <- sum(D2$raised_amount_usd)
#group by D2's main sector based on number of investments
D2CountInvested <- arrange (aggregate(raised_amount_usd~main_sector, data = D2, FUN= length ), desc(raised_amount_usd))
#group by D2's main sector based on amount of investment
D2AmountInvested <- arrange(aggregate(raised_amount_usd~main_sector, data = D2, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))

#Total number of investments in D3
D3TotalCountInvested <- nrow(D3)
#Total amount of investment in D3
D3TotalAmountInvested <- sum(D3$raised_amount_usd)
#group by D3's main sector based on number of investments
D3CountInvested <- arrange (aggregate(raised_amount_usd~main_sector, data = D3, FUN= length ), desc(raised_amount_usd))
#group by D3's main sector based on amount of investment
D3AmountInvested <- arrange(aggregate(raised_amount_usd~main_sector, data = D3, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))

# Since 'Others' main sector has maximum investement in D1, group the result by amount of investment and find the company name
D1Others <- filter(D1, main_sector == "Others")
D1OthersAmountInvested <- arrange(aggregate(raised_amount_usd~name, data = D1Others, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))

# Since ' Social, Finance, Analytics, Advertising' main sector has next highest investement in D1, group the result by amount of investment and find the company name
D1SocFinAnaAdv <-   filter(D1, main_sector == "Social, Finance, Analytics, Advertising")
D1SocFinAnaAdvAmountInvested <- arrange(aggregate(raised_amount_usd~name, data = D1SocFinAnaAdv, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))

# Since 'Others' main sector has maximum investement in D2, group the result by amount of investment and find the company name
D2Others <- filter(D2, main_sector == "Others")
D2OthersAmountInvested <- arrange(aggregate(raised_amount_usd~name, data = D2Others, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))

# Since 'Social, Finance, Analytics, Advertising' main sector has next highest investement in D2, group the result by amount of investment and find the company name
D2SocFinAnaAdv <-   filter(D2, main_sector == "Social, Finance, Analytics, Advertising")
D2SocFinAnaAdvAmountInvested <- arrange(aggregate(raised_amount_usd~name, data = D2SocFinAnaAdv, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))

# Since 'Others' main sector has maximum investement in D3, group the result by amount of investment and find the company name
D3Others <- filter(D3, main_sector == "Others")
D3OthersAmountInvested <- arrange(aggregate(raised_amount_usd~name, data = D3Others, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))

# Since 'Social, Finance, Analytics, Advertising' main sector has next highest investement in D3, group the result by amount of investment and find the company name
D3SocFinAnaAdv <-   filter(D3, main_sector == "Social, Finance, Analytics, Advertising")
D3SocFinAnaAdvAmountInvested <- arrange(aggregate(raised_amount_usd~name, data = D3SocFinAnaAdv, FUN = sum, na.rm = TRUE), desc(raised_amount_usd))



