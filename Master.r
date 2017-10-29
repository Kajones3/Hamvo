
#Load in the Tidyvers
install.packages("tidyverse")
library(tidyverse)

#Let's get all the data in and formatted 
main <- read.csv("main.csv", header = T, stringsAsFactors = T)
summary(main)
str(main)
main$Date <- as.Date(main$Date, format="%m/%d/%Y")
str(main)
stock1 <- read.csv("Stock1.csv", header = T, stringsAsFactors = T)
stock1$Date <- as.Date(stock1$Date, format = "%m/%d/%Y")
str(stock1)
stock2 <- read.csv("Stock2.csv", header = T, stringsAsFactors = T)

#Hypothesis1: 
#Examine the week of 12/20/15 through week of 12/27/2015.
#Sales tend to pick up in the first week of January.
#See how the sales have trended for the four week period from 12/13/2015 to 1/9/2016.
#If return rates surge higher than the increase in sales in the week of 1/3/2016,
#seasonality does not explain the surge in returns.

#Let's look at the sales data first.
ggplot(data=main) +
  geom_bar(mapping = aes(x = Date))+
  coord_cartesian(ylim = c(3000,6000))

#Filter out the return data and graph it.
returns <- filter(main, ReturnCode != "")
sales <- filter(main, ReturnCode == "")

ggplot(data=returns) +
  geom_bar(mapping = aes(x = Date))+
  coord_cartesian(ylim = c(500,900))

#It looks like the returns make up a higher percentage of sales 
#Let's check to make sure.
summary(returns$Week)/summary(main$Week)

#Looks like returns made up 16 percent of all transations. Not sure if that is significant.
prop.table(summary(sales$Week))
prop.table(summary(returns$Week))

#Sales / Return percentages were in roughly in line with each other until 1/3/2016.
#At that point, returns jumped ahead by about two percent. Therefore..

# ------------------------------------------------------------------------------------
#---------------------------------------REJECT HYPOTHESIS 1 --------------------------
#-------------------------------------------------------------------------------------

#Hypothesis 2
#Which product categories (or Product IDs) are affecting the overall increase?
#Start at the supercategory and dig down into specific Product IDs
#Compare the average return rate at each product level from 12/27/2015 to 1/3/2016.

#Sub-Hypothesis
#Check to see if this is specific to new products.


#Let's see what is driving the returns.
ggplot(data=returns) +
  geom_bar(mapping = aes(x = returns$ReturnCode, fill = Supercategory))+
  coord_flip()

#Look like RT03 is has almost twice as many returns
#Most of those are in women's fashion.
#can we verify?
summary(returns$ReturnCode)
prop.table(table(returns$ReturnCode, returns$Week),1)
prop.table(table(allreturns$Supercategory, allreturns$Week),1)

#Yup, twice as many returns. Let's break down women's fashion.
womens <- filter(returns, Supercategory == "Women's Fashion")
allreturns <- filter(main, ReturnCode == "RT09")

ggplot(data=womens) +
  geom_bar(mapping = aes(x = womens$ReturnCode, fill = Category))+
  coord_flip()
#Suspect categories are tops&tees, sunglasses, skirts, and jeans.
summary(womens$ProductID)

#Let's see what it looks like when we look at the numbers
table(womens$ReturnCode, womens$Category)

#RT03 is where I want to focus.
#Tops & Tees had 449 returns
#Sunglasses had 339
#Skirts had 257

tops <- filter(womens, Category == "Tops & Tees")
sunglasses <- filter(womens, Category == "Sunglasses")
skirts <- filter(womens, Category == "Skirts")

tops <- sort(table(tops$ProductID), decreasing = TRUE)
head(tops)
#PRD420 PRD315 PRD442 PRD414 PRD481 PRD457 
# 170     67     65     60     58     55

sunglasses <- sort(table(sunglasses$ProductID), decreasing = TRUE)
head(sunglasses)
#PRD446 PRD462 PRD478 PRD364 PRD421 PRD313 
# 174     61     59     58     58     55

skirts <- sort(table(skirts$ProductID), decreasing = TRUE)
head(skirts)

#PRD323 PRD350 PRD463 PRD375 PRD314 PRD345 
# 55     54     54     53     52     52 

# Definitely tops and tees and Sunglasses are where the issue started.
# Skirts don't appear to be as big of an issue as we thought.
# For our analysis, we should target PRD420 and PRD446.

subset(stock1, ProductID == "PRD420")
subset(stock2, ProductID == "PRD420")
#We launched the product on January 3, 2016.
#We sold 347 units and 170 were returned. A 49 percent return rate.

subset(stock1, ProductID == "PRD446")
subset(stock2, ProductID == "PRD446")
#We launched the product on January 6, 2016
#We sold 346 units and 174 were returned. A 50 percent return rate.

#That said, we were never out of stock of either one.
#We may need to look into the quality of those items.

#Let's look at the reasons behind the returns a little more
ggplot(data=womens) +
  geom_bar(mapping = aes(x = womens$Week, fill = ReturnCode),
           position = "Dodge")+
  labs(title = "Returns in Womens Dept. by Return Category", x = "Week", y = "Total Transactions")

#Something is going on with Return Code RT09
rt09 <- filter(womens, ReturnCode == "RT09")

x <- sort(table(rt09$ProductID), decreasing = TRUE)
head(x)
#PRD366 PRD446 PRD420 PRD404 PRD325 PRD358 
# 86     84     68     64      6      6 


ggplot(data=rt09) +
  geom_bar(mapping = aes(x = rt09$Week, fill = ProductID))

#Let's clean things up a bit.
rt09$reason <- 0
rt09$reason[rt09$ProductID == "PRD366"] <- 366
rt09$reason[rt09$ProductID == "PRD446"] <- 446
rt09$reason[rt09$ProductID == "PRD420"] <- 420
rt09$reason[rt09$ProductID == "PRD404"] <- 404

rt09$reason <- as.factor(rt09$reason)

ggplot(data=rt09) +
  geom_bar(mapping = aes(x = rt09$Week, fill = reason))

#Looks like all the products were virtually non existant before week 1/3/2016

subset(stock1, ProductID == "PRD366")
subset(stock2, ProductID == "PRD366")
#This was launched on January 6, 2016.
#It was out of stock for one day. We sold 349 and 86 were returned for RT09.

subset(stock1, ProductID == "PRD404")
subset(stock2, ProductID == "PRD404")
#This was launched on January 6, 2016.
#It was never out of stock. We sold 329 and 64 were returned for RT09.

reason <- sort(table(returns$ProductID), decreasing = TRUE)
head(reason, n=10)

# PRD446 PRD420 PRD108 PRD366 PRD107 PRD154 PRD109 PRD152 PRD153 PRD156 
#  174    170    163    162    161    159    155    153    147    146 

#Are we still confident that women's fashion is the main culprit?
ggplot(data=returns) +
  geom_bar(mapping = aes(x = returns$Week, fill = Supercategory),
          position = "Dodge" )

ggplot(data=returns) +
  geom_bar(mapping = aes(x = returns$Week, fill = ReturnCode),
           position = "Dodge" )


table(returns$Week, returns$ReturnCode)

#We need to look at the top culprits for the problem
topculprits <- subset(returns,
                      ProductID == "PRD446" |
                        ProductID == "PRD420" |
                        ProductID == "PRD108" |
                        ProductID == "PRD366" |
                        ProductID == "PRD107" |
                        ProductID == "PRD154" |
                        ProductID == "PRD109" |
                        ProductID == "PRD152" |
                        ProductID == "PRD153" |
                        ProductID == "PRD156")

ggplot(data=topculprits) +
  geom_bar(mapping = aes(x = topculprits$Week, fill = ProductID),
           position = "Dodge" )


#The "hints" are making me doubt my progress.
rt06 <- filter(main, ReturnCode == "RT06", Week == "Week of 1/3/2016")

six <- sort(table(rt06$ProductID), decreasing = TRUE)
head(six)
#---------------------------------------------------------------------------
#---------------------------HYPOTHESIS 2 -----------------------------------
#Looks like we definitely need to focus on Products 366, 420, 446
#For a quick recap
#PRD366 - Launched: January 6, 2016 - Stock out: 1 - Sold: 349 - Returned: 162
#PRD420 - Launched: January 3, 2016 - Stock out: 0 - Sold: 347 - Returned: 170
#PRD446 - Launched: January 6, 2016 - Stock out: 0 - Sold: 346 - Returned: 174

#We need to check and see if there are problems with the new products in general or
#these products specifically. So, we are going to separate the "returns" df into
#New and Old. If the date is 2015, we'll push it into old. If it is 2016, it will
#be pushed into New. Then we will match the variables from Old to make sure we catch
#all the old ones.

#Now we need to look at return rates by week and see what increased.
prd366 <- filter(main, ProductID == "PRD366")
prd404 <- filter(main, ProductID == "PRD404")
prd420 <- filter(main, ProductID == "PRD420")
prd446 <- filter(main, ProductID == "PRD446")

#Graphs of the return reasons
ggplot(data=prd366) +
  geom_bar(mapping = aes(x = Week, fill = ReturnCode),
           position = "Dodge")+
  labs(title = "Return Reasons for PRD366")

ggplot(data=prd404) +
  geom_bar(mapping = aes(x = Week, fill = ReturnCode),
           position = "Dodge")+
  labs(title = "Return Reasons for PRD404")

ggplot(data=prd420) +
  geom_bar(mapping = aes(x = Week, fill = ReturnCode),
           position = "Dodge")+
  labs(title = "Return Reasons for PRD420")

ggplot(data=prd446) +
  geom_bar(mapping = aes(x = Week, fill = ReturnCode),
           position = "Dodge")+
  labs(title = "Return Reasons for PRD446")
