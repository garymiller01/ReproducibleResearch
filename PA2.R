
# PA 2

# PART 2 - Data Processing

# Create Data folder if not already existing

if(!file.exists("data")) {
    dir.create("data")
}

# download data

FileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(FileUrl,destfile="./data/StormData.csv.bz2")

DownloadDate <- format(Sys.Date(),"%B %d %Y")

# Read In Data

StormData <- read.csv(bzfile("./data/StormData.csv.bz2"))

# Explore

dim(StormData)
names(StormData)

# Data Type Conversion

StormData$BGN_DATE <- strptime(StormData$BGN_DATE,format="%m/%d/%Y")
StormData$BGN_YEAR <- format(StormData$BGN_DATE,"%Y")

# Subset Data

table(StormData$BGN_YEAR)


StormData <- StormData[StormData$BGN_YEAR>=1961,]

# US States

table(StormData$STATE)

States <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA",
            "ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS",
            "MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA",
            "RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")

StormData <- StormData[StormData$STATE %in% States,]

# Column Reduction

KeepCols <- c("EVTYPE","BGN_YEAR","FATALITIES","INJURIES","PROPDMG",
              "PROPDMGEXP","CROPDMG","CROPDMGEXP")

StormData <- StormData[,names(StormData) %in% KeepCols]

dim(StormData)
head(StormData,5)

# Event Type Aggregation

length(unique(StormData$EVTYPE))

StormData$EVTYPE <- toupper(StormData$EVTYPE)

StormData$EVLABEL <- "OTHER"
StormData$EVLABEL[grepl("FIRE",StormData$EVTYPE)] <- "FIRE"
StormData$EVLABEL[grepl("DUST",StormData$EVTYPE)] <- "DUST"
StormData$EVLABEL[grepl("FLOOD|SURGE",StormData$EVTYPE)] <- "FLOOD"
StormData$EVLABEL[grepl("HAIL",StormData$EVTYPE)] <- "HAIL"
StormData$EVLABEL[grepl("WIND",StormData$EVTYPE)] <- "HIGH WIND"
StormData$EVLABEL[grepl("WARM|HOT|DROUGHT|DRY|DRI|HEAT|
                        HIGH",StormData$EVTYPE)] <- "HEAT"
StormData$EVLABEL[grepl("COLD|FROST",StormData$EVTYPE)] <- "COLD"
StormData$EVLABEL[grepl("SNOW|BLIZZARD|WINT*MIX|WINT*STORM|ICE",
                        StormData$EVTYPE)] <- "SNOWSTORM"
StormData$EVLABEL[grepl("TORN|SPOUT|WALL|FUNNEL",StormData$EVTYPE)]<-"TORNADO"
StormData$EVLABEL[grepl("HURRICANE|TYPHOON|TROP",StormData$EVTYPE)]<-"HURRICANE"
StormData$EVLABEL[grepl("THUN|TSTM|MICROB|RAIN",StormData$EVTYPE)] <- "THUNDERSTORM"

YYZ <- StormData[StormData$EVLABEL=="OTHER",]
table(YYZ$EVTYPE)



# Population Damage

StormData$CASUALTIES <- StormData$INJURIES + StormData$FATALITIES

# Property Damage

table(StormData$PROPDMGEXP)

# Create columns for multiplier based on code

StormData$PROPDMGMULTIPLIER <- as.numeric(1) 

# Populate multiplier values based on code

StormData$PROPDMGMULTIPLIER[StormData$PROPDMGEXP=="K"|
                            StormData$PROPDMGEXP=="k"|
                            StormData$PROPDMGEXP=="3"] <- 1000
StormData$PROPDMGMULTIPLIER[StormData$PROPDMGEXP=="4"] <- 10000
StormData$PROPDMGMULTIPLIER[StormData$PROPDMGEXP=="5"] <- 100000
StormData$PROPDMGMULTIPLIER[StormData$PROPDMGEXP=="M"|
                                StormData$PROPDMGEXP=="m"|
                                StormData$PROPDMGEXP=="6"] <- 1000000
StormData$PROPDMGMULTIPLIER[StormData$PROPDMGEXP=="7"] <- 10000000
StormData$PROPDMGMULTIPLIER[StormData$PROPDMGEXP=="8"] <- 100000000
StormData$PROPDMGMULTIPLIER[StormData$PROPDMGEXP=="B"] <- 1000000000

table(StormData$PROPDMGMULTIPLIER)

# Calculate total Property Damage as new variable

StormData$PROPDMGTOTAL <- StormData$PROPDMG * StormData$PROPDMGMULTIPLIER

# Repeat for Crop Damage

table(StormData$CROPDMGEXP)

StormData$CROPDMGMULTIPLIER <- as.numeric(1)

StormData$CROPDMGMULTIPLIER[StormData$CROPDMGEXP=="K"|
                            StormData$CROPDMGEXP=="k"] <- 1000
StormData$CROPDMGMULTIPLIER[StormData$CROPDMGEXP=="M"|
                            StormData$CROPDMGEXP=="m"] <- 1000000
StormData$CROPDMGMULTIPLIER[StormData$CROPDMGEXP=="B"] <- 1000000000

table(StormData$CROPDMGMULTIPLIER)

# Get total economic damage and add to new column

StormData$CROPDMGTOTAL <- StormData$CROPDMG * StormData$CROPDMGMULTIPLIER

# Reorder columns

StormData$TOTALDAMAGE <- StormData$PROPDMGTOTAL + StormData$CROPDMGTOTAL

StormData <- StormData[c("EVLABEL","EVTYPE","BGN_YEAR","CASUALTIES","INJURIES",
                         "FATALITIES","TOTALDAMAGE","PROPDMG","PROPDMGEXP",
                         "PROPDMGTOTAL","CROPDMG","CROPDMGEXP",
                         "CROPDMGMULTIPLIER","CROPDMGTOTAL")]

head(StormData,5)

# Aggregate by Event Label

library(reshape2)

# Casualties

# Get data in 'long' format

StormCasualtyMelt <- melt(StormData,id="EVLABEL",measure.vars=
                        c("FATALITIES","INJURIES"))

StormEcoMelt <- melt(StormData,id="EVLABEL",measure.vars=
                         c("CROPDMGTOTAL","PROPDMGTOTAL"))

# Plots 

library(ggplot2)    
library(scales)    

# Count of Storms Per Year

h <- ggplot(StormData,aes(x=BGN_YEAR))

h2 <- h + geom_histogram(fill="blue") +
    labs(x="Year",y="Number of Storms") + 
    labs(title="Number of Storms by Year, 1961 - 2011") +
    scale_x_discrete(breaks=seq(1961,2011,5)) 

# Plot Casualty Info

c <- ggplot(StormCasualtyMelt,aes(x=EVLABEL,y=value,fill=variable))

c2 <- c + geom_bar(stat="identity") + 
    labs(x="Event Type",y="Number of Casualties") + 
    labs(title="Combined Fatalities and Injuries by Event Type") +
    scale_fill_manual(values=c("#e34a33","#fdcc8a"),name="Casualty Type") +
    scale_y_continuous(label=comma) +
    theme(axis.text.x = element_text(angle=90))

head(StormCasualtyMelt)

# Plot Property Info

p <- ggplot(StormEcoMelt,aes(x=EVLABEL,y=value,fill=variable))

p2 <- p + geom_bar(stat="identity") + 
    labs(x="Event Type",y="Total Property and Crop Damage ($US)") +
    labs(title="Property Damage by Event Type") +
    scale_fill_manual(values=c("#2ca25f","#b2e2e2"),name="Damage Type",
                      labels=c("CROP","PROPERTY")) + 
    scale_y_continuous(label=comma) +
    theme(axis.text.x = element_text(angle=90))
