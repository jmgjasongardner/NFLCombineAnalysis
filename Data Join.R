## Load packages
library(plyr)
library(dplyr)

## Load in data sets
AVPlayers <- read.csv("AVPlayers.csv")
DataUpdate1 <- read.csv("DataUpdate1.csv")

## Change to character for manipulation
DataUpdate1$Position <- as.character(DataUpdate1$Position)
AVPlayers$position <- as.character(AVPlayers$position)

## Standardize names for join
AVPlayers$Name <- AVPlayers$full_name
AVPlayers$Position <- AVPlayers$position

## Remove unnecessary data
AVPlayers <- AVPlayers[, -c(1, 7, 8, 12, 13)]

## Make data look nicer 
colnames(DataUpdate1) <- c("Season", "Name", "College", "Position", "Height",
                           "Weight", "Wonderlic", "40 YD", "Bench", "Vert", "Broad",
                           "Shuttle", "3Cone", "Pro Bowls", "All Pros")
colnames(AVPlayers) <- c("Draft Year", "Team", "Draft Round", "Pick Number", "PlayerID",
                         "Side", "Category", "position", "Games", "Avg Value", "AV16",
                         "Name", "Position")

## Standardize position naming
for(val in 1:length(DataUpdate1$Position)) {
  if(DataUpdate1$Position[val] == "OT") {
    DataUpdate1$Position[val] <- "OL"
  }
  if(DataUpdate1$Position[val] == "OG") {
    DataUpdate1$Position[val] <- "OL"
  }
  if(DataUpdate1$Position[val] == "C") {
    DataUpdate1$Position[val] <- "OL"
  }
  if(DataUpdate1$Position[val] == "ILB") {
    DataUpdate1$Position[val] <- "LB"
  }
  if(DataUpdate1$Position[val] == "OLB") {
    DataUpdate1$Position[val] <- "LB"
  }
  if(DataUpdate1$Position[val] == "FS") {
    DataUpdate1$Position[val] <- "DB"
  }
  if(DataUpdate1$Position[val] == "SS") {
    DataUpdate1$Position[val] <- "DB"
  }
  if(DataUpdate1$Position[val] == "CB") {
    DataUpdate1$Position[val] <- "DB"
  }
  if(DataUpdate1$Position[val] == "DE") {
    DataUpdate1$Position[val] <- "DL"
  }
  if(DataUpdate1$Position[val] == "DT") {
    DataUpdate1$Position[val] <- "DL"
  }
}

for(val in 1:length(AVPlayers$Position)) {
  if(AVPlayers$Position[val] == "OLB") {
    AVPlayers$Position[val] <- "LB"
  }
  if(AVPlayers$Position[val] == "ILB") {
    AVPlayers$Position[val] <- "LB"
  }
  if(AVPlayers$Position[val] == "NT") {
    AVPlayers$Position[val] <- "DL"
  }
  if(AVPlayers$Position[val] == "T") {
    AVPlayers$Position[val] <- "OL"
  }
  if(AVPlayers$Position[val] == "G") {
    AVPlayers$Position[val] <- "OL"
  }
  if(AVPlayers$Position[val] == "OLB") {
    AVPlayers$Position[val] <- "LB"
  }
  if(AVPlayers$Position[val] == "DE") {
    AVPlayers$Position[val] <- "DL"
  }
  if(AVPlayers$Position[val] == "DT") {
    AVPlayers$Position[val] <- "DL"
  }
  if(AVPlayers$Position[val] == "CB") {
    AVPlayers$Position[val] <- "DB"
  }
  if(AVPlayers$Position[val] == "FS") {
    AVPlayers$Position[val] <- "DB"
  }
  if(AVPlayers$Position[val] == "SS") {
    AVPlayers$Position[val] <- "DB"
  }
  if(AVPlayers$Position[val] == "S") {
    AVPlayers$Position[val] <- "DB"
  }
  if(AVPlayers$Position[val] == "C") {
    AVPlayers$Position[val] <- "OL"
  }
  if(AVPlayers$Position[val] == "OT") {
    AVPlayers$Position[val] <- "OL"
  }
  if(AVPlayers$Position[val] == "LS") {
    AVPlayers$Position[val] <- "OL"
  }
  if(AVPlayers$Position[val] == "FB") {
    AVPlayers$Position[val] <- "RB"
  }
}

## Check to make sure positions are correct; AVPlayers has K and P, otherwise same
# unique(DataUpdate1$Position)
# unique(AVPlayers$Position)

## Join!
Data <- join(AVPlayers, DataUpdate1, by = c("Name", "Position"))

## Remove dupe players and unnecessary columns
Data <- Data[-c(56, 151, 248, 354, 416, 419, 477, 661, 663, 722, 916, 985, 
                989:990, 1060, 1019, 1357, 1381, 1433, 1441, 1469, 1697, 1753, 
                1771, 1803, 1810:1811, 1845, 1873, 1930, 2043, 2050, 2107, 2190, 
                2455, 2540, 2562, 2604, 2621, 2721, 2768, 2782, 2800, 2897, 2916, 
                3028, 3809, 3523, 3653, 3736, 3799), -c(6:8, 14)]

## Create 2020 only set
Data20 <- subset(DataUpdate1, Season == 2020)

## Write!
write.csv(Data, "DataUpdate3.csv")
write.csv(Data20, "Prospects2020.csv")
