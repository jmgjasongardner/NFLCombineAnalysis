prospects <- read.csv("Prospects2020_2.csv")

prospects$Position <- as.character(prospects$Position)

for(val in 1:length(prospects$Position)) {
  if(prospects$Position[val] == "LS") {
    prospects$Position[val] <- "OL"
  }
  if(prospects$Position[val] == "S") {
    prospects$Position[val] <- "DB"
  }
  if(prospects$Position[val] == "CB") {
    prospects$Position[val] <- "DB"
  }
}

unique(prospects$Position)

write.csv(prospects, "Prospects2020_2.csv")
