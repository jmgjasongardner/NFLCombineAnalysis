# Load in packages
library(MASS)

########## Regression on AV16 ##########
##-----------------------------
# Load in position CSV
qb <- read.csv("QBs.csv")

# Subset data to remove NA
qb.full <- qb[is.na(qb$Forty) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Weight) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Height) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Vert) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Broad) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Shuttle) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Wonderlic) == FALSE, ]

# Full linear model
qblm2 <- lm(AV16 ~ Height + Weight + Forty + Broad + Vert + Shuttle + Wonderlic, 
            data = qb.full)
summary(qblm2)

# Variable selection
summary(stepAIC(object = qblm2, direction = c("backward")))
# sig = broad, vertical 

##-----------------------------
# Load in position CSV
rb <- read.csv("RBs.csv")

# Subset data to remove NA
rb.full <- rb[is.na(rb$Forty) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Weight) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Height) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$ThreeCone) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Broad) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Bench) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Shuttle) == FALSE, ]

# Full linear model
rblm2 <- lm(AV16 ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = rb.full)
summary(rblm2)

# Variable selection
summary(stepAIC(object = rblm2, direction = c("backward")))
# sig = 40yd, 3cone, shuttle

##-------------------------
# Load in position CSV
wr <- read.csv("WRs.csv")

# Subset data to remove NA
wr.full <- wr[is.na(wr$Forty) == FALSE, ]
wr.full <- wr.full[is.na(wr.full$ThreeCone) == FALSE, ]
wr.full <- wr.full[is.na(wr.full$Broad) == FALSE, ]

# Full linear model
wrlm2 <- lm(AV16 ~ Forty + ThreeCone + Broad, 
            data = wr.full)
summary(wrlm2)

# Variable selection
summary(stepAIC(object = wrlm2, direction = c("backward")))
# nothing sig :(

##-----------------------------
# Load in position CSV
te <- read.csv("tes.csv")

# Subset data to remove NA
te.full <- te[is.na(te$Forty) == FALSE, ]
te.full <- te.full[is.na(te.full$Weight) == FALSE, ]
te.full <- te.full[is.na(te.full$Height) == FALSE, ]
te.full <- te.full[is.na(te.full$ThreeCone) == FALSE, ]
te.full <- te.full[is.na(te.full$Broad) == FALSE, ]
te.full <- te.full[is.na(te.full$Bench) == FALSE, ]
te.full <- te.full[is.na(te.full$Shuttle) == FALSE, ]

# Full linear model
telm2 <- lm(AV16 ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = te.full)
summary(telm2)

# Variable selection
summary(stepAIC(object = telm2, direction = c("backward")))
# sig = 40yd

##----------------------------
# Load in position CSV
ol <- read.csv("OLs.csv")

# Subset data to remove NA
ol.full <- ol[is.na(ol$Forty) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$Weight) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$Height) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$ThreeCone) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$Broad) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$Bench) == FALSE, ]

# Full linear model
ollm2 <- lm(AV16 ~ Height + Weight + Forty + Broad + Bench + ThreeCone, 
            data = ol.full)
summary(ollm2)

# Variable selection
summary(stepAIC(object = ollm2, direction = c("backward")))
# sig = weight, 40yd

##-----------------------------
# Load in position CSV
dl <- read.csv("dls.csv")

# Subset data to remove NA
dl.full <- dl[is.na(dl$Forty) == FALSE, ]
dl.full <- dl.full[is.na(dl.full$Height) == FALSE, ]
dl.full <- dl.full[is.na(dl.full$Weight) == FALSE, ]
dl.full <- dl.full[is.na(dl.full$ThreeCone) == FALSE, ]
dl.full <- dl.full[is.na(dl.full$Broad) == FALSE, ]

# Full linear model
dllm2 <- lm(AV16 ~ Weight + Forty + ThreeCone + Broad + Height, 
            data = dl.full)
summary(dllm2)

# Variable selection
summary(stepAIC(object = dllm2, direction = c("backward")))
# sig = weight, 3cone, broad, forty (to 0.1)

##-----------------------------
# Load in position CSV
lb <- read.csv("lbs.csv")

# Subset data to remove NA
lb.full <- lb[is.na(lb$Forty) == FALSE, ]
lb.full <- lb.full[is.na(lb.full$Weight) == FALSE, ]
lb.full <- lb.full[is.na(lb.full$Height) == FALSE, ]
lb.full <- lb.full[is.na(lb.full$ThreeCone) == FALSE, ]
lb.full <- lb.full[is.na(lb.full$Broad) == FALSE, ]
lb.full <- lb.full[is.na(lb.full$Bench) == FALSE, ]
lb.full <- lb.full[is.na(lb.full$Shuttle) == FALSE, ]

# Full linear model
lblm2 <- lm(AV16 ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = lb.full)
summary(lblm2)

# Variable selection
summary(stepAIC(object = lblm2, direction = c("backward")))
# sig = weight, broad, 3cone (to 0.1)

##-----------------------------
# Load in position CSV
db <- read.csv("dbs.csv")

# Subset data to remove NA
db.full <- db[is.na(db$Forty) == FALSE, ]
db.full <- db.full[is.na(db.full$Weight) == FALSE, ]
db.full <- db.full[is.na(db.full$Height) == FALSE, ]
db.full <- db.full[is.na(db.full$ThreeCone) == FALSE, ]
db.full <- db.full[is.na(db.full$Broad) == FALSE, ]
db.full <- db.full[is.na(db.full$Bench) == FALSE, ]
db.full <- db.full[is.na(db.full$Shuttle) == FALSE, ]

# Full linear model
dblm2 <- lm(AV16 ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = db.full)
summary(dblm2)

# Variable selection
summary(stepAIC(object = dblm2, direction = c("backward")))
# sig = 40yd, weight, broad (to 0.1)



########## Regression on Pick ##########
##-----------------------------
# Full linear model
qblm3 <- lm(Pick.Number ~ Height + Weight + Forty + Broad + Vert + Shuttle + Wonderlic, 
            data = qb.full)
summary(qblm3)

# Variable selection
summary(stepAIC(object = qblm3, direction = c("backward")))
# sig = broad 

##-----------------------------
# Full linear model
rblm3 <- lm(Pick.Number ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = rb.full)
summary(rblm3)

# Variable selection
summary(stepAIC(object = rblm3, direction = c("backward")))
# sig = weight, 40yd, 3cone (to 0.1), broad (to 0.1)

##-----------------------------
# Full linear model
wrlm3 <- lm(Pick.Number ~ Forty + ThreeCone + Broad, 
            data = wr.full)
summary(wrlm3)

# Variable selection
summary(stepAIC(object = wrlm3, direction = c("backward")))
# sig = 40yd

##-----------------------------
# Full linear model
telm3 <- lm(Pick.Number ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = te.full)
summary(telm3)

# Variable selection
summary(stepAIC(object = telm3, direction = c("backward")))
# sig = 40yd, bench

##-----------------------------
# Full linear model
ollm3 <- lm(Pick.Number ~ Height + Weight + Forty + Broad + Bench + ThreeCone, 
            data = ol.full)
summary(ollm3)

# Variable selection
summary(stepAIC(object = ollm3, direction = c("backward")))
# sig = height, weight, 40yd, bench

##-----------------------------
# Full linear model
dllm3 <- lm(Pick.Number ~ Weight + Forty + ThreeCone + Broad + Height, 
            data = dl.full)
summary(dllm3)

# Variable selection
summary(stepAIC(object = dllm3, direction = c("backward")))
# sig = weight, 40yd, 3cone (to 0.1), broad (to 0.1)

##-----------------------------
# Full linear model
lblm3 <- lm(Pick.Number ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = lb.full)
summary(lblm3)

# Variable selection
summary(stepAIC(object = lblm3, direction = c("backward")))
# sig = weight, broad, 40yd, shuttle

##-----------------------------
# Full linear model
dblm3 <- lm(Pick.Number ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = db.full)
summary(dblm3)

# Variable selection
summary(stepAIC(object = dblm3, direction = c("backward")))
# sig = 40yd, 3cone, broad

