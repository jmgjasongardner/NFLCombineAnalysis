library(MASS)

rb <- read.csv("RBs.csv")

rb.full <- rb[is.na(rb$X40.YD) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Weight) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Height) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$X3Cone) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Broad) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Bench) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Shuttle) == FALSE, ]

rblm2 <- lm(AV16 ~ Height + Weight + X40.YD + X3Cone + Broad + Bench + Shuttle, 
            data = rb.full)
summary(rblm2)
summary(stepAIC(object = rblm2, direction = c("backward")))
# sig = 40yd, 3cone, shuttle

##-----------------------------
qb <- read.csv("QBs.csv")
qb.full <- qb[is.na(qb$Forty) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Weight) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Height) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Vert) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Broad) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Shuttle) == FALSE, ]
qb.full <- qb.full[is.na(qb.full$Wonderlic) == FALSE, ]

qblm2 <- lm(AV16 ~ Height + Weight + Forty + Broad + Vert + Shuttle + Wonderlic, 
            data = qb.full)
summary(qblm2)
summary(stepAIC(object = qblm2, direction = c("backward")))
# sig = broad, vertical 

##----------------------------
ol <- read.csv("OLs.csv")
ol.full <- ol[is.na(ol$Forty) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$Weight) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$Height) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$Broad) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$Bench) == FALSE, ]
ol.full <- ol.full[is.na(ol.full$ThreeCone) == FALSE, ]

ollm2 <- lm(AV16 ~ Height + Weight + Forty + Broad + Bench + ThreeCone, 
            data = ol.full)
summary(ollm2)
summary(stepAIC(object = ollm2, direction = c("backward")))
# sig = weight, 40 

##-------------------------
wr <- read.csv("WRs.csv")
wr.full <- wr[is.na(wr$Forty) == FALSE, ]
wr.full <- wr.full[is.na(wr.full$ThreeCone) == FALSE, ]
wr.full <- wr.full[is.na(wr.full$Shuttle) == FALSE, ]

wrlm2 <- lm(All.Pros ~ Forty + ThreeCone + Shuttle, 
            data = wr.full)
summary(wrlm2)
summary(stepAIC(object = wrlm2, direction = c("backward")))
# nothing sig :(
