library(MASS)

rb <- read.csv("RBs.csv")

rb.full <- rb[is.na(rb$Forty) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Weight) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Height) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$ThreeCone) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Broad) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Bench) == FALSE, ]
rb.full <- rb.full[is.na(rb.full$Shuttle) == FALSE, ]

rblm2 <- lm(AV16 ~ Height + Weight + Forty + ThreeCone + Broad + Bench + Shuttle, 
            data = rb.full)
summary(rblm2)
summary(stepAIC(object = rblm2, direction = c("backward")))
