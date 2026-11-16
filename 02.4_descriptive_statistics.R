library(tidyverse)
library(mice)

load("./data/MI_datlist_new.RData")
load("./data/imputations_new.RData")

# Get pooled mean and sd of aadx
imp.data <- complete(imp, "long")
aadx_mean <- with(imp.data, tapply(aadx, .imp, mean))
aadx_pooled_mean <- mean(aadx_mean)

aadx_sd <- with(imp.data, tapply(aadx, .imp, sd))
aadx_pooled_sd <- mean(aadx_sd)



aadx_mean_sex <- with(imp.data, tapply(aadx, list(.imp, sex), mean))
aadx_sd_sex <- with(imp.data, tapply(aadx, list(.imp, sex), sd))

mean_sex_0 <- mean(aadx_mean_sex[, "0"])
sd_sex_0 <- mean(aadx_sd_sex[, "0"])

mean_sex_1 <- mean(aadx_mean_sex[, "1"])
sd_sex_1 <- mean(aadx_sd_sex[, "1"])


cat(paste("Age at diagnosis mean:", aadx_pooled_mean),
    paste("Age at diagnosis sd:", aadx_pooled_sd),
    paste("Mean age at diagnosis boys:", mean_sex_0),
    paste("SD age at diagnosis boys:", sd_sex_0),
    paste("Mean age at diagnosis girls:", mean_sex_1),
    paste("SD age at diagnosis girls:", sd_sex_1))