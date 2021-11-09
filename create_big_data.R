

# get names of all datasets
ecodatas <- data(package = "ecodata")
head(ecodatas)

names <- ecodatas$results[,3]
names

# fix seabird_mab
n <- which(names == "seabird_mab (seabird_MAB)")
names[n] <- "seabird_mab"

# load ecodata so datasets can be called
library(ecodata)

# make empty list, loop through and add data
big_data <- list()
n <- 1
for(i in names){
  dat <- get(i)
  big_data[[n]] <- dat
  n <- n + 1
}

# add data names
names(big_data) <- names

# save
saveRDS(big_data, file = "test_big_data.RDS")

# did it work?

test <- readRDS("test_big_data.RDS")
str(test)
