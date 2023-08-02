
library(dplyr)
library(tidyr)
library(purrr)

birth <- read.csv("data/births.csv")
names(birth)

test <- birth %>%
  mutate(MomRaceAmIndian = ifelse(MomRaceAmIndian == "", NA, 1)) %>%
  mutate(MomRaceWhite = ifelse(MomRaceWhite == "", NA, 1)) %>%
  mutate(MomRaceBlack = ifelse(MomRaceBlack == "", NA, 1))


test <- test %>%
  mutate(mom_race = case_when(MomRaceAmIndian == 1 ~ "aian"))

table(test[, c(165:194)] == 1)

MomRaceTribeSpec2

table(test$MomRaceTribeSpec1)
table(test$MomRaceTribeSpec2)

table(test$MomRaceAmIndian)
table(test$MomRaceWhite)
table(test$MomRaceBlack)

test <- test %>% mutate(mycol = coalesce("MomRaceAmIndian",
                                  "MomRaceWhite",
                                  "MomRaceBlack")) %>%
  select(MomID, mycol)

table(test$MomRaceAmIndian)



birth %>%
  pivot_longer(cols =  names_to = "mom_race")




#data[-1] <- 
test <- lapply(names(birth[c(-1:-163, -195:-226)]), 
               function(x) ifelse(birth[x] == 1, x, NA))

#birth[-1] <-
test <- as.data.frame(ifelse(birth[c(-1:-163, -195:-226)] == "X", 
                             names(birth[c(-1:-163, -195:-226)])[col(birth[c(-1:-163, -195:-226)])], NA))
summary(test)

test %>% 
  mutate(race = coalesce(1:31))


m_race <- cbind(test, mycol = unlist(test[-1]))

unite(test, col = "mom_race", c(1:31), sep='-')

paste(test[, 1:31])

summary(birth[c(-1:-163, -195:-226)] == "X")

