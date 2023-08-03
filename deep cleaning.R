
library(dplyr)
library(tidyr)
library(purrr)
library(psych)

#birth <- read.csv("data/births.csv")

births_2013 <- read.csv("/Users/brenna/Downloads/STATA Ready Excel-selected/STATA Ready births 2013+.csv")
births_2016 <- read.csv("/Users/brenna/Downloads/STATA Ready Excel-selected/STATA Ready births 2016+.csv")

births <- rbind(births_2013, births_2016)

names(births)

test <- births %>%
  mutate(MomRaceAmIndian = ifelse(MomRaceAmIndian == "", NA, "aian")) %>%
  mutate(MomRaceWhite = ifelse(MomRaceWhite == "", NA, "white")) %>%
  mutate(MomRaceBlack = ifelse(MomRaceBlack == "", NA, "black")) %>%
  mutate(MomRaceChinese = ifelse(MomRaceChinese == "", NA, "chinese")) %>%
  mutate(MomRaceJapanese = ifelse(MomRaceJapanese == "", NA, "japanese")) %>%
  mutate(MomRaceFilipino = ifelse(MomRaceFilipino == "", NA, "filipino")) %>%
  mutate(MomRaceHawaiian = ifelse(MomRaceHawaiian == "", NA, "hawaiian")) %>%
  mutate(MomRaceAsianIndian = ifelse(MomRaceAsianIndian == "", NA, "asian_indian")) %>%
  mutate(MomRaceOtherAsian = ifelse(MomRaceOtherAsian == "", NA, "other_asian")) %>%
  mutate(MomRaceGuamanian = ifelse(MomRaceGuamanian == "", NA, "guamanian")) %>%
  mutate(MomRaceKorean = ifelse(MomRaceKorean == "", NA, "korean")) %>%
  mutate(MomRaceSamoan = ifelse(MomRaceSamoan == "", NA, "samoan")) %>%
  mutate(MomRaceVietnamese = ifelse(MomRaceVietnamese == "", NA, "vietnamese")) %>%
  mutate(MomRacePacIslander = ifelse(MomRacePacIslander == "", NA, "pac_islander")) %>%
  mutate(MomRaceTongan = ifelse(MomRaceTongan == "", NA, "tongan")) %>%
  mutate(MomRaceOther = ifelse(MomRaceOther == "", NA, "other")) %>%
  mutate(MomRaceUnknown = ifelse(MomRaceUnknown == "", NA, "unknown"))

#names(test) <- tolower(names(test))

table(test$MomRaceOtherSpec1)
table(test$MomRaceOtherSpec2)
table(test$MomRaceAsianSpec1)
table(test$MomRaceAsianSpec2)

test[which(test$MomRaceAsianIndian == "asian_indian" &
             test$MomRaceOtherAsian == "other_asian"), ]


x <- unite(test, col = "mom_race", c("MomRaceAmIndian", "MomRaceWhite", "MomRaceBlack",
                                     "MomRaceChinese", "MomRaceJapanese", "MomRaceFilipino",
                                     "MomRaceHawaiian", "MomRaceAsianIndian", "MomRaceOtherAsian",
                                     "MomRaceGuamanian", "MomRaceKorean", "MomRaceSamoan",
                                     "MomRaceVietnamese", "MomRacePacIslander", "MomRaceTongan",
                                     "MomRaceOther", "MomRaceUnknown"), sep=' ')

x$mom_race <- gsub("NA ", "", x$mom_race)
x$mom_race <- gsub(" NA", "", x$mom_race)

table(x$mom_race == "NA")

check <- x[which(x$mom_race == "NA"), ] # fix em

all_races <- c("chinese", "japanese", "filipino", "korean", "vietnamese", "other_asian", "asian_indian",
               "guamanian", "hawaiian", "samoan", "tongan", "pac_islander", "white",
               "black", "unknown", "aian", "other")
# source: https://www.census.gov/quickfacts/fact/note/US/RHI625222#:~:text=OMB%20requires%20five%20minimum%20categories,report%20more%20than%20one%20race.
asian_races <- c("chinese", "japanese", "filipino", "korean", 
                 "vietnamese", "other_asian", "asian_indian")
nhpi_races <- c("guamanian", "hawaiian", "samoan", "tongan", 
                "pac_islander")
non_asian_races <- setdiff(all_races, asian_races)
non_nhpi_races  <- setdiff(all_races, nhpi_races)


table(grepl(asian_races, x$mom_race) & !grepl("white", x$mom_race))

x$multiple_same <- case_when(x$mom_race %in% grep(paste(asian_races, collapse="|"), 
                                               x$mom_race, value=TRUE) &
                            !x$mom_race %in% grep(paste(non_asian_races, collapse="|"), 
                                                  x$mom_race, value=TRUE) ~ "multiple_asian",
                            x$mom_race %in% grep(paste(nhpi_races, collapse="|"), 
                                                 x$mom_race, value=TRUE) &
                              !x$mom_race %in% grep(paste(non_nhpi_races, collapse="|"), 
                                                    x$mom_race, value=TRUE) ~ "multiple_nhpi")

x$multiple_same <- ifelse(grepl(" ", x$mom_race), x$multiple_same, NA)

#table(x$multiple_same)

table(x$mom_race, x$multiple_same)

x <- x %>%
  mutate(mom_race_simple = ifelse(grepl(" ", mom_race),# | grepl("multiple", multiple_same), 
                                  "multiple_races", mom_race)) %>%
  mutate(mom_race_simple = ifelse(!is.na(multiple_same), multiple_same,
                                  mom_race_simple))%>%
  mutate(mom_race_census = case_when(mom_race_simple %in% c("multiple_asian", "asian_indian",
                                                            "chinese", "filipino", "japanese",
                                                            "korean", "other_asian",
                                                            "vietnamese") ~ "asian",
                                     mom_race_simple %in% c("guamanian", "hawaiian", "samoan",
                                                            "multiple_nhpi", "pac_islander",
                                                            "tongan") ~ "nhpi",
                                     mom_race_simple == "aian" ~ "aian",
                                     mom_race_simple == "black" ~ "black",
                                     mom_race_simple == "multiple_races" ~ "multiple_races",
                                     mom_race_simple %in% c("other", "NA", "unknown") ~ "other",
                                     mom_race_simple == "white" ~ "white"))

table(x$mom_race_simple)
table(x$mom_race_census)
table(is.na(x$multiple_same), x$mom_race)

round(prop.table(table(x$mom_race_census))*100, 3)

names(x)

table(x$MomHispanicOrigin, x$mom_race_simple)

x <- x %>%
  mutate(hispanic = ifelse(MomHispanicOrigin == 1, "hispanic", "not hispanic")) %>%
  mutate(mom_race_ethn = ifelse(hispanic == "hispanic", "hispanic", mom_race_census))

table(x$mom_race_ethn)
names(x)
summary(x$UnplannedOperation)



# missing values often coded as *; as.numeric() will convert
test <- as.factor(as.numeric(x$PrevPregStillborn))
summary(test)
table(x$TobaccoPrior) # lots of numbers
table(x$TobaccoTrimester1) # ""
table(x$TobaccoTrimester2) # ""
table(x$TobaccoTrimester3) # ""
table(x$SourceOfPayment) # ~1-9, C
table(x$RenalOtherUrinarySpecify) # many conditions, ICD?
table(x$PrevTerminations) # 9-28
table(x$PrevPregStillborn) # 0-5
table(x$PrevPregNowLiving) # 0-17
table(x$PrevPregNowDead) # 
table(x$PrevPregMultiple)
table(x$OtherMuscSpecify)
table(x$OtherLimbSpecify)
table(x$OtherCongAnomSpecify)
table(x$NumberVisits)
table(x$NumberOfCSections)
table(x$NICU)
x$MomWeightPrior
table(x$MomWeightGainLoss)
table(x$MomWeightAtDelivery)
table(x$MomHeightInches)
table(x$MomHeightFeet)


numeric_fx <- function(x){
  x <- as.numeric(x)
}
factor_fx <- function(x){
  x <- as.numeric(x)
}


x <- x %>%
  mutate_at(vars(MomWeightPrior) = as.numeric(MomWeightPrior)) %>%



table(x$MomCountryOfBirthFips)
table(x$MomCountryOfBirth)
table(x$MomAge)
table(x$MalfGenSpecify)
table(x$LimbReductionSpecify)
table(x$IllicitDrugUseSpecify)





small_dat <- 


# birth weight
x$BirthWeightGrams <- as.numeric(x$BirthWeightGrams)
summary(x$BirthWeightGrams)

hist(x$BirthWeightGrams)


x$lbw <- ifelse(x$BirthWeightGrams <= 2500, "low", "not low")
table(x$lbw)

# aside: which of these are unreasonable
v_low <- x %>%
  filter(BirthWeightGrams < 1000)
hist(v_low$Gestation)
corr.test(x$BirthWeightGrams, x$Gestation)

v_low[which(v_low$Gestation == 40), ]




table(x$mom_race_simple)
#x <- x %>%
  #mutate(case_when())

table(grepl(" ", x$mom_race))


table(test_2$MomRaceUnknown)
table(test_2$MomRaceKorean)
table(test_2$MomRaceSamoan)
table(test_2$MomRaceVietnamese)
table(test_2$MomRacePacIslander, test_2$MomRaceIslanderSpec1)
table(test_2$MomRaceTongan)
table(test_2$MomRaceOther, test_2$MomHispanicOrigin)
table(test_2$MomHispanicOrigin)
table(test_2$MomRaceTongan)
table(test_2$MomRaceTongan)

names(test_2)








# source: https://en.wikipedia.org/wiki/List_of_federally_recognized_tribes_in_the_contiguous_United_States



test_2 <- test %>%
  mutate(mom_tribe1 = case_when(MomRaceTribeSpec1 %in% c("Abnaki") ~ "Abenaki",
                                MomRaceTribeSpec1 %in% c("Adabaskan") ~ "Athabascan",
                                MomRaceTribeSpec1 %in% c("Arikioa") ~ "Arikara",
                                       MomRaceTribeSpec1 %in% c("Astic") ~ "Aztec",
                                       MomRaceTribeSpec1 %in% c("aTHAPASKAN") ~ "Athapaskan",
                                       MomRaceTribeSpec1 %in% c("Blackfoot cherokee", "Blackfoot", 
                                                                "Blackfloot Grossventre Assinboine",
                                                                "Black Foot") ~ "Blackfeet Tribe",
                                       MomRaceTribeSpec1 %in% c("Carry the Kettle") ~ "Carry the Kettle Nakoda Nation",
                                       MomRaceTribeSpec1 %in% c("Catawba Indian Nation", "Catawba") ~ "Catawba Nation",
                                       MomRaceTribeSpec1 %in% c("Cheyenne sioux river tribe",
                                                                "Cheyenne river sioux", "Cheyenne River",
                                                                "Cheyenne") ~ "Cheyenne River Sioux Tribe",
                                       MomRaceTribeSpec1 %in% c("Chickisaw", "Chickashaw",
                                                                "Chickasaw chalktaw cheeroke",
                                                                "Chickasaw", "Chalktaw Chickasaw") ~ "Chickasaw Nation",
                                       MomRaceTribeSpec1 %in% c("Choctaw and Cayuga", "Choctaw",
                                                                "Choctaw and Cherokee") ~ "Choctaw Nation of Oklahoma",
                                       MomRaceTribeSpec1 %in% c("Chre", "Chee") ~ "Cher-Ae Heights Indian",
                                       MomRaceTribeSpec1 %in% c("Chuckchansi") ~ "Picayune Rancheria of Chukchansi",
                                       MomRaceTribeSpec1 %in% c("Chumash") ~ "Santa Ynez Band of Chumash",
                                       MomRaceTribeSpec1 %in% c("Colville Confederated Tribe") ~ "Confederated Tribes of the Colville",
                                       MomRaceTribeSpec1 %in% c("Commanche Nation", "Comanche") ~ "Comanche Nation",
                                       MomRaceTribeSpec1 %in% c("Confederated Tribes of Goshute Res") ~ "Confederated Tribes of the Goshute Reservation",
                                       MomRaceTribeSpec1 %in% c("Crow Creek Sioux", "Crow Indian",
                                                                "Crow", "Creek") ~ "Crow Creek Sioux",
                                       MomRaceTribeSpec1 %in% c("Flathead", "Confederated Salish") ~ "Confederated Salish & Kootenai Tribes",
                                       MomRaceTribeSpec1 %in% c("Fort McDowell Yauapai Nation") ~ "Fort McDowell Yavapai Nation",
                                       MomRaceTribeSpec1 %in% c("Fortbelknap Montana") ~ "Fort Belknap Indian",
                                       MomRaceTribeSpec1 %in% c("Ft berthold", 
                                                                "Three affiated tribes") ~ "Three Affiliated Tribes of the Fort Berthold",
                                       MomRaceTribeSpec1 %in% c("Ft McDermitt Paiute") ~ "Fort McDermitt Paiute and Shoshone Tribes",
                                       MomRaceTribeSpec1 %in% c("Gitxan") ~ "Gitxsan",
                                       MomRaceTribeSpec1 %in% c("Grand Ron Rogue River Chinook", "CTGR") ~ "Confederated Tribes of the Grand Ronde",
                                       MomRaceTribeSpec1 %in% c("Hida", "Haida", "Apatchi", "Hipatchi",
                                                                "Apache cherokee", "Apache", "Adache") ~ "Apache",
                                       MomRaceTribeSpec1 %in% c("Hopi  Chemehuevi", "Hopi") ~ "Hopi",
                                       MomRaceTribeSpec1 %in% c("Inuet") ~ "Inuit",
                                       MomRaceTribeSpec1 %in% c("Iroquois", "Iroqois", "Irioquois") ~ "Iroquois",
                                       MomRaceTribeSpec1 %in% c("Kaibab Paiute Tribe", "Kaibab Band of Paiute",
                                                                "Kaibab paiute", "Kaibab band of Paiute",
                                                                "Kaibab Band of Piaute") ~ "Kaibab Band of Paiute Indians",
                                       MomRaceTribeSpec1 %in% c("Kato") ~ "Cahto Tribe",
                                       MomRaceTribeSpec1 %in% c("Kaurk tribe", "Karuk") ~ "Karuk Tribe",
                                       MomRaceTribeSpec1 %in% c("Ketowah-Cherokee") ~ "United Keetoowah Band of Cherokee Indians",
                                       MomRaceTribeSpec1 %in% c("Kialeale") ~ "Kialegee",
                                       MomRaceTribeSpec1 %in% c("Klalum") ~ "Jamestown S'Klallam Tribe",
                                       MomRaceTribeSpec1 %in% c("LaJolla Luiseno") ~ "La Jolla Band of Luiseno Indians",
                                       MomRaceTribeSpec1 %in% c("Lakota-Sioux", "Lakota Sioux",
                                                                "Lakota oglala sioux", "Lakota") ~ "Oglala Sioux Tribe",
                                       MomRaceTribeSpec1 %in% c("Miwok", "Lost Cherokee",
                                                                "Cherokee Talequan", "Cherokee souix",
                                                                "Cherokee Nation of Oklahoma", "Cherokee Indian",
                                                                "Cherokee and Sioux", "Cherokee and Creek",
                                                                "Cherokee", "cherokee", "Cheriket", "Cherikee",
                                                                "Cherakawa", "Cheorke") ~ "Cherokee Nation",
                                       MomRaceTribeSpec1 %in% c("Miwok", "Miwok wilton rancheria") ~ "Miwok",
                                       MomRaceTribeSpec1 %in% c("Mohawk") ~ "Saint Regis Mohawk Tribe",
                                       MomRaceTribeSpec1 %in% c("Mojave Cheerokee") ~ "Fort Mojave Indian Tribe",
                                       MomRaceTribeSpec1 %in% c("Muskogee") ~ "Muscogee Nation",
                                       MomRaceTribeSpec1 %in% c("Not enrolled - Chipawaa Cree",
                                                                "Chipwee Cree", "Chippewa", "Chipewae",
                                                                "CHIPAWA CREE", "Chipava", "Chipwee Cree",
                                                                "Chippewa-turtle mountain band") ~ "Chippewa Cree",
                                       MomRaceTribeSpec1 %in% c("Nez perce", "Nez Pierce") ~ "Nez Perce",
                                       MomRaceTribeSpec1 %in% c("Northern Arapahoe", "Northern Arapacho",
                                                                "northeren Arapaho and Northren Ute",
                                                                "Arapano", "Arapahoe", "Arapaho") ~ "Northern Arapaho Tribe",
                                       MomRaceTribeSpec1 %in% c("Northern Cheyenne Sioux", "Northern Cheyenne",
                                                                "Northern cheyenne", "Northern Cheyene") ~ "Northern Cheyenne Tribe",
                                       MomRaceTribeSpec1 %in% c("Northern Navajo Nation", "Navajo-Tangle",
                                                                "Navajo-where the two waters meet", 
                                                                "Navajo- Ute", "Navajo Tribe", "Navajo tribe",
                                                                "Navajo Nation", "Navajo Indian", "Navajo Apache",
                                                                "Navajo and ute", "Navajo and Cree", "Navajo  Ute",
                                                                "Navajo  Nation", "NAVAJO", "Navajo", "navajo",
                                                                "Navaho", "Navado", "Native American Navajo",
                                                                "Mother half Navajo and half Hispanic", "Dine") ~ "Navajo Nation",
                                       MomRaceTribeSpec1 %in% c("Northwestern shoshone nation",
                                                                "Northwestern band of shoneshone",
                                                                "Northwest Shoshone", "North western band shoshone",
                                                                "North western band of shosone") ~ "Northwestern Band of the Shoshone Nation",
                                       MomRaceTribeSpec1 %in% c("Oglala", "Ogwala Lakuta", "Oglala Lakota",
                                                                "Oglala Sioux", "Ogalala Souix") ~ "Oglala Sioux",
                                       MomRaceTribeSpec1 %in% c("Ojibwe", "Ojibway", "Ojibwa") ~ "Ojibwe",
                                       MomRaceTribeSpec1 %in% c("Omaha", "Macy Nabraska Omaha indian") ~ "Macy Nabraska Omaha indian",
                                       MomRaceTribeSpec1 %in% c("Oneida Tribe", "Oneida") ~ "Oneida Tribe",
                                       MomRaceTribeSpec1 %in% c("Osage Indian", "Osage") ~ "Osage",
                                       MomRaceTribeSpec1 %in% c("Paiute Indian Tribe of Utah",
                                                                "Piaute", "Southern Paiute Tribe of Utah",
                                                                "Southern Paiute", "Plaute", "Paiute",
                                                                "Ute Piute") ~ "Paiute Indian Tribe of Utah",
                                       MomRaceTribeSpec1 %in% c("Paiute Shoshone", "Duck valley shoshone") ~ "Shoshone-Paiute Tribes of the Duck Valley Reservation",
                                       MomRaceTribeSpec1 %in% c("Pima") ~ "Salt River Pima-Maricopa",
                                       MomRaceTribeSpec1 %in% c("Pit river tribe") ~ "Pit River Tribe",
                                       MomRaceTribeSpec1 %in% c("PoncaKawtaw", "Kaw Tribe", "Kaw") ~ "Kaw Nation",
                                       MomRaceTribeSpec1 %in% c("Potowatomi", "Potawatomi",
                                                                "Potawatami") ~ "Potawatomi",
                                       MomRaceTribeSpec1 %in% c("Pueblo", "Pueblo of Zane",
                                                                "Peublo of Zane", "Laguna") ~ "Pueblo",
                                       MomRaceTribeSpec1 %in% c("Quenhagak") ~ "Kwinhagak",
                                       MomRaceTribeSpec1 %in% c("Rosebud Sioux Tribe", "Rosebud Souix",
                                                                "Rosebud Sue", "Rosebud souix",
                                                                "Rosebud") ~ "Rosebud Sioux Tribe",
                                       MomRaceTribeSpec1 %in% c("Seneca") ~ "Seneca Nation of Indians",
                                       MomRaceTribeSpec1 %in% c("Shoshone", "Shashonie",
                                                                "shoshone", "Shishoni",
                                                                "Shashone") ~ "Shoshone",
                                       MomRaceTribeSpec1 %in% c("Shosone Bannock Tribe",
                                                                "Shoshone-Bannock Tribes",
                                                                "Shoshone-Bannock") ~ "Shoshone-Bannock Tribes",
                                       MomRaceTribeSpec1 %in% c("Siox", "Souix", "Sioux Cherokee",
                                                                "Sioux") ~ "Sioux or Oceti Sakowin",
                                       MomRaceTribeSpec1 %in% c("Sisston Wapton", "Sisstoh Whapton",
                                                                "Sisseton wahpeton ouate",
                                                                "Sisseston Whapenton Sioux") ~ "Sisseton-Wahpeton Oyate",
                                       MomRaceTribeSpec1 %in% c("Skagoan tribe") ~ "Upper Skagit Indian Tribe",
                                       MomRaceTribeSpec1 %in% c("Skull Valley band of Goshutes",
                                                                "Goshute tribe", "Goshute Hualapai Shoshone",
                                                                "Goshute") ~ "Skull Valley Band of Goshute Indians",
                                       MomRaceTribeSpec1 %in% c("Souix Assinaboine", "Ft peck assiniboine",
                                                                "Fort Peck", "Fort Peck Tribes",
                                                                "Assiniboine Sioux", "Assiniboine") ~ "Assiniboine and Sioux Tribes",
                                       MomRaceTribeSpec1 %in% c("Tarahumana") ~ "Tarahumara",
                                       MomRaceTribeSpec1 %in% c("Standing Rock Sioux Tribe",
                                                                "Standing Ruck Sioux") ~ "Standing Rock Sioux Tribe",
                                       MomRaceTribeSpec1 %in% c("Tigua") ~ "Ysleta del Sur Pueblo",
                                       MomRaceTribeSpec1 %in% c("Tlingit", "Tlinget", "Thinglit",
                                                                "T Lingit") ~ "Tlingit Tribe",
                                       MomRaceTribeSpec1 %in% c("Tohono Oodam", "Tohnon Oodham",
                                                                "Thono Odham Nation") ~ "Tohono O'odham Nation",
                                       MomRaceTribeSpec1 %in% c("Uclulet BS Canada") ~ "Ucluelet First Nation",
                                       MomRaceTribeSpec1 %in% c("Unkown", "Unspecified", "Unknown tribe",
                                                                "Unknown  adopted", "Unknown", "Uknown",
                                                                "Outtay", "None given", "Not Specified", 
                                                                "Not specified", "None", "Left blank",
                                                                "not provided", "Not listed", "Not enrolled",
                                                                "Native American", "Native American uknown tribe",
                                                                "Native american", "Native Ameican", "Nana Corporation",
                                                                "Indian", "Fairchild", "Didn't specify", "Did not list",
                                                                "Did not identify", "Dorocha canadian tribe",
                                                                "AMERICAN INDIAN", "American Indian", "American indian",
                                                                "aMERICAN INDIAN", "American Indain", "American indain",
                                                                "American idian", "Alaska native", "*") ~ "Unknown",
                                       MomRaceTribeSpec1 %in% c("Ute Indian Tribe", "Ute", "Ute indian tribe",
                                                                "Ute Ouray", "Ute Ouray", "Ute tribe", "Ute Tribe",
                                                                "Ute northern uiintah basin", "Unitah",
                                                                "Ute", "Uintah Band", "Uintah And Ouray tribe",
                                                                "Uintah", "Northren Ute Tribe", "Northren Ute",
                                                                "Northren Ute Indian Tribe", "Northren ute",
                                                                "Northern Ute Tribe Uintah and Ouray", "Northern Ute",
                                                                "Northern Ute Tribe", "Northern Ute Indian Tribe",
                                                                "Northern ute", "northern Ute", "Northern Utah Tribe",
                                                                "Northern Tribe", "Northemute", "Native ute", 
                                                                "Native Northren Ute", "American indian northren ute tribe") ~ "Ute Indian Tribe of the Uintah & Ouray Reservation",
                                       MomRaceTribeSpec1 %in% c("Ute Mountain Ute Tribe", "Ute Mountain Tribe",
                                                                "Southren Ute Tribe", "Mountain Ute") ~ "Ute Mountain Ute Tribe",
                                       MomRaceTribeSpec1 %in% c("Utu gwaiutu painte tribe") ~ "Utu Utu Gwaitu Paiute Tribe",
                                       MomRaceTribeSpec1 %in% c("Western Temoke", "Westren Temoke Shoshone",
                                                                "Western Shosone", "Western Shoshonee",
                                                                "Temoke Shoshone", "Teomake", "Te-Moak Western Shoshone",
                                                                "Te Moak") ~ "Te-Moak Tribe of Western Shoshone Indians of Nevada",
                                       MomRaceTribeSpec1 %in% c("White Earth") ~ "Minnesota Chippewa Tribe",
                                       MomRaceTribeSpec1 %in% c("Wichita") ~ "Wichita and Affiliated Tribes",
                                       MomRaceTribeSpec1 %in% c("Yakama") ~ "Yakama Nation",
                                       MomRaceTribeSpec1 %in% c("Yankton Sioux") ~ "Yankton Sioux Tribe",
                                       MomRaceTribeSpec1 %in% c("Yaqui") ~ "Pascua Yaqui Tribe",
                                       MomRaceTribeSpec1 %in% c("Yavapai apache") ~ "Yavapai-Apache Nation",
                                       MomRaceTribeSpec1 %in% c("Yokut Tule River Reservation CA") ~ "Tule River Indian Tribe",
                                       MomRaceTribeSpec1 %in% c("Yupik", "Yupik Eskimo") ~ "Yupik",
                                       MomRaceTribeSpec1 %in% c("Zuni", "Zuni Pueblo") ~ "Zuni",
                                TRUE ~ MomRaceTribeSpec1))

table(is.na(test_2$mom_tribe1))
table(test$MomRaceTribeSpec1 == "")
table(test_2$mom_tribe1)
table(test$MomRaceTribeSpec2)

table(test_2$mom_tribe1 == "", is.na(test_2$MomRaceAmIndian))








table(x$MomRaceTribeSpec1 == "",
      x$MomRaceTribeSpec2 == "")

names(test)


test <- test %>%
  mutate(mom_race = case_when(MomRaceAmIndian == 1 ~ "aian"))

for(i in 165:194) {
  print(table(test[, c(i)]))
}

table(test[, c(165:194)])



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

