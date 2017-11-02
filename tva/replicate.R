setwd("tva")
library(haven)
library(tidyverse)

# build data

## TVA dummies
tmp <- read.table("./original/data/tvacounties.txt")
summary(tmp)
tmp[, "tva"] <- rep_len(1, nrow(tmp))
colnames(tmp) <- c("fips", "tva")
tmp[, "fips"] <- as.numeric(tmp[, "fips"])


## read state-level data and take 1930 income variable
tmp7 <- read_dta("./original/data/state_level_data.dta")
tmp7 <- tmp7[, c("state", "pcp_income_30")]


## wage data, these are the correct manuf. and trade (retail+wholesale) wages
tmp76 <- read_dta("./original/data/tva1.dta")
tmp76 <- tmp76 %>%
    select(fips,
           starts_with("mwage"),
           starts_with("pcmwage"),
           starts_with("pctwage"),
           var88_county72,
           var89_county72)
tmp76 <- tmp76 %>% distinct(fips, .keep_all = TRUE)


## county-level data
tva_update <- read_dta("./original/data/tva_update.dta") %>%
    select(-starts_with("mwage"), -starts_with("twage"), -`_merge`)
with(tva_update, {
    table(duplicated(fips))
})

county_level <- tva_update %>%
    left_join(tmp76, by = "fips") %>%
    left_join(tmp, by = "fips")
county_level <- county_level %>%
    mutate(tva = ifelse(is.na(tva), 0, 1))

## merge on Fishback data
county_level <- county_level %>%
    filter(!is.na(county) & !is.na(state)) # 6 obs deleted
fishback <- read_dta("./original/data/fishback.dta")
db <- county_level %>%
    left_join(fishback, by = c("county", "state")) %>%
    # 101 obs deleted
    select(-N10)

## merge on a.g. land values
ag_land <- read_dta("./original/data/data.dta")
db <- db %>%
    left_join(ag_land, by = "fips")
dup_report <- function(df, col) {
    table(duplicated(df[col]))
}
dup_report(db, "fips")

## merge on TVA 'donut'
donut <- read_dta("./original/data/donut.dta")
db2 <- db %>%
    left_join(donut, by = "fips") # 2974 rows after join


## merge on correct employment variables
enrico_jobs <- read_dta("./original/data/enrico_jobs.dta")
db3 <- db2 %>%
    left_join(enrico_jobs, by = "fips")
dup_report(db3, "fips")

## merge on housing value/rents
housingvals <- read_dta("./original/data/housingvals.dta")
db4 <- db3 %>%
    left_join(housingvals, by = "fips")
dup_report(db4, "fips")

county62_1 <- read_dta("./original/data/county62_1.dta")
db5 <- db4 %>%
    left_join(select(county62_1, fips, var61_county62, var63_county62), by = "fips") %>%
    rename(medhsval60 = var61_county62,
           medrnt60 = var63_county62)
nrow(db5) # 2794 rows
dup_report(db5, "fips")

## merge on weather variables
jul <- read_dta("./original/data/JUL_MEAN_IDW200_365_1968_2002.dta")
jan <- read_dta("./original/data/JAN_MEAN_IDW200_365_1968_2002.dta")

db6 <- db5 %>%
    left_join(jul, by = "fips") %>%
    left_join(jan, by = "fips", suffix = c("_jul", "_jan"))
nrow(db6) # 2974 rows


## merge extra manuf. variables

