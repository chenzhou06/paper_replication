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
tmp7 <- read_dta("./original/data/state_level_data.dta") %>%
    select(state, pcp_income_30) %>%
    mutate(state = as.integer(state))


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
manuf <- read_dta("./original/data/data2.dta")
tva <- read_dta("./original/data/tva.dta")
tva3 <- read_dta("./original/data/tva3.dta")
db7 <- db6 %>%
    left_join(manuf, by = "fips") %>%
    left_join(tva, by = "fips") %>%
    left_join(tva3, by = "fips")
nrow(db7)

row_max <- function(...) {
    m <- data.frame(...)
    apply(m, 1, max)
}
## proposed authorities
auth <- read_dta("./original/data/autorities/data.dta")
db8 <- db7 %>% left_join(auth, by = "fips") %>%
    mutate(aut4 = ifelse(row_max(aut_euclidean1, aut_euclidean2, aut_euclidean3, aut_centr4, aut_euclidean5, aut_euclidean6) == 1,
                        1, 0))
nrow(db8)
with(db8, {
    print(table(aut_euclidean1))
    print(table(aut_euclidean2))
    print(table(aut_euclidean3))
    print(table(aut_centr4))
    print(table(aut_euclidean5))
    print(table(aut_euclidean6))
    print(table(aut4))
    print(table(tva))
})

db8 %>%
    select(-starts_with("aut_eucl"),
           -starts_with("aut_cent")) -> db9

## clean up and definition
### state and region
mk_state_table <- function() {
    # make state location table
    ne <- "northeast"
    mw <- "midwest"
    s <- "south"
    w <- "west"
    tmp <- rbind(
        c(9, ne),
        c(23, ne),
        c(25, ne),
        c(33, ne),
        c(44, ne),
        c(50, ne),
        c(34, ne),
        c(36, ne),
        c(42, ne),
        c(17, mw),
        c(18, mw),
        c(26, mw),
        c(39, mw),
        c(55, mw),
        c(19, mw),
        c(20, mw),
        c(27, mw),
        c(29, mw),
        c(31, mw),
        c(38, mw),
        c(46, mw),
        c(10, s),
        c(11, s),
        c(12, s),
        c(13, s),
        c(24, s),
        c(37, s),
        c(45, s),
        c(51, s),
        c(54, s),
        c(1, s),
        c(2, s),
        c(28, s),
        c(47, s),
        c(5, s),
        c(22, s),
        c(40, s),
        c(48, s),
        c(4, w),
        c(8, w),
        c(16, w),
        c(30, w),
        c(32, w),
        c(35, w),
        c(49, w),
        c(56, w),
        c(2, w),
        c(6, w),
        c(15, w),
        c(41, w),
        c(54, w)
    )
    tmp <- cbind(tmp, rep(1, nrow(tmp)))
    colnames(tmp) <- c("state", "loc", "val")
    as.data.frame(tmp) %>%
        mutate(state = as.integer(state),
               val = as.integer(val)) %>%
        spread(loc, val, fill = 0)
}

db9 %>%
    mutate(state = as.integer(fips/1000)) %>%
    filter(!(state %in% c(51, 52, 2, 3, 15))) %>%
    left_join(mk_state_table(), by = "state") %>%
    mutate(region = ifelse(northeast == 1, 1, 0)) %>%
    mutate(region = ifelse(midwest == 1, 2, 0)) %>%
    mutate(region = ifelse(south == 1, 3, 0)) %>%
    mutate(region = ifelse(west == 1, 4, 0)) -> db10
table(db10$region) # WARNING: problematic

## CPI
db10 %>%
    mutate(cpi890 = 5,
           cpi0 = 7,
           cpi10 = 9,
           cpi20 = 20,
           cpi30 = 16.7,
           cpi40 = 14,
           cpi50 = 24.1,
           cpi60 = 29.6,
           cpi70 = 38.8,
           cpi80 = 82.4,
           cpi90 = 130.7,
           cpi2000 = 172.2) -> db11

## merge the state-level variable
db11 %>%
    left_join(tmp7, by = "state") %>%
    drop_na(tva) ->
    db12
nrow(db12)

## drop donut and counties with missing latitude/longitude
# TODO:
# db12 %>% filter(border_county != 1) %>%
#     drop_na(latitude, longitud) %>%
#     filter(pop0 >= 1000 &)