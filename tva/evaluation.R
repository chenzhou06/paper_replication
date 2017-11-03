library(tidyverse)
library(haven)
# build <- readRDS(". / build.rds") # missing area variable
build <- read_dta("./original/build.dta")

# New Variable
build %>%
    mutate(
        wage0 = pcmwage00 / (cpi0 / 100),
        wage20 = pcmwage20 / (cpi20 / 100),
        wage30 = pcmwage30 / (cpi30 / 100),
        wage40 = pcmwage39 / (cpi40 / 100),
        wage50 = pcmwage47 / (cpi50 / 100),
        wage60 = pcmwage58 / (cpi60 / 100),
        wage70 = pcmwage67 / (cpi70 / 100),
        wage80 = pcmwage82 / (cpi80 / 100),
        wage90 = pcmwage87 / (cpi90 / 100),
        wage2000 = pcmwage97 / (cpi2000 / 100)
    ) -> dfw # wage
select(dfw, starts_with("wage")) %>% summary

## Per capita wage in trade
dfw %>%
    mutate(
        twage30 = pctwage30 / (cpi30 / 100),
        twage40 = pctwage40 / (cpi40 / 100),
        twage50 = pctwage54 / (cpi50 / 100),
        twage60 = pctwage63 / (cpi60 / 100),
        twage70 = pctwage72 / (cpi70 / 100),
        twage80 = pctwage82 / (cpi80 / 100),
        twage90 = pctwage87 / (cpi90 / 100),
        twage2000 = pctwage97 / (cpi2000 / 100)
    ) -> dftw
dftw %>% select(starts_with("twage")) %>% summary

## Agricultural values
dfw %>%
    mutate(
        lnfaval0 = log(faval900 / (cpi0 / 100)),
        logfaval10 = log(faval910 / (cpi10 / 100)),
        logfaval20 = log(faval920 / (cpi20 / 100)),
        logfaval30 = log(faval930 / (cpi30 / 100)),
        logfaval40 = log(faval940 / (cpi40 / 100)),
        logfaval50 = log(faval950 / (cpi50 / 100)),
        logfaval60 = log(faval959 / (cpi60 / 100)),
        logfaval70 = log(faval1969 / (cpi70 / 100)),
        logfaval80 = log(faval1982 / (cpi80 / 100)),
        logfaval90 = log(faval1992 / (cpi90 / 100)),
        logfaval2000 = log(faval2002 / (cpi2000 / 100))
    ) %>%
    # Median family income
    mutate(
        logmedfaminc50  = log(medfaminc50/(cpi50/100)),
        logmedfaminc60  = log(medfaminc60/(cpi60/100)),
        logmedfaminc70  = log(medfaminc70/(cpi70/100)),
        logmedfaminc80  = log(medfaminc80/(cpi80/100)),
        logmedfaminc90  = log(medfaminc80/(cpi90/100)),
        logmedfaminc2000 = log(medfaminc2000/(cpi2000/100))
    ) %>%
    # farm production
    mutate(
        lnvfprod30   = log(vfprod30/(cpi30/100)),
        lnvfprod40   = log(vfprod40/(cpi40/100)),
        lnvfprod50   = log(vfprod50/(cpi50/100)),
        lnvfprod60   = log(vfprod60/(cpi60/100)),
        lnvfprod70   = log(vfprod70/(cpi70/100)),
        lnvfprod80   = log(vfprod80/(cpi80/100)),
        lnvfprod90   = log(vfprod90/(cpi90/100)),
        lnvfprod2000 = log(vfprod2000/(cpi2000/100))
    ) %>%
    # foreign born
    mutate(
        fb0  = fbwmtot00 + fbwftot00,
        fb10 = fbwtot10,
        fb20 = fbwmtot20 + fbwftot20,
        fb30 = fbwmtot + fbwftot,
        fbshr20 = fb20 / (wmtot20 + wftot20),
        fbshr30 = fb30 / (wmtot + wftot)
    ) %>%
    # housing values/rents
    rename(
        medrnt30 = medrnt30_NHGIS,
        medhsval30 = medhsval30_NHGIS,
        medhsval70 = var88_county72,
        medrnt70 = var89_county72
    ) -> dfh

# normalize housing values/rents, year 50 missing
dfh %>%
    mutate(
        medhsval30 = medhsval30 / (cpi30 / 100),
        medhsval40 = medhsval40 / (cpi40 / 100),
        # medhsval50 = medhsval50 / (cpi50 / 100),
        medhsval60 = medhsval60 / (cpi60 / 100),
        medhsval70 = medhsval70 / (cpi70 / 100),
        medhsval80 = medhsval80 / (cpi80 / 100),
        medhsval90 = medhsval90 / (cpi90 / 100),
        medhsval2000 = medhsval2000 / (cpi2000 / 100),
        medrnt30 = medrnt30 / (cpi30 / 100),
        medrnt40 = medrnt40 / (cpi40 / 100),
        # medrnt50 = medrnt50 / (cpi50 / 100),
        medrnt60 = medrnt60 / (cpi60 / 100),
        medrnt70 = medrnt70 / (cpi70 / 100),
        medrnt80 = medrnt80 / (cpi80 / 100),
        medrnt90 = medrnt90 / (cpi90 / 100),
        medrnt2000 = medrnt2000 / (cpi2000 / 100),
    ) -> dfhn

# drop some obs.
dfhn %>%
    select(-other60) %>%
    mutate(d = (b1_lnd01_county00 - area) / (b1_lnd01_county00 + area) / 2) %>%
    filter(abs(d) <= 0.03) %>%
    mutate(area = (b1_lnd01_county00 + area) / 2) -> dfnv
nrow(dfnv)
dfnv$area %>% psych::describe()