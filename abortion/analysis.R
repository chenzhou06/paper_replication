library(haven)
library(dplyr)
library(plm)

cleanps <- "abortion/original/Data/2 Clean Data/CPS for Analysis.dta"
cleannsfg <- "abortion/original/Data/2 Clean Data/NSFG for Analysis.dta"

df <- read_dta(cleanps)

df %>%
    filter(ageyrs >= 22 & yob >= 1935 & yob <= 1958) %>%
    filter(weight != 0) %>%
    mutate(evermarried = 1 - nevermarried) %>%
    filter(evermarried != 0 | femstat != 1 | insupp != 1) %>%
    filter(evermarried != 1 | femstat < 2 | femstat > 3 | insupp != 1) -> sampledf

sampledf %>%
    filter(nevermarried == 1 & !is.na(age_1mar)) # should be zero obs.


# time trend
# skip

# policy interactions
sampledf2 <- sampledf %>%
    mutate(pxal18 = epillconsent18 * eabortionlegal18,
           pxac18 = epillconsent18 * eabortionconsent18,
           pxal19 = epillconsent19 * eabortionlegal19,
           pxac19 = epillconsent19 * eabortionconsent19)



# RHS controls
stateyear <- "factor(state) + factor(yob)"
control0 <- "black + other + hispanic"
control18 <- paste0(control0, " + ",
                    "eabortion_reformlegal18 + eepl18 + erd18 + enofault18")
control19 <- paste0(control0, " + ",
                    "eabortion_reformlegal19 + eabortion_reformconsent19 +eepl19 + erd19 + enofault19")

# results table 2
# pill policy
# subdfpill <- sampledf2 %>% select(firstbirth18, epilllegal18, epillconsent18, black, other, hispanic, weight, state, yob)
# mpill <- plm(as.formula(paste0("firstbirth18", " ~ ", "epilllegal18 + epillconsent18", "+", control0, "+ weight")),
#              data = subdfpill,
#              index = c("state", "yob"),
#              model = "between",
#              effect = "twoways")
subsample <- sample_frac(sampledf2, 0.5)
mpill <- lm(as.formula(
                paste0("firstbirth18", " ~ ",
                       "epilllegal18 + epillconsent18", "+",
                       control0, "+", stateyear,
                       "+", "factor(state) : factor(yob)")),
            data = subsample,
            weights = subsample$weight)