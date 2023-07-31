demog.dat <- function() {
   library(haven)
   x <- read_dta("C:/Users/Omar Lizardo/Google Drive/MISC DATA SOURCES/SSI-2012/SSI2012.dta", col_select = c("id", "nodipdeg", "hsged", "somcol", "aadeg", 
                                "bach", "ma", "docprof", "raceeth", 
                                "female", "age", "percclass", "incmid",
                                "region", "parented")) %>% 
      mutate(educ = case_when(nodipdeg == 1 ~ 1,
                              hsged == 1 ~ 2,
                              aadeg == 1 ~ 3,
                              somcol == 1 ~ 4,
                              bach == 1 ~ 5,
                              ma == 1 ~ 6,
                              docprof == 1 ~ 6),
             race = factor(raceeth, labels = c("Asian", 
                                               "Black", 
                                               "Hisp.", 
                                               "White", 
                                               "Mult.", 
                                               "Other")),
             age = factor(age, labels = c("18 – 19",
                                          "20 – 24",
                                          "25 – 29",
                                          "30 – 34",
                                          "35 – 39",
                                          "40 – 44",
                                          "45 – 49",
                                          "50 – 54",
                                          "55 – 59",
                                          "60 – 64",
                                          "65 – 69",
                                          "70 – 74",
                                          "75 – 79",
                                          "80+"), ordered = TRUE),
             subjclass = factor(percclass, labels = c("Lower",
                                                    "Working",
                                                    "Middle",
                                                    "Upper")),
             objclass = case_when(incmid == 5000 | incmid == 15000 ~ 1,
                                  incmid == 25000 | incmid == 35000 ~ 2, 
                                  incmid == 45000 | incmid == 55000 | 
                                  incmid == 65000 ~ 3,
                                  incmid == 75000 | incmid == 85000 | 
                                  incmid == 95000 ~ 4,
                                  incmid == 105000 | incmid == 115000 | 
                                  incmid == 125000 | incmid == 135000 |
                                  incmid == 145000 | incmid == 150000 |
                                  incmid == 165000 | incmid == 175000 |
                                  incmid == 185000 | incmid == 195000 |
                                  incmid == 212500 | incmid == 237500 ~ 5),
             gender = factor(female, labels = c("Men", "Women"))) %>%
      mutate(age.n = as.numeric(age), age.n2 = as.numeric(age)^2) %>% 
      mutate(objclass = factor(objclass, labels = c("Poor", "Working", "Middle", "Upper Middle", "High Income"))) %>% 
      dplyr::select(c("id", "educ", "race", "gender", 
                      "age.n", "age.n2", "subjclass", "objclass"))
   return(x)
}