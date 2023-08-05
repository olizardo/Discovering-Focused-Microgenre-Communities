ed.labs <- c("< High School", "High School/GED", "Associate", "Some College", "Bachelor's", "Postgraduate")
demog.dat <- read_dta("C:/Users/Omar Lizardo/Google Drive/MISC DATA SOURCES/SSI-2012/SSI2012.dta") %>% 
    dplyr::select(c("id", "age", "agemid", "female", "raceeth", 
                    "nodipdeg", "hsged", "somcol", "aadeg", "bach", "ma", 
                    "docprof", "incmid", "income", "percclass")) %>% 
    as_factor() %>% 
    mutate(educ = if_else(nodipdeg == "yes", 1, 
                          if_else(hsged == "yes", 2, 
                                  if_else(aadeg == "yes", 3, 
                                          if_else(somcol == "yes", 4,
                                                  if_else(bach == "yes", 5,
                                                          if_else(ma == "yes" | docprof == "yes", 6, 1)))))),
           educ = factor(educ, labels = ed.labs),
           raceeth = fct_recode(raceeth,
                                "Asian" = "Asian",
                                "Black" = "Black",
                                "Hisp." = "Hispanic",
                                "White" = "White",
                                "Mult./Other" = ">1 race",
                                "Mult./Other" = "other"),
           female = fct_recode(female, 
                               "Men" = "male", 
                               "Women" = "female")
    ) %>% 
    dplyr::select(-c("nodipdeg", "hsged", "somcol", "aadeg", "bach", "ma", "docprof")) %>% 
    right_join(lclust.dat, by = "id") %>% 
    mutate(id = as.integer(id),
           omnivore = rowSums(across(where(is.double)))) %>% 
    mutate(omnicat = case_when(omnivore %in% 1 ~ 1,
                               omnivore %in% 2:3 ~ 1,
                               omnivore %in% 4:6 ~ 2,
                               omnivore %in% 7:9 ~ 3,
                               omnivore %in% 10:20 ~ 4),
           omnicat = factor(omnicat, 
                            labels = c("One to Three", "Four to Six", "Seven to Nine", "Ten or More"))) %>%  
    mutate(educ.n = as.numeric(educ)) %>% 
    na.omit()
