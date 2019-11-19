#### Assignment 2 - preclean ####

ketchup_data <- read.csv("../raw_data/ketchup_long.csv")

ketchup_data <- ketchup_data %>%
  arrange(week, brand) %>%
  mutate(brand = case_when(brand == "Del Monte" ~ "DM",
                           brand == "Heinz" ~ "HE",
                           brand == "Hunt's" ~ "HU",
                           brand == "store brand" ~ "ST"))

nbr_brands <- as.numeric(ketchup_data %>%
                           distinct(brand) %>%
                           tally())
nbr_weeks <- as.numeric(ketchup_data %>%
                           distinct(week) %>%
                           tally())
N <- nbr_brands*nbr_weeks

ketchup_data <-  ketchup_data %>%
  mutate(one = 1) %>%
  spread(brand, one, fill = 0, sep = "_") %>%
  mutate(brand = case_when(brand_DM == 1 ~ "DM",
                           brand_HE == 1 ~ "HE",
                           brand_HU == 1 ~ "HU",
                           brand_ST == 1 ~ "ST"))