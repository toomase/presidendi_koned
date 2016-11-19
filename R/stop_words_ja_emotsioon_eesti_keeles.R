## 1. Töötle loetelu eesti keele stop words kohta 
## (enamlevinud sõnad, mis tuleks tekstianalüüsi korral eemdaldada)
## Põhineb bakatööl: http://dspace.ut.ee/handle/10062/32779?locale-attribute=en
## 2. Töötle andmetabelit eesti keele sõnade emotsiooni kohta (pos neg)
## Põhineb EKI emotsioonidetektori andmetel: https://github.com/EKT1/valence

library(readr)
library(dplyr)
library(stringr)
library(purrr)

# lae eestikeelsed stop words
stop_words_est_raw <- read_file("data/stop_words_est.txt")

# lae eesti sõnade emotsioonid (pos/neg)
sonade_emotsioon_est_raw <- read_csv("data/sqnad.csv", col_names = FALSE)

lisa_stop_words_est <- c("oleme", "kas", "sellest", "olema",
                         "püüd", "ongi", "niisiis", "lubatagu",
                         "su", "hakkama", "ndatel", "ndate")

# töötle stop words vektoriks
stop_words_est <- str_split(stop_words_est_raw, ", ") %>%
    flatten() %>%
    as.character() %>%
    c(., lisa_stop_words_est) %>%
    tibble(sona = .)

# töötle sõnade emotsiooni tabelit
sonade_emotsioon_est <- sonade_emotsioon_est_raw %>%
    select(sona = 1, emotsioon = 2) %>%
    mutate(emotsioon_sona = ifelse(emotsioon == -1, "negatiivne", "positiivne"),
           emotsioon_nr = emotsioon) %>%
    select(-emotsioon)

# salvesta stop words vektor
save(stop_words_est, file = "data/stop_words_est.RData")

# salvesta ssõnade emotsiooni data frame
save(sonade_emotsioon_est, file = "data/sonade_emotsioon_est.RData")