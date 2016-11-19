## Lae kõik Lennart Meri, Arnold Rüütli ja Toomas Hendrik Ilves kõned
## Seejärel analüüsi neid detailsemalt.
## Eestis varem tetud analüüs: http://stat24.ee/2012/02/vabariigi-aastapaeva-konede-analuus/
## Sarnane analüüs Itaaliast tehtud R-ga: http://www.salvaggio.net/publications/R-blog/files/happy-new-year-mr-president.php#unique-entry-id-10

library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# Genereeri url-d Meri kõnede url-de kraapimiseks
meri_konede_yld_url <- str_c("https://vp1992-2001.president.ee/est/k6ned/K6nedeArhiiv.asp?mida=&aasta=",
                        seq(from = 1992, to = 2001, by = 1))

# Genereeri url-d Rüütli kõnede url-de kraapimiseks
ryytel_konede_yld_url <- str_c("https://vp2001-2006.president.ee/et/ametitegevus/k6ned.php?arhiiv=true&aasta=",
                          seq(from = 2001, to = 2006, by = 1))

# Url-d Ilvese kõnede kraapimiseks
ilves_konede_yld_url <- "https://vp2006-2016.president.ee/et/ametitegevus/koned/index.html#0_1"

# Funktsioon Meri kõnede url-de kraapimiseks
kraabi_meri_kone_urlid <- function(x){
    read_html(x) %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        data.frame() %>%
        filter(str_detect(., "K6ne.asp")) %>%
        select(url = 1) %>%
        mutate(kone_url = str_c("https://vp1992-2001.president.ee/est/k6ned/", url)) %>%
        select(kone_url)
}

# Kraabi Meri kõnede url-d
meri_kone_urlid <- map_df(meri_konede_yld_url, kraabi_meri_kone_urlid)

# Funktsioon Meri kõnede kraapimiseks (pealkiri, kuupäev, tekst)
kraabi_meri_koned <- function(x){
    kone_html <- read_html(x)
    
    pealkiri <- kone_html %>%
        html_nodes("br+ table td > font b") %>%
        html_text() %>%
        repair_encoding(from = "UTF-8")
    
    kuupaev <- kone_html %>%
        html_nodes("i") %>%
        html_text()
    
    tekst <- kone_html %>%
        html_nodes("font+ p") %>%
        html_text() %>%
        repair_encoding(from = "UTF-8")
    
    Sys.sleep(1)
    
    data_frame(pealkiri, kuupaev, tekst) %>%
        mutate(president = "Lennart Meri")
}

# Kraabi Meri kõned
meri_koned <- map_df(meri_kone_urlid$kone_url, kraabi_meri_koned)


# Funktsioon Rüütli kõnede url-de kraapimiseks
kraabi_ryytel_kone_urlid <- function(x){
    read_html(x) %>%
        html_nodes("td td td div a") %>%
        html_attr("href") %>%
        data.frame() %>%
        select(url = 1) %>%
        mutate(kone_url = str_c("https://vp2001-2006.president.ee/et/ametitegevus/k6ned.php", url)) %>%
        select(kone_url)
}

# Kraabi Rüütli kõnede url-d
ryytel_kone_urlid <- map_df(ryytel_konede_yld_url, kraabi_ryytel_kone_urlid)

# Funktsioon Rüütli kõnede kraapimiseks (pealkiri, kuupäev, tekst)
kraabi_ryytel_koned <- function(x){
    kone_html <- read_html(x)
    
    pealkiri <- kone_html %>%
        html_nodes("strong") %>%
        html_text() %>%
        repair_encoding(from = "UTF-8")
    
    kuupaev <- kone_html %>%
        html_nodes("small") %>%
        html_text()
    
    tekst <- kone_html %>%
        html_nodes("br+ table tr~ tr+ tr td") %>%
        html_text() %>%
        repair_encoding(from = "UTF-8")
    
    kone <- data_frame(pealkiri, kuupaev, tekst)
    
    kone %>%
        # Kustuta teksti algusest pealkiri ja kuupäev
        mutate(asenda = str_c("^(.*?)", kuupaev, sep = ""),
               tekst = str_replace(tekst, asenda, ""),
               president = "Arnold Rüütel") %>%
        select(-asenda)
    
    Sys.sleep(1)
    
    return(kone)
}

# Kraabi Rüütli kõned
ryytel_koned <- map_df(ryytel_kone_urlid$kone_url, kraabi_ryytel_koned)


# Kraabi Ilvese kõnede url-d
ilves_kone_urlid <- read_html(ilves_konede_yld_url) %>%
    html_nodes(".list_text a") %>%
    html_attr("href") %>%
    data.frame() %>%
    filter(str_detect(., "/et/ametitegevus/koned/")) %>%
    select(url = 1) %>%
    mutate(kone_url = str_c("https://vp2006-2016.president.ee", url)) %>%
    select(kone_url)

# Funktsioon Ilvese kõnede kraapimiseks
kraabi_ilves_koned <- function(x){
    kone_html <- read_html(x)
    
    pealkiri <- kone_html %>%
        html_nodes(".article_title a") %>%
        html_text() %>%
        repair_encoding(from = "UTF-8")
    
    kuupaev <- kone_html %>%
        html_nodes("p.date") %>%
        html_text()
    
    tekst <- kone_html %>%
        html_nodes(".article_left+ .article") %>%
        html_text() %>%
        repair_encoding(from = "UTF-8")
    
    tekst <- kone_html %>%
        html_nodes("p+ p") %>%
        html_text() %>%
        repair_encoding(from = "UTF-8") %>%
        # kuna tekst tuleb lõikude kaupa vektorina, siis ühenda need kokku
        paste0(., collapse = " ")
    
    Sys.sleep(1)
    
    data_frame(pealkiri, kuupaev, tekst) %>%
        mutate(president = "Toomas Hendrik Ilves")
}

# Kraabi Ilvese kõned
ilves_koned <- map_df(ilves_kone_urlid$kone_url, kraabi_ilves_koned)

# Kõned ühte tabelisse kokku
presidentide_konad <- meri_koned %>%
    bind_rows(ryytel_koned) %>%
    bind_rows(ilves_koned)

# Salvesta kõnede tabel
save(presidentide_konad, file = "data/presidentide_koned.RData")