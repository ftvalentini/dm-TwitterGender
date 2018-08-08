# guarda base limpia dejando textos como variables

# librerias y funciones ---------------------------------------------------
source("src/load_librerias.R")
source("src/funciones.R")

# carga base --------------------------------------------------------------
path_base <- here("data","raw","EXCEL_gender-classifier-DFE-791531_utf8.csv")
base_raw <- read.csv(path_base, header=T, stringsAsFactors=F, encoding="UTF-8")

# explora -----------------------------------------------------------------
base <- base_raw %>% 
  # variables irrelevantes para dropear
  dplyr::select(-c(X.U.FEFF._unit_id, X_golden, X_trusted_judgments, X_last_judgment_at,
                   X_unit_state,profile_yn, profile_yn_gold, profileimage,
                   tweet_id, tweet_created, tweet_location)) %>% 
  # variables que tal vez no son irrelevantes pero las sacamo
  dplyr::select(-c(gender.confidence, profile_yn.confidence, gender_gold)) %>% 
  # observaciones que sirven
  dplyr::filter(gender %in% c("brand","female","male")) %>% 
  # variables nuevas o modificadas
    # mes de creacion de perfil, binaria de ubicacion y colores con nombres
  dplyr::mutate(profile_month=lubridate::month(created,label=T) %>% as.character,
                known_coord=ifelse(tweet_coord=="","no","yes"),
                color_link=hextotext(link_color),
                color_side=hextotext(sidebar_color),
                timezone_user=dplyr::if_else(user_timezone=="", "UNKNOWN", user_timezone)) %>% 
          # lo de los colores tarda bastante
    # caracteristicas de text y description
  dplyr::mutate(
      # nro palabras
    n_words_t=stringr::str_count(clean_text(text),"\\S+"),
    n_words_d=stringr::str_count(clean_text(description),"\\S+"),
      # nro puntuacion
    n_punt_t=stringr::str_count(clean_text(text), "\\.|\\,|\\:"),
    n_punt_d=stringr::str_count(clean_text(description), "\\.|\\,|\\:"),
      # nro oraciones (medio dudoso):
    n_oraciones_t=stringr::str_count(clean_text(text), "[[:alnum:]][.!?]"),
    n_oraciones_d=stringr::str_count(clean_text(description), "[[:alnum:]][.!?]"),
      # nro menciones en tweet
    n_mentions_t=stringr::str_count(text, "(\\s|^)@\\S+"),
      # nro hashtags
    n_hash_t=stringr::str_count(text, "(\\s|^)#\\S+"),
    n_hash_d=stringr::str_count(description, "(\\s|^)#\\S+"),
      # nro urls
    n_url_t=stringr::str_count(text, "http[^[:space:]]*"),
    n_url_d=stringr::str_count(description, "http[^[:space:]]*"),
      # nro mails u otros sitios en desc
    n_mail_d=stringr::str_count(description, "[:graph:]+\\.com"),
      # name IN description, text
    name_in_desc=dplyr::case_when(
      stringr::str_detect(tolower(description),tolower(name)) ~ "yes",
      TRUE ~ "no"
    ),
    name_in_text=dplyr::case_when(
      stringr::str_detect(tolower(text),tolower(name)) ~ "yes",
      TRUE ~ "no"
    )) %>%
  # borra vars que sobran
  dplyr::select(-c(tweet_coord,created,name,link_color,sidebar_color)) 

# solo quedan los primeros 20 colores por frecuencia, el resto pasa a "otros"
# lo mismo para timezone
  # es para que modelos corran mas rapido
  # ademas algunos algoritmos solo se bancan 50 niveles max
top_color_side <- base$color_side %>% table %>% sort(decreasing=T) %>% head(20) %>% names
top_color_link <- base$color_link %>% table %>% sort(decreasing=T) %>% head(20) %>% names
top_timezone <- base$timezone_user %>% table %>% sort(decreasing=T) %>% head(20) %>% names
base$color_side %<>% {ifelse(. %in% top_color_side, ., "otros")}
base$color_link %<>% {ifelse(. %in% top_color_link, ., "otros")}
base$timezone_user %<>% {ifelse(. %in% top_timezone, ., "otros")}

# guarda base como objeto
saveRDS(base, file="data/working/base_text.rds")

#### DUDAS:
# sacar gender_gold?
# ver notas.txt

