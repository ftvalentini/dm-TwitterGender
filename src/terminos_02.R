# guarda base dejando texto como vars separadas (words)

# librerias y funciones ---------------------------------------------------
source("src/funciones.R")
source("src/load_librerias.R")

# matrices de terminos ----------------------------------------------------
# carga base ya preprocesada
base_t <- readRDS(file="data/working/base_text.rds")
# lista con 2 elementos (matriz de terminos de text y de desc)
mats_words <- list(text=base_t$text, desc=base_t$description) %>% 
  map(.,
      function(x) (clean_words(x) %>% enc2utf8 %>% 
                     tm::VectorSource(x=.) %>% tm::Corpus(.) %>% 
                     tm::TermDocumentMatrix(x=.) %>%
                     # "Remove Sparse Terms" para que no pese tanto
                     tm::removeSparseTerms(.,0.995) %>% 
                     as.matrix %>% t))
# descriptiva -------------------------------------------------------------
# nro de palabras de cada var
mats_words %>% map(ncol)
  # para obtener mas (menos) palabras aumentar (reducir) el factor de removesparseterms
# 20 words mas frecuentes de cada var
mats_words %>% map(function(x) (colSums(x) %>%
                                  sort(decreasing=T) %>% "["(1:20)))
mats_words %>% map(function(x) (colSums(x) %>% min))
# todas las palabras tienen mas de 19 apariciones

# nueva base "words" ------------------------------------------------------

# cambia nombres de variables para identificar desc (d_) y text (t_)
colnames(mats_words$text) %<>% paste0("t_",.) 
colnames(mats_words$desc) %<>% paste0("d_",.) 
# base con variables de palabras anexadas a la derecha de la base original
base_w <- base_t %>% cbind(.,Reduce(cbind,mats_words)) %>% 
  # sin text ni desc
  dplyr::select(-c(description, text))
# guarda base "words" como objeto
saveRDS(base_w, file="data/working/base_words.rds")

# ponderar cada observacion segun gender.confidence y/o profile_yn.confidence?
# medio complicado
