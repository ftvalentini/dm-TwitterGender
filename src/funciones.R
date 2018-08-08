source("src/load_librerias.R")

# operador para concatenar texto:
"%+%" <- function(a,b) paste(a,b,sep="")

# convierte hex colors en nombres de colores
hextotext <- function(x) {
  library(plotrix)
  out <- rep("UNKNOWN",length(x))
  # pone numerales si faltan
  vec <- ifelse(substr(x,1,1)!="#", paste0("#",x), x)
  # reemplaza con nombres
    # (si falta un digito agrega un 7 xq no cambia mucho)
    # (si falta mas de un digito devuelve "UNKNOWN") 
  for (i in seq_along(x)) {
    if (nchar(vec[i])==7) {out[i]<-color.id(vec[i])[1]}
    if (nchar(vec[i])==6) {out[i]<-color.id(paste0(vec[i],"7"))[1]}
  }
  # saca numeros de los strings y return
  return(gsub('[0-9]+', '', out))
}

# convierte positivo a 1, 0 a 0
count_to_boolean <- function(x) as.factor(ifelse(x>0, 1, 0))

# convierte factor/character en data.frame de unos y ceros
fac_to_mat <- function(x) {
  if (is.character(x)) nom <- unique(x)
  if (is.factor(x)) nom <- levels(x)
  out <- matrix(0,nr=length(x),nc=length(nom)) %>% set_colnames(nom)
  for (j in seq_along(nom)) out[,j] <- ifelse(x==nom[j],1,0)
  return(as.data.frame(out))
}

# genera fila de resultados sobre modelo caret
results_caret <- function(model) {
  mod <- model
  # accuracy
  acc <- mod$results["Accuracy"] %>% max %>% setNames("Accuracy")
  # specif y sensit por clase
  specsens <- caret::confusionMatrix(mod$pred$pred, mod$pred$obs) %>%  
    "$"("byClass") %>% "["(.,,c("Sensitivity","Specificity")) %>% c %>% 
    setNames(c(t(outer(X=c("Sensitivity","Specificity"),Y=mod$levels, paste, sep="_"))))
  # AUC por clase
  auc <- cbind(fac_to_mat(mod$pred$obs) %>% setNames(names(.)%+%"_true"),
               mod$pred[mod$levels] %>% setNames(names(.)%+%"_pred_mod")) %>% 
    multiROC::multi_roc(.) %>% "$"("AUC") %>% unlist %>% "["(1:length(mod$levels)) %>% 
    setNames("AUC_"%+%mod$levels)
  out <- c(acc, specsens, auc)
  return(out)
}

# genera resultados para lista de modelos caret
results_caret_map <- function(model_list, model_abbr) {
  out <- map(model_list, results_caret) %>% Reduce(f=rbind, .) %>% 
    {if (length(model_list)==1) as.data.frame(t(.)) else as.data.frame(.)} %>%  
    cbind(model=model_abbr%+%"."%+%names(model_list),.) %>% dplyr::arrange(-Accuracy)
  return(out)
}

# ensamble: resultados de voto por mayoria de modelos con CV con iguales folds
  # no hay auc porque no hay probs
  # argumento: lista de modelos con nombres
ensamble_vot_simple <- function(model_list) {
  folds <- model_list[[1]]$pred[c("Resample","rowIndex")] %>%
    split(as.factor(.$Resample)) %>% map(function(x) x$rowIndex)
  niveles <- model_list[[1]]$levels
  # clases obs
  obs <- model_list[[1]]$pred %>% dplyr::arrange(rowIndex) %>% "$"("obs")
  # accuracy de cada modelo en cada fold
  acc_mods <- map(model_list, function(x) x$resample$Accuracy) %>% Reduce(f=cbind,x=.) %>% 
    as.data.frame %>% setNames(names(model_list))
  # data.frame con accuracy final de cada fold
  acc_df <- data.frame(Accuracy=rep(NA, length(folds)))
  # data.frame con specsens final de cada fold
  specsens_df <- matrix(NA, nr=length(folds), nc=2*length(niveles)) %>% as.data.frame %>% 
    setNames(c(t(outer(X=c("Sensitivity","Specificity"),Y=niveles, paste, sep="_"))))
  for (i in seq_along(folds)) {
    # voto mayoritario
    df_pred <- map_dfr(model_list, function(x)
      x$pred %>% dplyr::arrange(rowIndex) %>% "["(folds[[i]],"pred")) %>%
      dplyr::mutate(voto=apply(.,1, get_moda)) %>% dplyr::mutate_all(as.character)
    # si hay empate se elige prediccion de modelo con mejor acc en el fold
      # si hay empate en accuracy se elige al azar
    best_i <- names(acc_mods)[acc_mods[i,]==max(acc_mods[i,])] %>% 
      "["(sample.int(length(.),1))
    df_pred$voto[is.na(df_pred$voto)] <- df_pred[[best_i]][is.na(df_pred$voto)]
    # matriz de confusion para obtener metricas del fold
    cm <- caret::confusionMatrix(as.factor(df_pred$voto), obs[folds[[i]]])
    acc_df[i,] <- cm$overall[1]
    specsens_df[i,] <- cm$byClass[,c("Sensitivity","Specificity")] %>% c
  }
  acc <- apply(acc_df, 2, mean)
  specsens <- apply(specsens_df, 2, mean)
  return(c(model="votos",acc, specsens))
}

# ensamble: resultados de promedio ponderado por ROC de modelos con CV con iguales folds
# argumento: lista de modelos caret con CV con iguales folds
ensamble_vot_prob <- function(model_list) {
  folds <- model_list[[1]]$pred[c("Resample","rowIndex")] %>%
    split(.,as.factor(.$Resample)) %>% map(function(x) x$rowIndex) %>% 
    map(sort)
  niveles <- model_list[[1]]$levels
  # clases obs
  obs <- model_list[[1]]$pred %>% dplyr::arrange(rowIndex) %>% "$"("obs")
  # auc de cada modelo en cada fold
  auc_mods <- map(model_list, function(x) map(folds, function(y) 
    auc_oneall(x, subset_i=y))) %>% 
    map(.,function(r) Reduce(rbind, r) %>% as.data.frame) %>% 
    transpose %>% map(as.data.frame)
  # data.frame con accuracy final de cada fold
  acc_df <- data.frame(Accuracy=rep(NA, length(folds)))
  # data.frame con specsens final de cada fold
  specsens_df <- matrix(NA, nr=length(folds), nc=2*length(niveles)) %>% as.data.frame %>% 
    setNames(c(t(outer(X=c("Sensitivity","Specificity"),Y=niveles, paste, sep="_"))))
  # data.frame con auc final de cada fold
  auc_df <- matrix(NA, nr=length(folds), nc=length(niveles)) %>% as.data.frame %>% 
    setNames("AUC_"%+%niveles)
  for (i in seq_along(folds)) {
    # probs de cada modelo por clase
    l_probs <- map(model_list, function(x)
      x$pred %>% dplyr::arrange(rowIndex) %>% "["(folds[[i]],niveles) %>%
        norm_fila) %>% transpose %>% map(as.data.frame) %>% map(as.matrix) %>% 
      # pone 1/3 prob si hay NA
      map(function(x) "[<-"(x, is.na(x),1/length(niveles)))
    # ponderadores del fold para cada clase
    ponds_i <- map(auc_mods, function(x) x[i,]) %>% map(as.matrix) %>% map(norm_fila)
    # promedio ponderado de probs
    new_probs <- map2_dfr(l_probs,ponds_i, function(a,b) a%*%t(b))
    # prediccion segun el promedio
    pred_prom <- niveles[apply(new_probs,1,which.max)]
    # matriz de confusion para obtener metricas del fold
    cm <- caret::confusionMatrix(as.factor(pred_prom), obs[folds[[i]]])
    acc_df[i,] <- cm$overall[1]
    specsens_df[i,] <- cm$byClass[,c("Sensitivity","Specificity")] %>% c
    auc_df[i,] <- auc_oneall_simple(obs[folds[[i]]], prob_y=new_probs)
  }
  acc <- apply(acc_df, 2, mean)
  specsens <- apply(specsens_df, 2, mean)
  auc <- apply(auc_df, 2, mean)
  return(c(model="promedios",acc, specsens, auc))
}

# funcion de ensamble stack para lista de modelos caret con iguales folds CV
# type="raw" si modelo con clases, type="prob" si modelo con probs
# method es el modelo de ensamble (ejemplos: rf)
# tunegrid: posibles parametros para CV si hace falta segun method
ensamble_stack <- function(model_list, type="prob", method, tunegrid=NULL, seed=1) {
  k=7; semilla=seed
  semillas <- vector("list",k+1)
  for (i in seq_along(semillas)) semillas[[i]] <- sample.int(10000, 2500)
  train_cv <- caret::trainControl(method="cv", number=k, seeds=semillas,
                                  savePredictions="final",
                                  classProbs=TRUE,
                                  summary=caret::multiClassSummary, 
                                  verbose=F)
  
  niveles <- model_list[[1]]$levels
  obs <- model_list[[1]]$pred %>% dplyr::arrange(rowIndex) %>% "$"("obs")
  preds_x <- map_dfr(model_list,
                     function(x) x$pred %>% dplyr::arrange(rowIndex) %>% "$"(pred))
  probs_x <- map(model_list,
                 function(x) x$pred %>% dplyr::arrange(rowIndex) %>% "["(niveles)) %>% 
    # prob=1/3 si prob es NA
    as.data.frame %>% map_dfr(function(x) replace(x, is.na(x), 1/length(niveles)))
  set.seed(semilla)
  stack_model <- caret::train(x=if (type=="prob") probs_x else if (type=="raw") preds_x,
                              y=obs,
                              method=method,
                              trControl=train_cv,
                              tuneGrid=tunegrid)
  out <- list(model=stack_model,
              results=c(model="stacking",results_caret(stack_model)) )
  return(out)
}

# secuencia simetrica desde midpoint
seq_mid <- function(mid,sides,cada=1) seq(floor(mid)-sides,floor(mid)+sides,by=cada)

# moda de un vector atomico
get_moda <- function(x){ 
  tab <- table(na.omit(x))
  mx <- max(tab)
  # si todas las clases tienen = frecuencia devuelve NA
  if (all(tab == mx) & length(tab)>1) mod <- NA
  else if (is.numeric(x)) mod <- as.numeric(names(tab)[tab==mx])
  else mod <- names(tab)[tab==mx]
  # si hay mas de una moda devuelve NA
  if (length(mod)>1) mod <- NA
  return(mod)
}

# normaliza filas de dataframe para que sumen uno
norm_fila <- function(df) {
  out <- apply(df, 1, function(x) x/sum(x)) %>% t %>% as.data.frame
  return(out)
  }

# limpia strings guardados en vector dejando palabras y puntuacion
clean_text <- function(char_vec) {
  library(magrittr)
  patt <- c('é', '…', '—', "[‘“’”´`]", '～', '＞', '+', '&amp;')
  repl <- c('e', '...', '-', "'", '~', '＞', '+', 'and')
  clean <- char_vec %>% 
    # pasa de utf8 a ascii (no se si sirve)
    qdap::mgsub(pattern=patt, replacement=repl, .) %>% 
    # saca html symbols (no se si sirve)
    stringr::str_replace_all('&[a-z]{1,6};', ' ') %>%
    # saca urls
    stringr::str_replace_all('http[^[:space:]]*', ' ') %>%  
    # saca menciones
    stringr::str_replace_all(.,'(\\s|^)@\\S+', ' ') %>% 
    # saca hashtags
    stringr::str_replace_all(.,'(\\s|^)#\\S+', ' ') %>%
    # saca mails o sitios
    stringr::str_replace_all(.,'[:graph:]+\\.com', ' ') %>% 
    # saca apostrofes
    stringr::str_replace_all("'", "") %>% 
    # saca todo lo que no sea numeros, letras, espacios y puntuacion
    stringr::str_replace_all('[^[:alnum:][:space:][:punct:]]', ' ') %>%
    # saca whitespace irrelevante y ws ppio y final
    tm::stripWhitespace(.) %>% trimws(.)
  return(clean)
}

# limpia strings guardados en vector dejando palabras utiles separadas por espacios
clean_words <- function(char_vec, rem_mention=T, rem_numbers=T, rem_hashtag=F) {
  library(magrittr)
  patt <- c('é', '…', '—', "[‘“’”´`]", '～', '＞', '+', '&amp;')
  repl <- c('e', '...', '-', "'", '~', '＞', '+', 'and')
  # stopwords:
  stopw <- c(tm::stopwords("en")) %>% 
    stringr::str_replace_all('[[:punct:] ]+', '') %>% tolower
  clean <- char_vec %>% 
    # pasa de utf8 a ascii (no se si sirve)
    qdap::mgsub(pattern=patt, replacement=repl, .) %>% 
    # saca html symbols (no se si sirve)
    stringr::str_replace_all('&[a-z]{1,6};', ' ') %>% 
    # saca cualquier cosa entre <> (no hay casos pero lo dejamo)
    stringr::str_replace_all('<[:graph:]+>', ' ') %>% 
    # saca urls
    stringr::str_replace_all('http[^[:space:]]*', ' ') %>%  
    # saca menciones
    {if (rem_mention==T) stringr::str_replace_all(.,'(\\s|^)@\\S+', ' ') else .} %>% 
    # saca hashtags
    {if (rem_hashtag==T) stringr::str_replace_all(.,'(\\s|^)#\\S+', ' ') else .} %>%
    # saca numeros aislados
    {if (rem_numbers==T) stringr::str_replace_all(.,'\\b\\d+\\b', ' ') else .} %>%
    # saca mails o sitios
    stringr::str_replace_all(.,'[:graph:]+\\.com', ' ') %>% 
    # saca palabras con letras raras (ver en.wikipedia.org/wiki/List_of_Unicode_characters)
    stringr::str_replace_all('[:graph:]*[¡-ỳ]+[:graph:]*', ' ') %>% 
    # saca apostrofes
    stringr::str_replace_all("'", "") %>% 
    # lower case
    tolower(.) %>% 
    # saca puntuacion
    stringr::str_replace_all('[[:punct:] ]+', ' ') %>%
    # saca stopwords ingles
    tm::removeWords(., stopw) %>%
    # saca cualquier caracter aislado -entre espacios-
    stringr::str_replace_all('[:space:].[:space:]', ' ') %>%
    # saca todo lo que no sea numeros, letras y espacios
    stringr::str_replace_all('[^[:alnum:][:space:]]', ' ') %>%
    # saca whitespace irrelevante y ws ppio y final
    tm::stripWhitespace(.) %>% trimws(.)
  return(clean)
} 

# calcula AUC one vs all para un modelo caret en un subset con indices
auc_oneall <- function(model, subset_i) {
  obs <- model$pred %>% dplyr::arrange(rowIndex) %>% "$"("obs") %>% "["(subset_i)
  probs <- model$pred %>% dplyr::arrange(rowIndex) %>% "["(model$levels) %>% "["(subset_i,)
  auc <- cbind(fac_to_mat(obs) %>% setNames(names(.)%+%"_true"),
               probs %>% setNames(names(.)%+%"_pred_mod")) %>% 
    multiROC::multi_roc(.) %>% "$"("AUC") %>% unlist %>% "["(1:length(model$levels)) %>% 
    setNames("AUC_"%+%model$levels)
  return(auc)
}

# calcula AUC one vs all para un vector de obs. y un df de probs de cada clase
auc_oneall_simple <- function(obs_y, prob_y) {
  auc <- cbind(fac_to_mat(obs_y) %>% setNames(names(.)%+%"_true"),
               prob_y %>% setNames(names(.)%+%"_pred_mod")) %>% 
    multiROC::multi_roc(.) %>% "$"("AUC") %>% unlist %>% "["(1:length(levels(obs_y))) %>% 
    setNames("AUC_"%+%levels(obs_y))
  return(auc)
}
