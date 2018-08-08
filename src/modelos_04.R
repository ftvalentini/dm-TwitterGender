# corre modelos y los guarda como RDS
  # cada objeto guardado es una lista de modelos (un algoritmo sobre varias bases)
  # guardar como "data/final/*_mod.rds"
  # no correr las secciones de modelos ya hechos

# TRUE si correr cada modelo con paralelizacion:
  # va mas rapido pero inhabilita el uso de la pc
run_parallel <- FALSE
# activar esto despues de terminar el TP!:
# source("src/traintest_03.R")

# librerias y funciones ---------------------------------------------------
source("src/funciones.R")
source("src/load_librerias.R")


# bases  ------------------------------------------------------------------
# train-validation
set.seed(1)
base_tv <- readRDS(file="data/final/base_train_validation.rds") 

### PREDICTORES
# genera una lista para train y test con posibles configuraciones de los datos
x_train <- list(
  # FULL_f: todas las variables y las palabras como frecuencia
  full_f = base_tv %>% dplyr::select(-gender),
  # FULL_b: todas las variables y las palabras como binaria
  full_b = base_tv %>% dplyr::select(-gender) %>% dplyr::mutate_at(
    dplyr::vars(dplyr::starts_with("t_"),dplyr::starts_with("d_")),
    count_to_boolean),
  # words_f: solo las palabras y como frecuencia
  words_f = base_tv %>% dplyr::select(-gender ) %>% 
    dplyr::select(dplyr::starts_with("t_"),dplyr::starts_with("d_")),
  # words_b: solo las palabras y como binaria
  words_b = base_tv %>% dplyr::select(-gender ) %>% dplyr::transmute_at(
    dplyr::vars(dplyr::starts_with("t_"),dplyr::starts_with("d_")),
    count_to_boolean)
)

### RESPUESTA
y_train <- base_tv$gender


# nombres de variables que no pueden interpretarse como numericas (para KNN)
cat_vars <- c("user_timezone","profile_month","known_coord","color_link","color_side",
              "name_in_desc","name_in_text")


# validation methods ------------------------------------------------------
# k-fold CV
k=7; semilla=1
# mismos folds para todos los modelos:
set.seed(semilla)
# lista length=k+1
semillas <- vector("list",k+1)
# la cantidad de elementos de cada elemento debe ser de al menos la cantidad de modelos probados en una corrida
for (i in seq_along(semillas)) semillas[[i]] <- sample.int(10000, 2500)
train_cv <- caret::trainControl(method="cv", number=k, seeds=semillas,
                                savePredictions="final",
                                classProbs=TRUE,
                                summary=caret::multiClassSummary, 
                                verbose=F)

# alternativa: parametros definidos por usuario sin validation:
train_simple <- caret::trainControl(method="none", 
                                    savePredictions="final",
                                    classProbs=TRUE,
                                    summary=caret::multiClassSummary,
                                    verbose=F)







# Naive bayes -------------------------------------------------------------
# combinaciones posibles de parametros:
# alternativa: usar tuneLength=10
nb_param <- expand.grid("laplace"=seq(0,4, by=1), 
                        "usekernel"=c(F),
                        "adjust"=c(F)) 

if (run_parallel) {
  library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
}

#### al final lo corrimos sin feature selection:
## feature selection para cada base (en NB hay problema de p grande)
# #define the control using a nbayes selection function
# nb_control <- caret::rfeControl(functions=caret::nbFuncs,
                                # method="cv", number=2, verbose=F) 
# # run the RFE algorithm and select optimal predictors (para cada base)
# set.seed(semilla)
# nb_feat <- map(x_train[c("words_f","words_b")], function(x)
#   caret::rfe(x=x, y=y_train,sizes=seq(1,ncol(x),5),rfeControl=nb_control) %>% 
#     caret::predictors(.))
## modelo (con feat selection:)
# set.seed(semilla)
# nb_mod <- map2(.x=x_train,.y=nb_feat, function(x,y)
#   caret::train(x=x[y],
#                y=y_train,
#                method="naive_bayes",
#                trControl=train_cv,
#                tuneGrid=nb_param))

# modelo (version sin feat selection)
set.seed(semilla)
nb_mod <- map(.x=x_train, function(x)
  caret::train(x=x,
               y=y_train,
               method="naive_bayes",
               trControl=train_cv,
               tuneGrid=nb_param))
if (run_parallel) stopCluster(cl)

results_caret_map(nb_mod, model_abbr="nb")
# save
saveRDS(nb_mod, file="data/final/nb_mod.rds")

# C4.5 --------------------------------------------------------------------
# combinaciones posibles de parametros:
c4_param <- expand.grid("C"=seq(0.01,0.5,by=0.05), 
                        "M"=seq(2,30,by=3)) 

if (run_parallel) {
  library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
}
# modelo (para cada base)
set.seed(semilla)
c4_mod <- map(x_train[c("full_f","full_b")], 
              function(x) caret::train(x=x,
                                       y=y_train,
                                       method="J48",
                                       trControl=train_cv,
                                       tuneGrid=c4_param))
if (run_parallel) stopCluster(cl)

results_caret_map(c4_mod, model_abbr="c4")
# save
saveRDS(c4_mod, file="data/final/c4_mod.rds")

# random forest -----------------------------------------------------------
# combinaciones posibles de parametros:
# el default es la raiz de #predictores
rf_param <- expand.grid("mtry"=seq_mid((sqrt(ncol(x_train$full_f))),3)) 
  # alternativa: tuneLength=10

if (run_parallel) {
  library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
}
# modelo (para cada base)
set.seed(semilla)
rf_mod <- map(x_train[c("full_f","full_b")], 
              function(x) caret::train(x=x,
                                       y=y_train,
                                       method="parRF",
                                       trControl=train_cv,
                                       tuneGrid=rf_param))
if (run_parallel) stopCluster(cl)

results_caret_map(rf_mod,model_abbr="rf")
# save
saveRDS(rf_mod, file="data/final/rf_mod.rds")

# KNN ---------------------------------------------------------------------
# combinaciones posibles de parametros:
kn_param <- expand.grid("k"=1:20) 

if (run_parallel) {
  library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
}
# modelo (solo variables no categoricas, y estandarizadas)
set.seed(semilla)
kn_mod <- map(list(num_f=x_train$full_f), 
              function(x) caret::train(x=x %>%
                                         dplyr::select(-dplyr::one_of(cat_vars)) %>% 
                                         scale %>% as.data.frame,
                                       y=y_train,
                                       method="knn",
                                       trControl=train_cv,
                                       tuneGrid=kn_param))
if (run_parallel) stopCluster(cl)

results_caret_map(kn_mod, model_abbr="kn")
# save
saveRDS(kn_mod, file="data/final/kn_mod.rds")

# neural network ----------------------------------------------------------
nn_param <- expand.grid("size"=1,
                        "decay"=3**(-6:1))
# weights=size*(p+1)+size+1 no puede superar n
  # no deberiamos tener ese problema porque 8*365+8+1=2929 < 18836*0.9
# modelo (solo variables no categoricas)

if (run_parallel) {
  library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
}
set.seed(semilla)
nn_mod <- map(x_train[c("full_f","full_b")], 
              function(x) caret::train(x=x,
                                       y=y_train,
                                       method="nnet",
                                       trControl=train_cv,
                                       tuneGrid=nn_param))
if (run_parallel) stopCluster(cl)

results_caret_map(nn_mod, model_abbr="nn")
# save
saveRDS(nn_mod, file="data/final/nn_mod.rds")


# xgboost -----------------------------------------------------------------
# combinaciones posibles de parametros:
  # para method="xgbLinear": 
xg_param <- expand.grid("nrounds"=c(10,15), 
                        "lambda"=c(0,1,2,3),
                        "alpha"=c(0,1),
                        "eta"=c(0.01)) 
# alternativa: tuneLength=10
# modelo (solo variables no categoricas, y ¿¿estandarizadas?? creo que si, da mejor)
if (run_parallel) {
  library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
}
set.seed(semilla)
xg_mod <- map(list(num_f=x_train$full_f), 
              function(x) caret::train(x=x %>%
                                         dplyr::select(-dplyr::one_of(cat_vars)) %>% 
                                         scale %>% as.data.frame,
                                       y=y_train,
                                       method="xgbTree",
                                       trControl=train_cv,
                                       tuneLength=3))
if (run_parallel) stopCluster(cl)

results_caret_map(xg_mod,model_abbr="xg")
# save
saveRDS(xg_mod, file="data/final/xg_mod.rds")


# bagged CART -------------------------------------------------------------

# NO LO CORRIMOS PORQUE TARDA MUCHO!!!

# # combinaciones posibles de parametros:
#   # no tiene parametros para tunear
# # modelo (para cada base)
# if (run_parallel) {
  # library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
# }
# set.seed(semilla)
# bg_mod <- map(x_train[c("full_f","full_b")], 
#               function(x) caret::train(x=x,
#                                        y=y_train,
#                                        method="treebag",
#                                        trControl=train_cv,
#                                        tuneLength=5))
# if (run_parallel) stopCluster(cl)
# results_caret_map(bg_mod, model_abbr="bg")
# # save
# saveRDS(bg_mod, file="data/final/bg_mod.rds")



# adaboost CART -----------------------------------------------------------
  
  # NO LO CORRIMOS PORQUE TARDA MUCHO!!!
# 
# # method="adaboost" no sirve para multiclass
# # combinaciones posibles de parametros:
# # el default es la raiz de #predictores
# ad_param <- expand.grid("mfinal"=,
#                         "maxdepth"=,
#                         "coeflearn"=) 
# # alternativa: fijar tuneLength
# 
# if (run_parallel) {
  # library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
# }
# # modelo (para cada base)
# set.seed(semilla)
# ad_mod <- map(x_train[c("full_f","full_b")], 
#               function(x) caret::train(x=x,
#                                        y=y_train,
#                                        method="AdaBoost.M1",
#                                        trControl=train_cv,
#                                        tuneLength=2))
# if (run_parallel) stopCluster(cl)
# results_caret_map(ad_mod,model_abbr="ad")
# # save
# saveRDS(ad_mod, file="data/final/ad_mod.rds")










