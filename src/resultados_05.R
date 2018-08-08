# carga modelos caret ya generados y produce resultados
  # cada modelo es un mismo algoritmo sobre distintas bases
semilla <- 1

# librerias y funciones ---------------------------------------------------
source("src/funciones.R")
source("src/load_librerias.R")



# carga modelos -----------------------------------------------------------
# modelos encontrados
model_paths <- list.files(path="data/final",pattern="\\mod.rds$",full.names=T)
# cada conjunto de modelos en una lista
model_l <- map(model_paths,readRDS) %>% 
  setNames(stringr::str_match(model_paths, "\\/final\\/(.*?)\\_mod")[,2])
# todos los modelos-base en un elemento distinto
model_l_u <- unlist(model_l, recursive=F)


# ensamble ----------------------------------------------------------------

# resultados para elegir modelos para ensamble
# cada elemento es un data.frame de rdos de un modelo
res_simples <- model_l %>%
  map2_dfr(.,names(.), function(x,y) results_caret_map(x, model_abbr=y))

# modelos elegidos para ensamblar (los del tp)
# modelos_str <- names(model_l_u)
modelos_str <- c("c4.full_b",
                 "kn.num_f",
                 "nb.words_b","nb.full_f",
                 "nn.full_b",
                 "rf.full_f",
                 "xg.num_f")
modelos <- model_l_u[modelos_str]

# besttune de cada modelo
modelos %>% map(function(x) x$bestTune)

# corplot del accuracy de los modelos elegidos
png(filename="output/corplot.png", bg="transparent")
    # width=12, height=8, units="in", res=300)
modelos %>% setNames(c("C4","KN","NB1","NB2","NN","RF","XG")) %>% 
  map_dfr(function(x) x$resample$Accuracy) %>% cor %>% 
  corrplot::corrplot(method="number",type="lower",diag=F,
                     tl.col="black",tl.cex=1.5,number.cex=1.5)
dev.off()

# tres ensambles: votos, promedios y stacking 

# combinaciones posibles de variables
var_combs <- map(3:length(modelos), function(x) combn(names(modelos), x)) %>% 
  map(function(x) split(x, c(col(x)))) %>% unlist(recursive=F) %>% setNames(NULL)

# ensamble de votos y de promedios para todas las combs de vars
res_votos_df <- map(var_combs, function(x) ensamble_vot_simple(modelos[x])) %>% 
  Reduce(dplyr::bind_rows,x=.) %>% 
  dplyr::mutate_if(function(x) !(all(as.numeric(x) %in% NA)), as.numeric)
res_prom_df <- map(var_combs, function(x) ensamble_vot_prob(modelos[x])) %>% 
  Reduce(dplyr::bind_rows,x=.) %>% 
  dplyr::mutate_if(function(x) !(all(as.numeric(x) %in% NA)), as.numeric)

# combinaciones optimas para cada uno
var_combs %>% "[["(which(res_votos_df$Accuracy==max(res_votos_df$Accuracy)))
var_combs %>% "[["(which(res_prom_df$Accuracy==max(res_prom_df$Accuracy)))

res_votos_best <- res_votos_df %>% dplyr::filter(Accuracy==max(Accuracy))
res_prom_best <- res_prom_df %>% dplyr::filter(Accuracy==max(Accuracy))

# ensamble stacking RF (con probs en lugar de clases) para todas las vars:
  # SIN C4 PORQUE NO FUNCIONA EL PREDICT!!
grid_rf=expand.grid("mtry"=1:length(modelos))
stack <- ensamble_stack(modelos[!(names(modelos) %in% "c4.full_b")],
                        type="prob", method="rf", tunegrid=grid_rf)
res_stack <- stack$results %>% as.list %>% data.frame(stringsAsFactors=F) %>% 
  dplyr::mutate_if(function(x) !(is.na(as.numeric(x))), as.numeric)

# stack_r <- ensamble_stack(modelos, type="raw", method="rf", tunegrid=grid_rf)
# res_stack_r <- stack_r$results



# resultados finales ------------------------------------------------------
simples <- res_simples %>% dplyr::filter(model %in% modelos_str)
ensambles <- dplyr::bind_rows(res_votos_best, res_prom_best, res_stack)
res_final <- rbind(simples, ensambles) %>% dplyr::mutate_at(-1, as.numeric)
# guarda tabla
write.csv(res_final, file="output/resultados_metricas.csv", row.names=F)
# plot de accuracy
plot_acc <- ggplot(data=res_final) +
  geom_col(aes(x=model, y=Accuracy))
ggsave(filename="output/accplot.png",plot=plot_acc,device="png")

# bases test --------------------------------------------------------------

base_test <- readRDS(file="data/final/base_test.rds")

# predictores
x_test <- list(
  # FULL_f: todas las variables y las palabras como frecuencia
  full_f = base_test %>% dplyr::select(-gender),
  # FULL_b: todas las variables y las palabras como binaria
  full_b = base_test %>% dplyr::select(-gender) %>% dplyr::mutate_at(
    dplyr::vars(dplyr::starts_with("t_"),dplyr::starts_with("d_")),
    count_to_boolean),
  # words_f: solo las palabras y como frecuencia
  words_f = base_test %>% dplyr::select(-gender ) %>% 
    dplyr::select(dplyr::starts_with("t_"),dplyr::starts_with("d_")),
  # words_b: solo las palabras y como binaria
  words_b = base_test %>% dplyr::select(-gender ) %>% dplyr::transmute_at(
    dplyr::vars(dplyr::starts_with("t_"),dplyr::starts_with("d_")),
    count_to_boolean)
)

# respuesta
y_test <- base_test$gender

# base numerica para xg y kn
cat_vars <- c("user_timezone","profile_month","known_coord","color_link","color_side",
              "name_in_desc","name_in_text")
x_test_num_f <- x_test$full_f %>% 
  dplyr::select(-dplyr::one_of(cat_vars)) %>%
  scale %>% as.data.frame %>% map_dfr(function(x) replace(x, is.na(x), 0))

# prediccion sobre test  --------------------------------------------------

# pone iguales niveles de factores en test que en train


# probabilidades asignadas por cada modelo simple
  # NO INCLUYE C4 PORQUE NO FUNCIONA!!!
p_probs <- list(
  # c4.fullb = predict(modelos$c4.full_b, newdata=x_test$full_b, type="prob"),
  kn.num_f = predict(modelos$kn.num_f, newdata=x_test_num_f, type="prob"),
  nb.words_b = predict(modelos$nb.words_b, newdata=x_test$words_b, type="prob"),
  nb.full_f = predict(modelos$nb.full_f, newdata=x_test$full_f, type="prob"),
  nn.full_b = predict(modelos$nn.full_b, newdata=x_test$full_b, type="prob"),
  rf.full_f = predict(modelos$rf.full_f, newdata=x_test$full_f, type="prob"),
  xg.num_f = predict(modelos$xg.num_f, newdata=x_test_num_f, type="prob")
) %>% 
  unlist(recursive=F) %>% as.data.frame

# clases asignadas por cada modelo simple
  # NO INCLUYE C4 PORQUE NO FUNCIONA!!!
p_classes <- list(
  # c4.full_b = predict(modelos$c4.full_b, newdata=x_test$full_b),
  kn.num_f = predict(modelos$kn.num_f, newdata=x_test_num_f),
  nb.words_b = predict(modelos$nb.words_b, newdata=x_test$words_b),
  nb.full_f = predict(modelos$nb.full_f, newdata=x_test$full_f),
  nn.full_b = predict(modelos$nn.full_b, newdata=x_test$full_b),
  rf.full_f = predict(modelos$rf.full_f, newdata=x_test$full_f),
  xg.num_f = predict(modelos$xg.num_f, newdata=x_test_num_f)
)
# prediccion del algoritmo de stacking (probs y clases)
p_stack_probs <- predict(stack$model, newdata=p_probs, type="prob")
p_stack_classes <- predict(stack$model, newdata=p_probs)

cm <- caret::confusionMatrix(p_stack_classes, y_test)
# matriz confusion
cm$table
# medidas de performance
acc <- cm$overall[1] %>% setNames("Accuracy")
specsens <- cm$byClass[,c("Sensitivity","Specificity")] %>% c %>% 
  setNames(c(t(outer(X=c("Sensitivity","Specificity"), Y=levels(y_test), paste, sep="_"))))
auc <- auc_oneall_simple(obs_y=y_test, prob_y=p_stack_probs)
medidas <- c(acc, specsens, auc) %>% t
write.csv(medidas, file="output/metricas_test.csv", row.names=F)


# txt con id y prediccion -------------------------------------------------

# base_raw con observaciones sin gender==NA
path_base_raw <- "data/raw/EXCEL_gender-classifier-DFE-791531_utf8.csv"
base_raw <- read.csv(path_base_raw, header=T, stringsAsFactors=F, encoding="UTF-8") %>%
  dplyr::filter(gender %in% c("brand","female","male"))
# regenero indices de test de traintest_03.R
set.seed(1)
N <- nrow(base_raw); n_t <- 500
test_i <- sample(N, size=n_t, rep=F)
# ids de instancias de test
unit_id <- base_raw[test_i,"X.U.FEFF._unit_id"]
test_pred <- data.frame(unit_id, clase_modelo=p_stack_classes)
write.table(test_pred,file="output/clases_modelo.txt",sep=",",row.names=F)
