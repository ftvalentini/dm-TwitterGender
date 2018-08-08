# separa en train+validation y test

# librerias y funciones ---------------------------------------------------
source("src/funciones.R")
source("src/load_librerias.R")

# base de words -----------------------------------------------------------
base_w <- readRDS(file="data/working/base_words.rds") %>% 
  dplyr::mutate_if(is.character, as.factor)  

# test - train - validation -----------------------------------------------
set.seed(1)
N <- nrow(base_w); n_t <- 500
test_i <- sample(N, size=n_t, rep=F)
train_i <- setdiff(1:N, test_i)
# test:
base_test <- base_w[test_i, ]
# train + validation:
base_tv <- base_w[-test_i, ]
  # guarda ambas en data/final:
saveRDS(base_test, file="data/final/base_test.rds")
saveRDS(base_tv, file="data/final/base_train_validation.rds")


