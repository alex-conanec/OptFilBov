library(modvarsel)
data <- read.csv('data/indicateurs.csv', header = TRUE, sep = ';', dec = ',')
load('data/choiceModelsFinal.RData')
data <- scale(data)
summary(data)

model <- ridge(X = data[,-c(1,2,3)], Y = data[,1], Ylabel = colnames(data)[1])

predict(model, newdata = data[1,])
predict(model, newdata = data[1,], predict.all = TRUE, R=3)

load('data/modelsFifi.RData')

class(models[[8]])

res <- c()
for (i in 1:length(models)){
  res <- c(res, class(models[[i]]))
}

predict(model = models[[8]], newdata=data[1:3,], predict.all = T)

class(models[[8]])


#creation de model 100% rf
choix_model <- c('ridge', 'ridge', 'rf', 'rf', 'ridge', 'rf', 'rf', 'linreg', 'rf', 'rf', 'rf', 'rf',
                 'rf', 'rf', 'rf', 'rf', 'ridge', 'rf', 'ridge', 'rf', 'rf', 'rf', 'rf', 'rf')


models <- list()
for (i in 1:length(choiceModels)){

  Y <- data[, i]
  X <- data[,which(colnames(data) %in% rownames(choiceModels[[i]]$pvarsel))]
  Ylabel <- colnames(data)[i]
  method <- choix_model[i]
  imp <- varimportance(X = X, Y = Y, method = 'linreg', nperm = 100)
  sel <- which(colnames(data) %in% select(imp, cutoff=TRUE)$var)
  Xnew <- data[, sel]

  models[[i]] <- switch(method,
                        ridge = ridge(X = Xnew, Y = Y, Ylabel = Ylabel),
                        rf = rf(X = Xnew, Y = Y, Ylabel = Ylabel),
                        linreg = reg_lm(X = Xnew, Y = Y, Ylabel = Ylabel))

}
length(models)
library(mfe)
#add under models
for (i in 1:length(models)){
  if (class(models[[i]]) == 'reg_lm')  models[[i]]$all_models <- underModels.reg_lm(models[[i]], B = 100)

  if (class(models[[i]]) == 'rf')  models[[i]]$all_models <- underModels.rf(models[[i]], B = 100)

  if (class(models[[i]]) == 'sir')  models[[i]]$all_models <- underModels.sir(models[[i]], B = 100)

  if (class(models[[i]]) == 'ridge')  models[[i]]$all_models <- underModels.ridge(models[[i]], B = 100)

  if (class(models[[i]]) == 'pls_reg')  models[[i]]$all_models <- underModels.pls_reg(models[[i]], B = 100)
}


save(models, file = 'data/models_final_scale.RData')

getwd()
library(mfe)
data <- read.csv('data/indicateurs.csv', header = TRUE, sep = ';', dec = ',')
cow0 <- setNames(data.frame(matrix(0, ncol = ncol(data), nrow = 1)), colnames(data))
cow <- predict_cow(cow_simulated = cow0, models = models, list_index = 1:3, list_value = c(0, 1, -0.5), B=5, R=10)
plot(pred_cow, models, choice = 'radar_diag', ylim = c(-2, 2), main = 'Vache 2',
     label_angle = 45, fixed_value = 1:3, radial.lim = c(-3, 3), R = 3)

X=as.matrix(data[,-(1:3)])

a=reg_lm(Y=data[,7], X=data[,-(4:10)], Ylabel = colnames(data)[7])

predict(model=a, newdata = data[1,], predict.all = T)

summary(a)
sobol_sensitivity(a, order = 1, nboot = 10)
