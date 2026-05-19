############################################################################################
#                           DML
#
#############################################################################################



#install.packages(
#  c(#"DoubleML", 
#    "mlr3", 
#    "mlr3learners", 
#    "data.table", 
#    "ranger", 
#    "glmnet", 
#    "nnet", 
#    "rpart", 
#    "gbm", 
#     "recipes"  )
#)

library(DoubleML)
library(data.table)
library(mlr3)
library(mlr3learners)
library(arrow)
library(recipes)
library(xgboost)
############################### Importation de la base de données finale ##############################

setwd("C:/Users/Public/Documents/Lyna_Clement/data/")
data <-read_parquet("base_finale.parquet")



###################### Data preparation ####################################################################
#setting controls 
X<-c("age_licenciement", "tranche_age", "pcs_groupe", "modeexercice", "salaire_fiable",  "type_layoff", "duree_contrat_j","femme", "idf",
              "nenf", "sitmat", "francais", "diplome", "qualif", "zrr", "qpv", "rma", "deja_pe", "n_episodes" )


#outcome
Y<-"emploi_stable_24m"

#treatment
D<-"traite"

#All cases should be completed
vars_needed<-c(Y, X, D)
data_dml<-data[complete.cases(data[, vars_needed]), ]


#I create feature engeniered
categorical_vars<-c("pcs_groupe","modeexercice", "type_layoff", "sitmat", "diplome", "qualif", 
                    "zrr", "qpv", "rma")

data_dml[categorical_vars]<-lapply(data_dml[categorical_vars], factor)


recipe_fe<-recipe("emploi_stable_24m" ~., 
                 data = data_dml[, c(X, "emploi_stable_24m")])%>%
  step_poly(age_licenciement, degree = 2)%>%
  step_poly(duree_contrat_j, degree = 2)%>% 
  step_dummy(all_nominal_predictors())%>%
  step_normalize(all_numeric_predictors())


prep_rec<-prep(recipe_fe)
X_mat<-bake(prep_rec, new_data = NULL)


data_dml<-cbind(data_dml[, c("emploi_stable_24m","traite" )], X_mat)

names(data_dml)[duplicated(names(data_dml))]%>%print()
cols_Xmat<-colnames(X_mat)[!colnames(X_mat)%in% c("emploi_stable_24m", "traite")]
names(data_dml)%>%print()
new_data_dml<-data.frame(y = data_dml$emploi_stable_24m, 
                         d = data_dml$traite, 
                         X_mat)
new_cols_xmat<-colnames(X_mat)
#Creating the dml object
dml<-DoubleMLData$new(
  data = new_data_dml, 
  y_col = "y", 
  d_col = "d", 
  x_cols = new_cols_xmat
)



###################### Double ML object ####################################################################


#introduce and  store models for learners
learner_g<-list(
  
  "lasso" = lrn("classif.cv_glmnet", alpha = 1), 
  "ridge" = lrn("classif.cv_glmnet", alpha = 0), 
  "RF" = lrn("classif.ranger", num.trees = 500, max.depth = 5), 
  "xgboost" = lrn("classif.xgboost", nrounds = 100, max_depth = 4, eta = 0.1)
)
learner_m<-list(
  
  "lasso" = lrn("classif.cv_glmnet", alpha = 1), 
  "ridge" = lrn("classif.cv_glmnet", alpha = 0), 
  "RF" = lrn("classif.ranger", num.trees = 500, max.depth = 5), 
  "xgboost" = lrn("classif.xgboost", nrounds = 100, max_depth = 4, eta = 0.1)
)

#Grid to search over combinations of nuisance parameters

results<-data.frame()
models<-list()

for(name_g in names(learner_g)){
  for (name_m in names(learner_m)){
    cat("Running combination:" , name_l, "/", name_m, "\n")
   
    tryCatch({
     dml_tmp<-DoubleMLIRM$new(
      dml, 
      ml_g = learner_g[[name_g]], 
      ml_m = learner_m[[name_m]], 
      n_folds = 5, 
      score = "ATE"
    )
    
    dml_tmp$fit(store_predictions = TRUE)
    
    #Résidus cross fittés
    g0_resid <-as.numeric(dml_tmp$predictions$ml_g0)
    g1_resid <-as.numeric(dml_tmp$predictions$ml_g1)
    m_resid <-as.numeric(dml_tmp$predictions$ml_m)
    
    #rmse
    rmse_g0<-sqrt(mean(g0_resid^2, na.rm = TRUE))
    rmse_g1<-sqrt(mean(g1_resid^2, na.rm = TRUE))
    rmse_g_mean<-(rmse_g1+rmse_g0)/2
    rmse_m<-sqrt(mean(m_resid^2, na.rm = TRUE))
    mse_m<-mean(m_resid^2, na.rm = TRUE)
   
    results<-rbind( results,
                   data.frame(
                     learner_g = name_g, 
                     learner_m = name_m, 
                     rmse_g_mean= round( rmse_g_mean, 4), 
                     rmse_g0= round( rmse_g0, 4),
                     rmse_g1 = round( rmse_g1, 4),
                     rmse_m = round(rmse_m, 4),
                     mse_m = round(mse_m, 4),
                     theta = round(dml_tmp$coef, 4), 
                     se = round(dml_tmp$se, 4), 
                     pval = round(dml_tmp$pval, 4)
                   ))
    
     models[[paste(name_g, name_m, sep = "__")]]<-dml_tmp
  }, error = function(e) {
    cat("Erreur pour", name_g, "/", name_m, ":", e$message, "\n" )
  })
  }
  
}

#Goodness of fit

print(results[order(results$rmse_g0, results$rmse_g1, 
                    results$rmse_m), ], 
      row.names = FALSE)


#On récupère le résultat du meilleur learner
best_g_name<-results%>% group_by(learner_g)%>% summarise(rmse_g_min = min(rmse_g_mean), .groups = "drop")%>%
  slice_min(rmse_g_min, n = 1)%>%
  pull(learner_g)


best_m_name<-results%>% group_by(learner_m)%>% summarise(rmse_m_min = min(rmse_m), .groups = "drop")%>%
  slice_min(rmse_m_min, n = 1)%>%
  pull(learner_m)


#Estimation finale 
dml_irm_best<-DoubleMLIRM$new(
  dml, 
  ml_g = learner_g[[best_g_name]],
  ml_m = learner_m[[best_m_name]],
  n_folds = 5, 
  score = "ATE"
  
)

dml_irm_best$fit(store_predictions = TRUE)
print(dml_irm_best$summary())
print(dml_irm_best$confint())


#tableau resume
ci_irm<-dml_irm_best$confint()
final_summary<-dataframe(
  model = "IRM", 
  best_g = best_g_name, 
  best_m = best_m_name, 
  theta = round(dml_irm_best$coef, 4), 
  se = round(dml_irm_best$se, 4), 
  ci_lo = round(dml_irm_best$ci_irm[1,1], 4), 
  ci_hi = round(dml_irm_best$ci_irm[1,2], 4), 
  pval= round(dml_irm_best$pval, 4), 
)
                 

cat("\n ===== Modele final =============")
print(final_summary, row.names = FALSE)



