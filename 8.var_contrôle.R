######################################################################################################################
# 
#                        Construction des variables de contrôle pour le DML
#
#######################################################################################################################

library(ggplot2)
library(arrow)
library(tidyverse)
# ========================= Importation des données de Pôle emploi et restriction à notre champ ==========================


#setwd("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/FH")
#de = read_sas("de_tempo.sas7bdat")
#d2 = read_sas("d2_tempo.sas7bdat")
#e0 = read_sas("e0_tempo.sas7bdat")
#e3 = read_sas("e3_tempo.sas7bdat")
#e3cons = read_sas("e3cons_tempo.sas7bdat")

#de<-de%>% filter( id_force%in% id_champ)
#d2<-d2%>% filter( id_force%in% id_champ)
#e0<-e0%>% filter( id_force%in% id_champ)
#e3<-e3%>% filter( id_force%in% id_champ)
#e3cons<-e3cons%>% filter( id_force%in% id_champ)
#write_parquet(de,"C:/Users/Public/Documents/Lyna_Clement/data/de_restreint_champ.parquet")
#write_parquet(d2,"C:/Users/Public/Documents/Lyna_Clement/data/d2_restreint_champ.parquet")
#write_parquet(e0,"C:/Users/Public/Documents/Lyna_Clement/data/e0_restreint_champ.parquet")
#write_parquet(e3,"C:/Users/Public/Documents/Lyna_Clement/data/e3_restreint_champ.parquet")
#write_parquet(e3cons,"C:/Users/Public/Documents/Lyna_Clement/data/e3cons_restreint_champ.parquet")


setwd("C:/Users/Public/Documents/Lyna_Clement/data/fh_raw/")
de <-read_parquet("de_restreint_champ.parquet")
d2<-read_parquet("d2_restreint_champ.parquet")
e0<-read_parquet("e0_restreint_champ.parquet")
e3<-read_parquet("e3_restreint_champ.parquet")
e3cons<-read_parquet("e3cons_restreint_champ.parquet")
names(de) = tolower(names(de))



setwd("C:/Users/Public/Documents/Lyna_Clement/data/")
licencies <-read_parquet("licencies_variable_interet.parquet")

# =================================================== Construction des variables de contrôle intéressantes ====================================================
######################### DEPUIS MMO #####################

base_X<-licencies%>% mutate(
  
    #age
  age_licenciement = as.integer(year(finctt_corr)-annee_naissance), 
  tranche_age = cut(age_licenciement, 
                    breaks = c(0, 25, 35, 45, 55, 65, Inf), 
                    labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+"), 
                    right = FALSE), 
  
  #la pcs
  pcs_groupe = case_when(
    substr(pcsese, 1, 1)== "1"~"Agriculteur", 
    substr(pcsese, 1, 1)== "2"~"Artisan/commerçant",
    substr(pcsese, 1, 1)== "3"~"Cadres",
    substr(pcsese, 1, 1)== "4"~"Profs intermédiaires",
    substr(pcsese, 1, 1)== "5"~"Employes", 
    substr(pcsese, 1, 1)== "6"~"Ouvriers",
    TRUE ~ "Autre"
  ), 
  
  #francilien ou non
  idf = ifelse(substr(cp_pref, 1, 2)%in% c("75", "77", "78", "91", "92", "93", "94", "95"), 1L, 0L), 
  
  #bin de salaire conditionnée à quali_salaire fiable
  salaire_fiable = case_when(quali_salaire_base %in% c("7") ~ TRUE, 
                             TRUE ~ FALSE), 
  salaire_bin = case_when(
    !salaire_fiable ~ NA_character_, 
    salaire_base<1500 ~ "A", 
    salaire_base<2000 ~ "B", 
    salaire_base<2500 ~ "C", 
    salaire_base<3000 ~ "D", 
    salaire_base<4000 ~ "E", 
    salaire_base>=4000 ~ "F", 
    TRUE ~ NA_character_
  ), 
  
  
)%>% select(id_force, age_licenciement, tranche_age, pcs_groupe, modeexercice, salaire_fiable, idf, type_layoff, duree_contrat_j) #on ne garde pa salaire_bin, trop de NA



######################### DEPUIS FH ###########################################################################################################
#On regarde d'abord les variables dispo sur Pôle emploi

de_merge<-de%>% left_join(licencies %>% select(id_force, finctt_corr), by = "id_force")%>%
  mutate(
    finctt_corr = as.Date(finctt_corr))

de_merge %>% mutate(periode_de = case_when( datins<finctt_corr ~ "AVANT licenciement", 
                                            datins>=finctt_corr~ "APRES licenciement", 
                                            TRUE ~ "date manquante"))%>% count(periode_de)%>% mutate(pct = round(n/sum(n)*100, 1))%>% print()

de_apres <-de_merge %>% filter(datins>=finctt_corr)
ids_inscrits_pe<-de_apres %>%distinct(id_force) # 9757 personnes ne se sont pas inscrites à PE

carac_indiv<-de_apres%>% group_by(id_force)%>% slice_min(datins, n = 1, with_ties = FALSE)%>% ungroup()%>%
  mutate(
    
    #sexe de la personne
    femme = if_else(sexe == "2", 1L, 0L), 
    
    #Nombre d enfants
    nenf = as.integer(nenf), 
    
    #Nationalite
    francais = ifelse(nation == "01", 1L, 0L), 
    
  )%>% select(id_force, femme, nenf, sitmat, francais, diplome, qualif, zrr, qpv, rma)



de_avant<-de_merge%>%filter(datins<finctt_corr)%>% group_by(id_force)%>% summarise(deja_pe = 1L, 
                                                                                   n_episodes = n(), 
                                                                                   .groups = "drop")


carac_indiv<-carac_indiv%>% left_join(de_avant, by = "id_force")%>% mutate(deja_pe = replace_na(deja_pe, 0L), 
                                                                           n_episodes = replace_na(n_episodes, 0L)) #on récupère l'info sur les epxériences de chômage passées

#Quelques statistiques sur les personnes qui ne se sont pas inscrites à pôle emploi apres leur licenciement 
non_inscrits<-licencies%>% filter(!id_force%in% ids_inscrits_pe$id_force)

base<-licencies%>% mutate(groupe = if_else(id_force %in% ids_inscrits_pe$id_force, "Inscrits_pe", "non inscrits pe"), 
                          age_licenciement = as.integer(year(finctt_corr)-annee_naissance))

base<-base %>%
  group_by(groupe)%>% summarise(
    age_moy = mean(age_licenciement, na.rm = TRUE), 
   n = n(),
 pct_form = mean(sum(sum_form>0)),
 pct_emp = mean(sum(emploi_stable_24m>0)),
       .groups = "drop")%>%print()






######################### On étudie le support commun  ###########################################################################################
X_complet<-base_X%>% left_join(carac_indiv, by = "id_force")%>% left_join(licencies %>% select(id_force, traite, emploi_stable_24m, censure), by = "id_force")

X_complet<-X_complet %>% filter(id_force %in% ids_inscrits_pe$id_force) #on ne garde que les gens qui se sont inscrit à PE apres leur licenciement (voir rapport )

X_complet %>% summarise(across(everything(), ~class(.)))%>% pivot_longer(everything(), 
                                                                        names_to = "variable", 
                                                                        values_to = "type")%>% print(n = Inf) #avant de lancer le modèle, je regarde la catégorie de chacune des variables

X_complet %>% summarise(across(everything(), ~sum(is.na(.))))%>% pivot_longer(everything(), 
                                                                         names_to = "variable", 
                                                                         values_to = "n_na")%>% print(n = Inf) #avant de lancer le modèle, je regarde la catégorie de chacune des variables


X_complet<-X_complet%>% 
  mutate(
  
    #On commence par traiter le cas des variables catégorielles
    pcs_groupe = as.factor(pcs_groupe), 
    modeexercice = as.factor(modeexercice), 
    type_layoff = as.factor(type_layoff), 
    sitmat = as.factor(sitmat), 
    diplome = as.factor(diplome), 
    qualif = as.factor(qualif), 
    zrr = as.factor(zrr), 
    qpv = as.factor(qpv), 
    rma = as.factor(rma), 
    
    #logical deviennent integer
    salaire_fiable = as.integer(salaire_fiable), 
    censure = as.integer(censure), 
   
     traite = case_when(
       traite == "Non formé" ~0L, 
       traite == "Formé" ~ 1L
     )
    
  )
formule_ps<-as.formula(paste("traite ~", paste(c("age_licenciement", "tranche_age", "pcs_groupe", "modeexercice", "salaire_fiable",  "type_layoff", "duree_contrat_j","femme", "idf", 
                                                 "nenf", "sitmat", "francais", "diplome", "qualif", "zrr", "qpv", "rma", "deja_pe", "n_episodes" ), collapse = " + ")))


ps_model<-glm(formule_ps, 
              data = X_complet, 
              family = binomial(link ="logit"))


X_complet<-X_complet %>% mutate(ps = predict(ps_model, type = "response"))

#Graphique 1
ggplot(X_complet, aes(x = ps, fill = factor(traite)))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(
    values = c("#4678CF", "#D65"), 
    labels = c("Non formés", "Formés")
  )+ 
  geom_vline(xintercept = c(0.05, 0.95), 
             linetype = "dashed", color = "black")+ 
  labs ( title = "Distribution du propensity score", 
         x = "P(formé|X", 
         y = "Densité", 
         fill = NULL ) + 
  theme_minimal()

p_score<-ggplot(X_complet, aes(x = ps, fill = factor(traite)))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(
    values = c("#4678CF", "#D65"), 
    labels = c("Non formés", "Formés")
  )+ 
  geom_vline(xintercept = c(0.05, 0.95), 
             linetype = "dashed", color = "black")+ 
  labs ( title = "Distribution du propensity score", 
         x = "P(formé|X", 
         y = "Densité", 
         fill = NULL ) + 
  theme_minimal()


ggsave("C:/Users/Public/Documents/Lyna_Clement/data/mmo_liquidation/p_score.png", p_score, width = 10, height = 6, dpi=300)


#Graphique 2

write_parquet(X_complet,"C:/Users/Public/Documents/Lyna_Clement/data/base_finale.parquet")












