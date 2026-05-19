######################################################################################################################
# 
#                        Construction de la variable d'intérêt pour le DML
#
#######################################################################################################################


library("haven")

# ========================= Importation des données de Mmo et restriction à notre champ ===================================================================


#setwd("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/FH")
#mmo_19<-read_sas("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/MMO/MMO_2_2019_F16.sas7bdat", col_select = c("id_force", "IdSISMMO", "L_Contrat_SQN", "DebutCTT", "FinCTT", "ModeExercice"))
#mmo_19<-mmo_19%>% filter( id_force%in% id_champ)
#write_parquet(mmo_19,"C:/Users/Public/Documents/Lyna_Clement/data/mmo_raw/mmo_19_restreint_champ.parquet")

#mmo_20<-read_sas("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/MMO/MMO_2_2020_F16.sas7bdat", col_select = c("id_force", "IdSISMMO", "L_Contrat_SQN", "DebutCTT", "FinCTT", "ModeExercice"))
#mmo_20<-mmo_20%>% filter( id_force%in% id_champ)
#write_parquet(mmo_20,"C:/Users/Public/Documents/Lyna_Clement/data/mmo_raw/mmo_20_restreint_champ.parquet")

#mmo_21<-read_sas("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/MMO/MMO_2_2021_F16.sas7bdat", col_select = c("id_force", "IdSISMMO", "L_Contrat_SQN", "DebutCTT", "FinCTT", "ModeExercice"))
#mmo_21<-mmo_21%>% filter( id_force%in% id_champ)
#write_parquet(mmo_21,"C:/Users/Public/Documents/Lyna_Clement/data/mmo_raw/mmo_21_restreint_champ.parquet")

#mmo_22<-read_sas("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/MMO/MMO_2_2022_F16.sas7bdat", col_select = c("id_force", "IdSISMMO", "L_Contrat_SQN", "DebutCTT", "FinCTT", "ModeExercice"))
#mmo_22<-mmo_22%>% filter( id_force%in% id_champ)
#write_parquet(mmo_22,"C:/Users/Public/Documents/Lyna_Clement/data/mmo_raw/mmo_22_restreint_champ.parquet")

#mmo23<-read_sas("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/MMO/MMO_2_F16_2023.sas7bdat", col_select = c("id_force", "IdSISMMO", "L_Contrat_SQN", "DebutCTT", "FinCTT", "ModeExercice"))
#mmo23<-mmo23%>% filter( id_force%in% id_champ)
#write_parquet(mmo23,"C:/Users/Public/Documents/Lyna_Clement/data/mmo_raw/mmo_23_restreint_champ.parquet")

#mmo_24<-read_sas("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/MMO/MMO_2024_F16_T32024.sas7bdat", col_select = c("id_force", "IdSISMMO", "L_Contrat_SQN", "DebutCTT", "FinCTT", "ModeExercice"))
#mmo_24<-mmo_24%>% filter( id_force%in% id_champ)
#write_parquet(mmo_24,"C:/Users/Public/Documents/Lyna_Clement/data/mmo_raw/mmo_24_restreint_champ.parquet")


setwd("C:/Users/Public/Documents/Lyna_Clement/data/mmo_raw/")
mmo_19 <-read_parquet("mmo_19_restreint_champ.parquet")
mmo_20<-read_parquet("mmo_20_restreint_champ.parquet")
mmo_21<-read_parquet("mmo_21_restreint_champ.parquet")
mmo_22<-read_parquet("mmo_22_restreint_champ.parquet")
mmo_23<-read_parquet("mmo_23_restreint_champ.parquet")
mmo_24<-read_parquet("mmo_24_restreint_champ.parquet")
names(mmo_19) = tolower(names(mmo_19))
names(mmo_20) = tolower(names(mmo_20))
names(mmo_21) = tolower(names(mmo_21))
names(mmo_22) = tolower(names(mmo_22))
names(mmo_23) = tolower(names(mmo_23))
names(mmo_24) = tolower(names(mmo_24))
    #Pour créer la variable d'intérêt, j'identifie les contrats dans mmo qui débutent après la date de licenciement finctt_corr
    #Je calcule la durée du contrat
    #Je considère qu'un emploi est stable s'il dure + de 180 jours et s'il est à temps plein (quand Modeexercice permet effectivement d'identifier ça)

setwd("C:/Users/Public/Documents/Lyna_Clement/data/")
licencies <-read_parquet("licencies_stables_faillite_18_23_avec_formation_et_p2.parquet")
# ========================= Création de la variable d'intérêt ===================================================================


#je concatène mes bases de contrat
mmo_post<-bind_rows(mmo_19, mmo_20, mmo_21, mmo_22, mmo_23, mmo_24)%>% mutate(debutctt =as.Date(debutctt), 
                                                                              finctt = as.Date(finctt))%>%
  filter(!is.na(debutctt))%>%
  group_by(l_contrat_sqn)%>%
  summarise(id_force = first(id_force), 
            idsismmo = first(idsismmo), 
            debutctt = min(debutctt, na.rm = TRUE), 
            finctt = suppressWarnings(max(finctt, na.rm = TRUE)),
            modeexercice = first(modeexercice), 
                         .groups = "drop") %>%
  mutate(finctt = if_else(is.infinite(finctt), as.Date(NA), finctt))#je ne retiens qu'un contrat par nom de contrats (en effet là, je peux avoir plusieurs mêmes contrats du fait qu'il soient actifs plusieurs années de suite)

#Je joins ces contrats à finctt_corr afin de ne conserver que les contrats post licenciement
mmo_post_lic<-mmo_post%>% inner_join(licencies%>% select(id_force, finctt_corr), by = "id_force")%>% filter(debutctt>finctt_corr)%>%
  mutate(finctt_obs = if_else(is.na(finctt), as.Date("2024-12-31"), finctt),
    
          duree_contrat_j = as.numeric(finctt)-as.numeric(debutctt), 
         contrat_actif = is.na(finctt))
  
cat("N contrats post licenciement :", n_distinct(mmo_post_lic$l_contrat_sqn), "\n")  
cat("N individus post licenciement :", n_distinct(mmo_post_lic$id_force), "\n") 
cat("Distribution de la duree des contrats :") 
mmo_post_lic%>% mutate(duree_contrat_j = as.numeric(duree_contrat_j))%>%
                         summarise(
                           total = n(),
                           n_actifs = sum(contrat_actif), 
                           n_termines = sum(!contrat_actif),
                           )%>% print()
  mmo_post_lic<-mmo_post_lic%>% filter(is.na(duree_contrat_j)|duree_contrat_j>=0) 
  

premier_contrat_stable <-mmo_post_lic%>% filter(is.na(duree_contrat_j)|duree_contrat_j>=180)%>% group_by(id_force)%>% slice_min(debutctt, n = 1, with_ties = FALSE)%>%
  ungroup()%>%
  select(id_force, date_debut_emploi_stable = debutctt, duree_emploi_stable_j = duree_contrat_j, contrat_actif)
  

setwd("C:/Users/Public/Documents/Lyna_Clement/data/")
periode_formation <-read_parquet("periode_formation.parquet")  

duree_formation_pertinente<-periode_formation %>% left_join(premier_contrat_stable%>% select(id_force, date_debut_emploi_stable), by = "id_force")%>%
                          filter(date_entree_form>=finctt_corr &
                                  (is.na(date_debut_emploi_stable)|date_entree_form<date_debut_emploi_stable))%>% 
                          group_by(id_force)%>% summarise(duree_totale_form_j = sum(as.numeric(date_fin_form-date_entree_form), na.rm = TRUE), 
                                                          .groups = "drop")
  
variable_interet<-licencies%>% select(id_force, finctt_corr)%>% left_join(duree_formation_pertinente, by = "id_force")%>% 
  mutate(duree_totale_form_j = replace_na(duree_totale_form_j, 0), 
         date_horizon = finctt_corr %m+% months(24)+days(duree_totale_form_j))


variable_interet<-variable_interet%>% left_join(premier_contrat_stable, by = "id_force")%>% 
  mutate(duree_retour_j = as.numeric(date_debut_emploi_stable)-as.numeric(finctt_corr), 
         emploi_stable_24m = case_when(!is.na(date_debut_emploi_stable)& as.numeric(date_debut_emploi_stable) <=as.numeric(date_horizon) ~1L, 
                                       TRUE ~ 0L), 
         censure = is.na(date_debut_emploi_stable) | as.numeric(date_debut_emploi_stable)>as.numeric(date_horizon))

variable_interet<-variable_interet%>% left_join(licencies, by=c("id_force", "finctt_corr"))%>% mutate(traite = if_else(sum_form>0, "Formé", "Non formé"))

cat("=== EMPLOI STABLE 24 MOIS =================")
variable_interet%>% count(emploi_stable_24m)%>% mutate(pct = round(n/sum(n)*100, 1))%>% print()
variable_interet%>% count(censure)%>% mutate(pct = round(n/sum(n)*100, 1))%>% print()

cat("=================== EMPLOI STABLE : formés contre non formés =====================")
variable_interet%>% group_by(traite)%>% summarise(n = n(), 
                                                  n_emploi_stable = sum(emploi_stable_24m==1), 
                                                  .groups = "drop")%>%print()



#Enregistrement de la base
write_parquet(variable_interet,"licencies_variable_interet.parquet")
  
  
  
  
  
  