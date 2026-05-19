################################################################################
#  Matching : entreprises liquidées
#
################################################################################
library("arrow")
library("tidyverse")
library("PanelMatch")
library("ggplot2")

###############################################################################
#  Importation de la base mmo, ajout des informations critiques (date du traitement)
###############################################################################
#Contrats associés aux entreprises ayant fait faillite entre 2017 et 2023
setwd("C:/Users/Public/Documents/Lyna_Clement/data/mmo_liquidation")
mmo_2018<-read_parquet("mmo_2018_disp_downsize_all_variables.parquet")
mmo_2019<-read_parquet("mmo_2019_disp_downsize_all_variables.parquet")
mmo_2020<-read_parquet("mmo_2020_disp_downsize_all_variables.parquet")
mmo_2021<-read_parquet("mmo_2021_disp_downsize_all_variables.parquet")
mmo_2022<-read_parquet("mmo_2022_disp_downsize_all_variables.parquet")
mmo_2023<-read_parquet("mmo_2023_disp_downsize_all_variables.parquet")

#entreprises ayant fait faillite entre 2017 et 2023
disp_ent<-read_parquet("C:/Users/Public/Documents/Lyna_Clement/data/disp_ent_17_23.parquet")
disp_ent_annee_depart<-disp_ent%>% select(siren, annee_disp)


###########################################################################################################################
# Passer au format long
###########################################################################################################################

# Maintenant, il faut crÃ©er pour chaque personne licenciÃ©e par une entreprise ayant Ã©tÃ© liquidÃ©e ou ayant eu un downsizing important (issue de la base de ClÃ©ment ) une ligne par annÃ©e de contrat (pdt les 3 derniÃ¨res annÃ©es qui prÃ©cÃ¨dent le licenciement; dc avant 2020)
# Pour chaque ligne il faut mettre Ã  jour les termes du contrat en rÃ©cupÃ©rant les donnÃ©es des bases MMO correspondantes et en utilisant l'identifiant des contrats L_contrat_sqn
# Il faut ajouter aussi les caractÃ©ristiques de la personnes qui ne changent pas (par hypothÃ¨se), issues de FH [prendre en compte Ã©galement les variations des contrÃ´les correspondants]
# et ajouter l'info de la formation pour l'annÃ©e correspondante

#Il faudra traiter les contrats qui sont NA pour finCTT alors que disp_ent est 2020 ou antérieure (en effet le NA ne peut pas s'expliquer simplement par la continuité du contrat)


#Filtrage des bases mmo
mmo_ent_liquidation<-function(base_mmo, annee){
  #fonction qui sélectionne les contrats de MMO concernés par une liquidation d'entreprise +/- proche
  #qui crée les groupes AL et IL
  
  disp_ent<-read_parquet("C:/Users/Public/Documents/Lyna_Clement/data/disp_ent_17_23.parquet")
  disp_ent<-disp_ent%>% select(siren, annee_disp)
  
  base_mmo %>% mutate(siren = substr(siret_af, 1, 9))%>% 
    left_join(disp_ent_annee_depart, by ="siren")%>% 
    filter(!is.na(annee_disp))%>% 
    mutate(finctt = as.Date(finctt, format = "%Y-%m-%d")) %>% 
    mutate(annee_finctt= year(finctt))%>% 
    mutate(type_layoff = case_when(  annee_finctt == annee_disp ~ "actual_layoff", 
                                    annee_finctt != annee_disp ~ "large_window_layoff"))%>%
    
    mutate(anne_source = annee)
  
}

annees<-2018:2023

for (annee in annees){
  base<-get(paste0("mmo_", annee))
  resultat<-mmo_ent_liquidation(base, annee)
  assign(paste0("mmo_", annee, "_liquidation"), resultat)
}
rm(resultat)
rm(mmo_2023)

#Concaténation
for (annee in annees){
  base<-paste0("mmo_", annee, "_liquidation")
  df<-get(base)%>%
    rename_with(~gsub(paste0("_", annee, "_"), "_", .x), starts_with(paste0("emploi_bit_", annee, "_")))
  assign(base, df)
  rm(df)
  rm(base)
}
vars_a_garder<-c("idsismmo", "h_salarie_sqn", "h_individu_sqn", "l_contrat_sqn", "id_force", "debutctt", "finctt", 
                 "motifrupture", "pcsese", "nature","disppolitiquepublique", "motifrecours", "siret_af", "modeexercice",
                 "annee_naissance", "cp_pref", "localite_pref", "catjuri_id", "salaire_base", "salaire_base_mois_complet", 
                 "quali_salaire_base", "siren", "annee_disp", "annee_finctt", "type_layoff", "anne_source", "emploi_bit_01","emploi_bit_02","emploi_bit_03",
                 "emploi_bit_04", "emploi_bit_05", "emploi_bit_06", "emploi_bit_07", "emploi_bit_08", "emploi_bit_09", 
                 "emploi_bit_10", "emploi_bit_11", "emploi_bit_12")
                


mmo_concat_liquidation<-bind_rows(lapply(2018:2023, function(annee){
  get(paste0("mmo_", annee, "_liquidation"))%>%
    select(all_of(vars_a_garder))
} ))


rm(mmo_2019)
rm(mmo_2018)
rm(mmo_2020)
rm(mmo_2021)
rm(mmo_2022)
rm(mmo_2023)

###########################################################################################################################
# Filtre et nettoyage des données
###########################################################################################################################

#ON FILTRE UNE PARTIE DES DONNEES !
#Il y a peut être des choix de filtrage à faire pour causes de : 
        #1. J'enlève de la base les personnes ayant connu un licenciment pour cause de liquidation d'entreprise antérieure à 2017
        #3. L'emploi n'est pas stable => J'enlève toutes les personnes non stables, ayant un modeexercice == 99
        #J'enlève les personnes ayant plus d'un contrat 2 avant avant le mass layoff; en revanche, apres le mass layoff, je conserve l'ensemble des formes de réinsertion
        # y compris, les trajectoires heurtées, le multi-emploi et les allers-retours vers l'emploi, car ce sont précisément les objets d'intérêt

mmo_concat_liquidation<-mmo_concat_liquidation%>% filter(annee_disp>=2018)


emploi_bit<-grep("^emploi_bit_", names(mmo_concat_liquidation), value = TRUE)
mmo_concat_liquidation<-mmo_concat_liquidation%>% mutate(nb_mois_manquants = rowSums(across(all_of(emploi_bit), ~ . == 0), na.rm = TRUE), 
                                                               statut_stabilite = if_else(nb_mois_manquants == 0, "stable", "instable"))
#mmo_pre_l<-mmo_concat_liquidation%>%filter(anne_source<finctt)%>% count(anne_source, statut_stabilite)
#instables<-mmo_concat_liquidation%>% filter(statut_stabilite == "instable")%>% select(id_force, annee_disp, modeexercice, disppolitiquepublique, motifrupture, siret_af, salaire_base)
                # Je vais enlever toutes les personnes ayant un parcours instable 

mmo_concat_liquidation<-mmo_concat_liquidation%>% filter(!(statut_stabilite == "instable")) # à corriger, uniquement avant le licenciement !!



#nettoyage
mmo_concat_liquidation<-mmo_concat_liquidation%>%
  mutate(debutctt=as.Date(debutctt), 
         finctt=as.Date(finctt), 
         
         annee_naissance = as.integer(annee_naissance), 
         duree_contrat_j = as.integer(finctt-debutctt)
         )


#je ne conserve qu'un contrat par individu, en sélectionnant en priorité le dernier contrat et le plus long
contrat_ref<-mmo_concat_liquidation%>% group_by(idsismmo)%>% arrange(desc(finctt), desc(duree_contrat_j))%>%
  slice(1)%>% ungroup()


#la base disp_ent a des coquilles, elle intègre probablement de entreprises qui ont fermées mais ont été rachetées (fusions acquisitions)
contrats_suspects<-contrat_ref%>% filter(annee_finctt>annee_disp)
cat("Diagnostic des conntrats suspects (entreprises n'ayant pas vraiment fermé")
cat("Nombre de contrats suspects :", nrow(contrats_suspects), "\n")
cat("Nombre de personnes concernées :", n_distinct(contrats_suspects$idsismmo), "\n")
cat("Nombre d'entreprises concernées :", n_distinct(contrats_suspects$siren), "\n")

#Analyser les siren problématiques
diagnostic_siren<-contrats_suspects%>% group_by(siren, annee_disp)%>% summarise(
  n_indiv = n_distinct(idsismmo), 
  n_contrats = n(), 
  annee_fin_ctt_min = min(annee_finctt, na.rm = TRUE), 
  annee_fin_ctt_max = max(annee_finctt, na.rm = TRUE), 
  ecart_max_annees = max(annee_finctt-annee_disp, na.rm = TRUE), 
  .groups = "drop"
)%>% arrange(desc(ecart_max_annees))

cat("===== Diagnostic siren ===========")
print(diagnostic_siren, n = Inf)

cat("===== Distribution des écarts (fin de contrat - annee de liquidation de l'entreprise) ===========")
contrats_suspects%>% mutate(ecart = annee_finctt-annee_disp)%>% 
  count(ecart)%>%
  arrange(ecart)%>% print()

disp_ent_long<-disp_ent%>% pivot_longer(cols = starts_with("eff_"), 
                                        names_to = "annee", 
                                        names_prefix = "eff_", 
                                        values_to = "effectif")%>% mutate(annee = as.integer(paste0("20", annee)))
effectif_avant_disp<-disp_ent_long%>% group_by(siren)%>% filter(annee<annee_disp)%>% filter(!is.na(effectif))%>%slice_max(annee, n=1)%>% ungroup()%>% 
  select(siren, annee_eff_ref=annee, eff_avant_disp = effectif)

#diagnostic complet (en intégrant l'information sur les effectifs des entreprises concernées)
diagnostic_complet<-diagnostic_siren%>% left_join(effectif_avant_disp, by=c("siren"))%>%
  mutate(ratio_suspects = n_indiv/eff_avant_disp, 
         decision = case_when(ecart_max_annees>=2 ~"EXCLURE_SIREN", 
                              is.na(eff_avant_disp)&ecart_max_annees<=1 ~"EXCLURE_SIREN",
                              ecart_max_annees<=1 & ratio_suspects<0.05 ~ "PLAFONNER_FINCTT",
                              ecart_max_annees<=1 & ratio_suspects>=0.5 ~"EXCLURE_SIREN", 
                              ecart_max_annees<=1 & ratio_suspects>=0.05 & ratio_suspects<0.5 ~"EXCLURE_SIREN", 
                              TRUE ~"VERIFIER_MANUELLEMENT"
                              ))

cat("===== Résumé de décisions ===========")
diagnostic_complet%>% count(decision)%>%print()

#Application des décisions
siren_exclure <-diagnostic_complet%>% filter(decision=="EXCLURE_SIREN")%>%pull(siren)
siren_plafonner<-diagnostic_complet%>%filter(decision=="PLAFONNER_FINCTT")%>%pull(siren)


###########################################################################################################################
# Création d'une base individus
###########################################################################################################################

base_individus<-contrat_ref%>% filter(!siren %in% siren_exclure)%>% 
  mutate(finctt_corr = if_else(siren%in% siren_plafonner&annee_finctt>annee_disp, 
                               as.Date(paste0(annee_disp, "-12-31")), finctt), 
         flag_finctt_corr = siren %in%siren_plafonner&annee_finctt>annee_disp)


#J'exclus les personnes en intérim, en stage, je les repère grâce à la variable Motifrupture 
motifs_exclure<-c("032", #intérim
                  "081", #apprentissage
                  "034", #fin periode d'essai
                  "035", #pareil
                  "038", #mise à la retraite par l'employeur, pas concerné par le retour à l'emploi
                  "039", #pareil
                  "065", #décès employeur
                  "066", #décès employé
                  "998", #transfert sans rupture
                  "100") #mutation au sein du mm groupe

base_individus_filtre<-base_individus%>% filter(!motifrupture %in% motifs_exclure)
cat("\n======== BILAN ======= \n")
cat("Avant :", nrow(base_individus), "individus\n")
cat("Apres :", nrow(base_individus_filtre), "individus\n")

#===== Je filtre pour ne conserver que des contrats stables (1 an sans mois de non emploi au sens du BIT) =========

#Je calcule la fenêtre à partir de laquelle évaluer la période d'emploi
#date_fin_ref<-base_individus_filtre%>% select(idsismmo, l_contrat_sqn, finctt_corr)%>% mutate(date_fin = as.Date(finctt_corr), date_debut_fenetre = date_fin %m-% months(12))

#Je récupère les informations sur deux années calendaires
#cols_emploi_bit<-paste0("emploi_bit_", sprintf("%02d", 1:12))
#base_individus_filtre<-base_individus_filtre%>%
#  mutate(across(all_of(cols_emploi_bit), 
#                ~as.integer(unlist())))
         
base_individus_final<-base_individus_filtre%>% filter(duree_contrat_j>=365)
cat("\n======== BILAN ======= \n")
cat("Avant :", nrow(base_individus_filtre), "individus\n")
cat("Apres :", nrow(base_individus_final), "individus\n")



#Enregistrement de la base
write_parquet(base_individus_final,"licencies_stables_faillite_18_23.parquet")



























