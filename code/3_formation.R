################################################################################
#  Ajout des informations sur la formation suivie
#
################################################################################


library(haven)
setwd("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/BREST")


###############################################################################
#  Importation des bases BREST et champ de l etude
###############################################################################
brest_2023 = read_sas("brest2_23t4_force.sas7bdat")
brest_2022 = read_sas("brest2_22t4_force.sas7bdat")
brest_2021 = read_sas("brest2_21_v12.sas7bdat")
brest_2017_20 = read_sas("brest2_1720_v12.sas7bdat")
names(brest_2023) = tolower(names(brest_2023))
names(brest_2022) = tolower(names(brest_2022))
names(brest_2021) = tolower(names(brest_2021))
names(brest_2017_20) = tolower(names(brest_2017_20))




setwd("C:/Users/Public/Documents/Lyna_Clement/data/mmo_liquidation/")
licencies <-read_parquet("licencies_stables_faillite_18_23.parquet")

###########################################################################################################################################
#  Construction d'indicatrice de formation
###########################################################################################################################################

# On crée des indicateurs de formation suivie par année
#2023
formation = licencies %>%
  left_join(brest_2023 %>% mutate(formation_2023 = 1) %>% select(id_force,formation_2023)
            , by = "id_force") %>% 
  mutate(formation_2023 = ifelse(is.na(formation_2023),0,formation_2023))
#2022
formation = formation %>%
  left_join(brest_2022 %>% mutate(formation_2022 = 1) %>% select(id_force,formation_2022)
            , by = "id_force") %>% 
  mutate(formation_2022 = ifelse(is.na(formation_2022),0,formation_2022))
#2021
formation = formation %>%
  left_join(brest_2021 %>% mutate(formation_2021 = 1) %>% select(id_force,formation_2021)
            , by = "id_force") %>% 
  mutate(formation_2021 = ifelse(is.na(formation_2021),0,formation_2021))

#2017 à 2020 : La base est différente pour ces années : on a une date d'entrée plutot qu'une variable par année. 
#Donc on extrait l'année, puis on pivote pour créer les mêmes colonnes. 
names(brest_2017_20) = tolower(names(brest_2017_20))
brest_2017_20_retraite = brest_2017_20 %>%
  #slice_sample(n = 1000) %>%
  mutate(annee_formation = format(date_entree,"%Y"),
         val = 1) %>%
  pivot_wider(
    names_from = annee_formation,
    names_prefix = "formation_",
    values_from = val,
    values_fill = 0
  ) %>%
  select("id_force","formation_2017","formation_2018","formation_2019","formation_2020")

brest_2017_20_retraite = brest_2017_20_retraite %>%
  mutate(
    sum_form = formation_2017 + formation_2018+ formation_2019+ formation_2020
  )
#Attention on a toujours une ligne par personne, par année
brest_2017_20_retraite%>% filter(id_force == "FORCE0002502467")
#On corrige
brest_2017_20_retraite_clean<-brest_2017_20_retraite%>% group_by(id_force)%>%
  summarise(formation_2017 = max(formation_2017, na.rm = TRUE), 
            formation_2018 = max(formation_2018, na.rm = TRUE),
            formation_2019 = max(formation_2019, na.rm = TRUE),
            formation_2020 = max(formation_2020, na.rm = TRUE), 
            sum_form = as.integer(
              max(formation_2017, na.rm = TRUE)+ max(formation_2018, na.rm = TRUE)+max(formation_2019, na.rm = TRUE)+
                max(formation_2020, na.rm = TRUE)
            ), 
            .groups = "drop")

#Enfin on assemble toutes les années, on calcule le nombre de formations par années puis on repasse au format long
formation = formation %>%
  #select(-c(formation_2017,formation_2018, formation_2019, formation_2020))
  left_join(brest_2017_20_retraite_clean, by = "id_force") 

formation_final = formation %>%
  mutate(formation_2020 = ifelse(is.na(formation_2020),0,formation_2020),
         formation_2019 = ifelse(is.na(formation_2019),0,formation_2019),
         formation_2018 = ifelse(is.na(formation_2018),0,formation_2018),
         formation_2017 = ifelse(is.na(formation_2017),0,formation_2017)
         ) %>%
  mutate(
    sum_form = formation_2017 + formation_2018+ formation_2019+ formation_2020 + formation_2021 + formation_2022 + formation_2023
  ) %>%
  select("id_force","formation_2017" , "formation_2018","formation_2019",
         "formation_2020","formation_2021","formation_2022",
         "formation_2023") %>%
  pivot_longer(
    cols = c("formation_2017" , "formation_2018","formation_2019",
             "formation_2020","formation_2021","formation_2022",
             "formation_2023"),
    names_to = "annee"
  ) %>%
  mutate(
    annee = substr(annee,11,14),
    formation = value,
    value = NULL
  )

###############################################################################################################################
#  Merge avec les données de contrat
##############################################################################################################################

#On passe au format large, et on ne retient que les formations suivies après notre evenement
formation_final_post<-formation_final%>%left_join(licencies%>% select(id_force, finctt_corr), by = "id_force")%>%
  mutate(finctt_corr = as.Date(finctt_corr), 
         annee = as.integer(annee), 
         annee_licenciement = year(finctt_corr), 
         formation = case_when(annee>annee_licenciement ~formation, 
                               annee<annee_licenciement ~ 0L, 
                               annee == annee_licenciement ~ formation))%>%
  select(-finctt_corr, -annee_licenciement)



formation_wide<-formation_final_post%>% pivot_wider(names_from = annee, 
                                               names_prefix = "formation_", 
                                               values_from = formation, 
                                               values_fill = 0,
                                               values_fn = max)

formation_wide %>% count(id_force)%>% filter(n>1)%>% nrow()%>% cat("doublous", ., "\n")
# On merge !! 
df<-licencies%>% left_join(formation_wide, by = "id_force")
df<-df %>% mutate(across(starts_with("formation_"), ~replace_na(.,0)), 
                  sum_form = formation_2017 + formation_2018 + formation_2019+
                    formation_2020 + formation_2021 + formation_2022 + formation_2023 )

###############################################################################
#  Quelques statistiques descriptives
###############################################################################
library(ggplot2)
theme_sns<-theme_minimal(base_size = 13)+ 
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5, color = "grey50"), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.title = element_text(face = "bold"), 
    plot.background = element_rect(fill = "white", color = NA))
  

df_form<-df%>% mutate(cat_formation = case_when(sum_form == 0 ~ "Aucune formation", 
                                                sum_form == 1 ~ "1 formation", 
                                                sum_form >= 2~ "2 formations ou plus"), 
                      cat_formation = factor(cat_formation, 
                                             levels = c("Aucune formation", 
                                                        "1 formation", 
                                                        "2 formations ou plus")))

p1<-df_form %>% count(cat_formation)%>% mutate(pct = n/sum(n)*100)%>%
  ggplot(aes(x = cat_formation, y = n, fill=cat_formation))+
  geom_col(width=0.6, show_legend = FALSE)+
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")),
            vjust=-0.5, size = 4, frontface = "bold")+
  scale_fill_manual(values = c("#4678CF", "#6ACC65", "#D65"))+
  scale_y_continuous(expand=expansion(mult=c(0, 0.15)))+
  labs(x = NULL, 
       y = "Nombre d'individus")+
  theme_sns
ggsave("formation.png", p1, width = 10, height = 6, dpi=300)



p2<-df_form %>%
  group_by(type_layoff, cat_formation)%>% 
  summarise(n=n(), .groups = "drop")%>%
  group_by(type_layoff)%>%
  mutate(pct = n/sum(n)*100)%>%
  ungroup()%>%
  ggplot(aes(x = cat_formation, y = n, fill=cat_formation))+
  geom_col(width=0.6, show_legend = FALSE)+
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")),
            vjust=-0.5, size = 4, frontface = "bold")+
  scale_fill_manual(values = c("#4678CF", "#6ACC65", "#D65"))+
  scale_y_continuous(expand=expansion(mult=c(0, 0.15)))+
  facet_wrap(~type_layoff, scales = "free_y")+
  labs(x = NULL, 
       y = "Nombre d'individus")+
  theme_sns


ggsave("C:/Users/Public/Documents/Lyna_Clement/data/mmo_liquidation/formation_layoff_type.png", p2, width = 10, height = 6, dpi=300)

write_parquet(df,"licencies_stables_faillite_18_23_avec_formation.parquet")


###########################################################################################################
#  Ajout des variables de la base FH
#
###########################################################################################################

setwd("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/FH")
#p3 = read_sas("p2_tempo.sas7bdat")
#Ajout des formations de PE
p2 = read_sas("p2_tempo.sas7bdat")
licencies <-read_parquet("C:/Users/Public/Documents/Lyna_Clement/data/mmo_liquidation/licencies_stables_faillite_18_23_avec_formation.parquet")

id_champ<-licencies%>%pull(id_force)

p2<-p2%>% filter( id_force%in% id_champ)

p2_new<-p2%>% mutate(
  annee_formation = year(as.Date(P2DATDEB))
)%>% 
  inner_join(licencies%>% select(id_force, finctt_corr), by = "id_force")%>%
  filter(annee_formation>=year(finctt_corr))


p2_new_wide<-p2_new%>% mutate(val=1)%>%group_by(id_force, annee_formation)%>%
  summarise(val = max(val), .groups = "drop")%>%
  pivot_wider(names_from = annee_formation, 
              names_prefix = "p2_", 
              values_from = val, 
              values_fill = 0)
p2_new_wide %>% count(id_force)%>% filter(n>1)%>% nrow()%>% cat("doublous", ., "\n")

licencies_new_formation<-licencies%>%
  left_join(p2_new_wide, by = "id_force")%>%
  mutate(across(starts_with("p2_"), ~replace_na(., 0)))

licencies_new_formation<- licencies_new_formation%>%
  mutate( 
    formation_2019 = pmax(formation_2019, p2_2019, na.rm = TRUE),
    formation_2020 = pmax(formation_2020, p2_2020, na.rm = TRUE), 
    formation_2021 = pmax(formation_2021, p2_2021, na.rm = TRUE),
    formation_2022 = pmax(formation_2022, p2_2022, na.rm = TRUE),
    formation_2023 = pmax(formation_2023, p2_2023, na.rm = TRUE),
    
    #Origine
    in_brest = (formation_2017 + formation_2018 + formation_2019 + formation_2020+
                  formation_2021+formation_2022+ formation_2023)>0, 
    
    
    in_p2 = ( p2_2019 + p2_2020 + p2_2021 + p2_2022 + p2_2023)>0, 
    
    origine_formation = case_when(in_brest & in_p2 ~ "brest et fh", 
                                  in_brest & !in_p2 ~ "brest", 
                                  !in_brest & in_p2 ~ "fh", 
                                  TRUE ~ "aucune"), 
    
    sum_form = formation_2017 + formation_2018 + formation_2019 + formation_2020+
      formation_2021+formation_2022+ formation_2023
    
    
  )%>% 
  select(-starts_with("p2_"), -in_brest, -in_p2)

#####################################################################################################################################################################
#  Quelques statistiques descriptives (mises à jour)
###############################################################################~#####################################################################################
library(ggplot2)
theme_sns<-theme_minimal(base_size = 13)+ 
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5, color = "grey50"), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.title = element_text(face = "bold"), 
    plot.background = element_rect(fill = "white", color = NA))


df_form<-licencies_new_formation%>% mutate(cat_formation = case_when(sum_form == 0 ~ "Aucune formation", 
                                                                     sum_form == 1 ~ "1 formation", 
                                                                     sum_form >= 2~ "2 formations ou plus"), 
                                           cat_formation = factor(cat_formation, 
                                                                  levels = c("Aucune formation", 
                                                                             "1 formation", 
                                                                             "2 formations ou plus")))

p3<-df_form %>% count(cat_formation)%>% mutate(pct = n/sum(n)*100)%>%
  ggplot(aes(x = cat_formation, y = n, fill=cat_formation))+
  geom_col(width=0.6, show_legend = FALSE)+
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")),
            vjust=-0.5, size = 4, frontface = "bold")+
  scale_fill_manual(values = c("#4678CF", "#6ACC65", "#D65"))+
  scale_y_continuous(expand=expansion(mult=c(0, 0.15)))+
  labs(x = NULL, 
       y = "Nombre d'individus")+
  theme_sns
ggsave("formation_avp2.png", p3, width = 10, height = 6, dpi=300)



p4<-df_form %>%
  group_by(type_layoff, cat_formation)%>% 
  summarise(n=n(), .groups = "drop")%>%
  group_by(type_layoff)%>%
  mutate(pct = n/sum(n)*100)%>%
  ungroup()%>%
  ggplot(aes(x = cat_formation, y = n, fill=cat_formation))+
  geom_col(width=0.6, show_legend = FALSE)+
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")),
            vjust=-0.5, size = 4, frontface = "bold")+
  scale_fill_manual(values = c("#4678CF", "#6ACC65", "#D65"))+
  scale_y_continuous(expand=expansion(mult=c(0, 0.15)))+
  facet_wrap(~type_layoff, scales = "free_y")+
  labs(x = NULL, 
       y = "Nombre d'individus")+
  theme_sns

ggsave("formation_avp2_AL.png", p4, width = 10, height = 6, dpi=300)
write_parquet(licencies_new_formation,"C:/Users/Public/Documents/Lyna_Clement/data/licencies_stables_faillite_18_23_avec_formation_et_p2.parquet")

################################################################################################################################################
# On récupère les dates des formations
################################################################################################################################################
id_champ<-licencies%>%pull(id_force)
licencies<-licencies%>%mutate(finctt_corr=as.Date(finctt_corr))
#D'abord dans BREST
brest_2017_20<-brest_2017_20%>% filter( id_force%in% id_champ) %>%   select(all_of(c("id_force", "date_entree", "date_fin")))
brest_2021<-brest_2021%>% filter( id_force%in% id_champ) %>%   select(all_of(c("id_force", "date_entree", "date_fin")))
brest_2022<-brest_2022%>% filter( id_force%in% id_champ) %>%   select(all_of(c("id_force", "date_entree", "date_fin")))
brest_2023<-brest_2023%>% filter( id_force%in% id_champ)%>%   select(all_of(c("id_force", "date_entree", "date_fin")))

ens_formation<-bind_rows(brest_2017_20%>% mutate(source = "brest_2017_20"), 
                        brest_2021%>%  mutate(source = "brest_2021"), 
                        brest_2022 %>% mutate(source = "brest_2022"), 
                        brest_2023%>% mutate(source = "brest_2023"))%>% 
  select(all_of(c("id_force", "date_entree", "date_fin", "source")))

formation_post<-ens_formation%>% inner_join(licencies%>% select(id_force, finctt_corr), by = "id_force")
formation_post<-formation_post%>%mutate(finctt_corr=as.Date(finctt_corr))%>% filter(date_entree>=finctt_corr)%>% arrange(id_force, date_entree)
formation_post<-formation_post%>% filter(!is.na(date_fin))%>% select(id_force, date_entree_form = date_entree, date_fin_form = date_fin, finctt_corr)


#Puis dans PE

setwd("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/FH")
#Ajout des formations de PE
p2 = read_sas("p2_tempo.sas7bdat")
p2<-p2%>% filter( id_force%in% id_champ)
formation_p2<-p2%>%mutate(source = "p2")%>%select(id_force, date_entree_form = P2DATDEB,date_fin_form = P2DATFIN, source)
formation_p2<-formation_p2%>% inner_join(licencies%>% select(id_force, finctt_corr), by = "id_force")%>% mutate(finctt_corr=as.Date(finctt_corr))
formation_p2<-formation_p2%>%filter(date_entree_form>=finctt_corr)%>% arrange(id_force, date_entree_form)
formation_p2<-formation_p2%>% filter(!is.na(date_fin_form))

periode_formation<-bind_rows(formation_post, formation_p2)

write_parquet(periode_formation,"C:/Users/Public/Documents/Lyna_Clement/data/periode_formation.parquet")


