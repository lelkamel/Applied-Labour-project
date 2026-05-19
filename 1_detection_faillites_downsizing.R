# Emp
setwd("//casd.fr/casdfs/Projets/ENSAE02/Data")

years_aimed = c(17:22)




# Etape 1 : Charger les REE
#for(i in 0:13){
#  var = ifelse(i %in% 0:9,paste("0",i,sep=""),as.character(i))
  
  
#  name_df = paste("ent_",var,sep= "")
#  name_cible = paste("REE_Stocks Entreprises_",2000+i,"/STENT0",i,".sas7bdat",sep="")
#  name_cible = ifelse(i %in% 0:9,paste("REE_Stocks Entreprises_",2000+i,"/STENT0",i,".sas7bdat",sep=""),paste("REE_Stocks Entreprises_",2000+i,"/stent",i,".sas7bdat",sep=""))
  
#  df = read_sas(name_cible, col_select = c("SIREN","EFF"))
#  names(df) = tolower(names(df))
  
#  df = df %>% filter(eff != 0)
#  assign(name_df,df) 
#}


for(i in 17:23){
  print(i)
  name_df = paste("ent_",i,sep= "")
  name_cible = paste("REE_Stocks Entreprises_",2000+i,"/STOCK_UL_",2000+i,".parquet",sep="")
  
  df = read_parquet(name_cible)
  names(df) = tolower(names(df))
  
  df = df %>% 
    select("siren", "eff" = "eff3112") %>% 
    filter(eff != 0) %>%
    mutate(year = 2000+i)
  
  assign(name_df,df) 
}

# Etape 2 : Trouver les entreprises ayant disparues.
for (i in 17:22) {
  var = ifelse(i %in% 0:9,paste("0",i,sep=""),as.character(i))
  var_1 = ifelse(i %in% 0:8,paste("0",i+1,sep=""),as.character(i+1))
  
  name_df = paste("fermeture_20",var,"_20",var_1,sep="")
  #name_check = paste("check_20",var,"_20",var_1,sep="")
  
  # 
  ent = get(paste("ent_",var,sep=""))
  ent_1 = get(paste("ent_",var_1,sep=""))
    
  df = ent %>% anti_join(ent_1, by = "siren") %>% arrange(desc(eff))
   
  assign(name_df,df)
}

# Etape 3 : Créer un dataframe de suivit des effectifs des entreprises.

ent_full = tibble()
for (i in c(17:22)) {
  print(i)
  
  j = ifelse(i %in% 0:9,paste("0",i,sep=""),as.character(i))
  
  ent_full = ent_full %>% bind_rows(get(paste("ent_",j,sep=""))%>% mutate(year = 2000+i))
}
ent_full = ent_full %>% 
  #arrange(siren) %>%
  group_by(siren) %>%
  mutate(
    annee_disp = max(year),
    eff_max = max(eff),
    var = (eff - lag(eff))/lag(eff) * 100#,
    #nb_baisse_30 = sum(var < 30)
  ) %>%
  filter(eff_max > 100)

#setwd("C:/Users/Public/Documents/Lyna_Clement/data")
#write_parquet(ent_full,"ent_full.parquet")
# ent_full = read_parquet("ent_full.parquet")
#setwd("//casd.fr/casdfs/Projets/ENSAE02/Data")

ent_full_v2 = ent_17 %>% select("siren","eff_17" = "eff")
for (i in c(18:22)) {
  print(i)
  
  j = ifelse(i %in% 0:9,paste("0",i,sep=""),as.character(i)) 
  
  ent_full_v2 = ent_full_v2 %>% full_join(get(paste("ent_",j,sep="")) %>% select("siren", "eff")  ,by = "siren", suffix = c("", paste("_",j,sep="") ))
} 
ent_full_v2 = ent_full_v2 %>% 
  rename("eff_01" = "eff") 
ent_full_v2 = ent_full_v2 %>%
  left_join(ent_full %>% select("siren","annee_disp") %>% distinct(), by = "siren")

#setwd("C:/Users/Public/Documents/Lyna_Clement/data")
#write_parquet(ent_full_v2,"historique_ent.parquet")
# ent_full_v2 = read_parquet("historique_ent.parquet")
#setwd("//casd.fr/casdfs/Projets/ENSAE02/Data")

disp_ent = ent_full_v2 %>%
  filter(annee_disp != 2022)

down_sizing = ent_full_v2 %>%
  left_join(ent_full %>% filter(eff > 100)%>%distinct() %>% select(-c("annee_disp")), by = "siren") %>%
  filter(eff_max > 100) %>%
  filter(var < -30)


#setwd("C:/Users/Public/Documents/Lyna_Clement/data")
#write_parquet(disp_ent,"disp_ent_17_23.parquet")
# disp_ent_17_23 = read_parquet("disp_ent_17_23.parquet")
#setwd("//casd.fr/casdfs/Projets/ENSAE02/Data")

#setwd("C:/Users/Public/Documents/Lyna_Clement/data")
#write_parquet(down_sizing,"ddown_sizing_17_23.parquet")
# down_sizing_17_23 = read_parquet("down_sizing_17_23.parquet")
#setwd("//casd.fr/casdfs/Projets/ENSAE02/Data")

# Etape 3 : Verifier si des entreprises disparéssent plusieurs fois

check = disp_ent %>%
  group_by(siren) %>%
  mutate(n_count = n()) %>%
  filter(n_count > 1) %>%
  arrange(desc(n_count))
###


rm(list = setdiff(ls(),c("disp_ent","down_sizing")))


# write_parquet(df,"mmo19.parquet")
setwd("C:/Users/ENSAE02_C_DANO000/Desktop/Data_trans")
mmo_19 = read_parquet("mmo19.parquet")
names(mmo_19) = tolower(names(mmo_19))
setwd("//casd.fr/casdfs/Projets/ENSAE02/Data")


setwd("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/MMO")
mmo_24 = read_sas("MMO_2024_F16_T32024.sas7bdat", col_select = c("IdSISMMO","id_force","MotifRupture","siret_AF","siret_ut","Nature","DebutCTT","FinCTT"))
names(mmo_24) = tolower(names(mmo_24)) # 9 minutes
#mmo_ = read_sas(,col_select = c())
df = mmo_24 %>%
  mutate(siren = substr(siret_af,1,9)) %>% 
  filter(siren %in% c(disp_ent$siren, down_sizing$siren))
#setwd("C:/Users/Public/Documents/Lyna_Clement/data")
#write_parquet(df,"mmo_24.parquet")
#setwd("//casd.fr/casdfs/Projets/ENSAE02/Data")

setwd("//casd.fr/casdfs/Projets/ENSAE02/Data/FORCE_FORCE_2024S2/MMO")

df = read_sas("MMO_2_2017_F16.sas7bdat", n = 100)
dg = read_sas("MMO_2_2017_F16.sas7bdat", col_select = "Salaire_Base")
dh = dg %>% filter(!is.na(Salaire_Base
                          ))
#names(df)
#[1] "IdSISMMO"                  "id_force"                  "H_Individu_SQN"            "L_Contrat_SQN"             "H_Salarie_SQN"            
#[6] "DebutCTT"                  "PreDSN"                    "DerDSN"                    "FinCTT"                    "MotifRupture"             
#[11] "PcsEse"                    "Nature"                    "MotifRecours"              "siret_AF"                  "ModeExercice"             
#[16] "H_Etab_SQN"                "mois_naissance"            "annee_naissance"           "siret_ut"                  "CP"                       
#[21] "Localite"                  "CP_Pref"                   "Localite_Pref"             "CATJURI_ID"                "secteur_PUBLIC"           
#[26] "Salaire_Base"              "salaire_base_mois_complet" "Quali_Salaire_Base"        "emploi_bit_2024_01"        "emploi_bit_2024_02"       
#[31] "emploi_bit_2024_03"        "emploi_bit_2024_04"        "emploi_bit_2024_05"        "emploi_bit_2024_06"        "emploi_bit_2024_07"       
#[36] "emploi_bit_2024_08"        "emploi_bit_2024_09"        "comp_disp_public"          "DispPolitiquePublique"    
chomeur_fermeture =  mmo_19 %>%
  mutate(siren = substr(siret_af,1,9)) %>%
  inner_join(pilote_ferme, by = "siren") %>%
  select(-c("siret_af","siret_ut")) 

event_study_graph = chomeur_fermeture  %>%
  slice_sample(n = 100000) %>%
  filter(!(motifrupture %in% c("066","998","100",""))) %>%
  mutate(type_rupture = case_when(
    motifrupture %in% c("031","032","081","085","094") ~ "Fin_Cont_limite",
    motifrupture %in% c("011","012","014","015","025","026","034","036","086","097","110",
                        "111","112","098","113","115","116") ~ "Fin_eco",
    motifrupture %in% c("087","088","095","096") ~ "Lic_faute",
    motifrupture %in% c("035","037","039","058","059","082") ~ "Départ_Sal",
    motifrupture %in% c("043","084","110","111","114") ~ "Accords",
    motifrupture %in% c("020","033","032","038","065","066","083","089","091","092","093","099","999") ~ "Autres",
    .default = "",
  )) %>%
  group_by(siren) %>%
  mutate(
    finctt = ymd(finctt)
  ) %>%
  filter(!is.na(finctt)) %>%
  mutate(  
    date_failled = max(finctt),
    tau = as.numeric(difftime(finctt,date_failled,units ="weeks")),
    #tau = round(tau,0)
  ) %>%
  filter(-100 < tau ) %>%
  group_by(tau,type_rupture) %>%
  summarise(n_licen = n())

ggplot(event_study_graph, aes(x = tau, y = n_licen, fill = type_rupture)) +
  geom_area(position =  "stack",alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "",
    x = "semaines",
    y = "Nombre de licenciements"
  )+
  theme_minimal()
  
ggplot(event_study_graph, aes(x = tau, y = n_licen)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "",
    x = "semaines",
    y = "Nombre de licenciements"
  )




classi_motif = chomeur_fermeture  %>%
  filter(!(motifrupture %in% c("066","998","100",""))) %>%
  mutate(type_rupture = case_when(
    motifrupture %in% c("031","032","081","085","094") ~ "Fin_Cont_limite",
    motifrupture %in% c("011","012","014","015","025","026","034","036","086","097","110",
                        "111","112","098","113","115","116") ~ "Fin_eco",
    motifrupture %in% c("087","088","095","096") ~ "Lic_faute",
    motifrupture %in% c("035","037","039","058","059","082") ~ "Départ_Sal",
    motifrupture %in% c("043","084","110","111","114") ~ "Accords",
    motifrupture %in% c("020","033","032","038","065","066","083","089","091","092","093","099","999") ~ "Autres",
    .default = "",
  )) %>%
  filter(type_rupture == "")
table(classi_motif$motifrupture)




#graph alt
ggplot(event_study_graph %>% filter(tau < -1) %>% mutate(tau = round(tau,0)), aes(x = tau, y = n_licen, fill = type_rupture)) +
  geom_area(position =  "stack",alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "",
    x = "semaines",
    y = "Nombre de licenciements"
  )+
  theme_minimal()


#graph alt
ggplot(event_study_graph %>% filter(tau < -1,type_rupture != "Fin_Cont_limite") %>% mutate(tau = round(tau,0)), aes(x = tau, y = n_licen, fill = type_rupture)) +
  geom_area(position =  "stack",alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "",
    x = "semaines",
    y = "Nombre de licenciements"
  )+
  theme_minimal()

ggplot(event_study_graph %>% filter(tau < -1,type_rupture == "Fin_Cont_limite") %>% mutate(tau = round(tau,0)), aes(x = tau, y = n_licen, fill = type_rupture)) +
  geom_area(position =  "stack",alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "",
    x = "semaines",
    y = "Nombre de licenciements"
  )+
  theme_minimal()


#graph alt
ggplot(event_study_graph %>% filter(tau > -5,type_rupture != "Fin_Cont_limite") %>% mutate(tau = round(tau,0)), aes(x = tau, y = n_licen, fill = type_rupture)) +
  geom_area(position =  "stack",alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "",
    x = "semaines",
    y = "Nombre de licenciements"
  )+
  theme_minimal()

df = event_study_graph %>% filter(tau == 0,type_rupture != "Fin_Cont_limite")