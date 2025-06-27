# Analisi Esplorativa dei Dati Demografici
# =====================================
library(readxl)
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(scales)



# 1. CARICAMENTO DEI DATI
data <- read_excel("C:\\Users\\chiar\\Desktop\\tirocinio\\Dati_regionali_2021\\R02_indicatori_2021_sezioni.xlsx")

data_clean <- data[, -c(1, 3, 5,7,9,10,11)]


# 2. CREAZIONE NUOVO DATASET FOCALIZZATO SUL GENERE
# -------------------------------------------------
dataset_genere <- data_clean %>%
  mutate(
    # INFORMAZIONI GENERALI 
    Sezione_ID = SEZ21_ID,
    Popolazione_Totale = P1,
    
    # DISTRIBUZIONE GENERE - TOTALE 
    Maschi_Totale = P2,
    Femmine_Totale = P3,
    Perc_Maschi = round((P2 / P1) * 100, 2),
    Perc_Femmine = round((P3 / P1) * 100, 2),
    Differenza_Genere = P2 - P3,
    Rapporto_Maschi_Femmine = ifelse(P3 > 0, round(P2 / P3, 3), NA),
    
    # DISTRIBUZIONE GENERE PER FASCE D'ETÀ
    # 0-4 anni
    Maschi_0_4 = P30,
    Femmine_0_4 = P67,
    Perc_Maschi_0_4 = ifelse((P30 + P67) > 0, round((P30 / (P30 + P67)) * 100, 2), NA), #P30 + P67 = P14
    Perc_Femm_0_4 = 100- Perc_Maschi_0_4,
    
    # 5-9 anni
    Maschi_5_9 = P31,
    Femmine_5_9 = P68,
    Perc_Maschi_5_9 = ifelse((P31 + P68) > 0, round((P31 / (P31 + P68)) * 100, 2), NA),
    Perc_Femm_5_9 = 100- Perc_Maschi_5_9,
    
    # 10-14 anni
    Maschi_10_14 = P32,
    Femmine_10_14 = P69,
    Perc_Maschi_10_14 = ifelse((P32 + P69) > 0, round((P32 / (P32 + P69)) * 100, 2), NA),
    Perc_Femm_10_14 = 100- Perc_Maschi_10_14,
    
    # 15-19 anni
    Maschi_15_19 = P33,
    Femmine_15_19 = P70,
    Perc_Maschi_15_19 = ifelse((P33 + P70) > 0, round((P33 / (P33 + P70)) * 100, 2), NA),
    Perc_Femm_15_19 = 100- Perc_Maschi_15_19,
    
    # 20-24 anni
    Maschi_20_24 = P34,
    Femmine_20_24 = P71,
    Perc_Maschi_20_24 = ifelse((P34 + P71) > 0, round((P34 / (P34 + P71)) * 100, 2), NA),
    Perc_Femm_20_24 = 100- Perc_Maschi_20_24,
    
    # 25-29 anni
    Maschi_25_29 = P35,
    Femmine_25_29 = P72,
    Perc_Maschi_25_29 = ifelse((P35 + P72) > 0, round((P35 / (P35 + P72)) * 100, 2), NA),
    Perc_Femm_25_29 = 100- Perc_Maschi_25_29,
    
    # 30-34 anni
    Maschi_30_34 = P36,
    Femmine_30_34 = P73,
    Perc_Maschi_30_34 = ifelse((P36 + P73) > 0, round((P36 / (P36 + P73)) * 100, 2), NA),
    Perc_Femm_30_34 = 100- Perc_Maschi_30_34,
    
    # 35-39 anni
    Maschi_35_39 = P37,
    Femmine_35_39 = P74,
    Perc_Maschi_35_39 = ifelse((P37 + P74) > 0, round((P37 / (P37 + P74)) * 100, 2), NA),
    Perc_Femm_35_39 = 100- Perc_Maschi_35_39,
    
    # 40-44 anni
    Maschi_40_44 = P38,
    Femmine_40_44 = P75,
    Perc_Maschi_40_44 = ifelse((P38 + P75) > 0, round((P38 / (P38 + P75)) * 100, 2), NA),
    Perc_Femm_40_44 = 100- Perc_Maschi_40_44,
    
    # 45-49 anni
    Maschi_45_49 = P39,
    Femmine_45_49 = P76,
    Perc_Maschi_45_49 = ifelse((P39 + P76) > 0, round((P39 / (P39 + P76)) * 100, 2), NA),
    Perc_Femm_45_49 = 100- Perc_Maschi_45_49,
    
    # 50-54 anni
    Maschi_50_54 = P40,
    Femmine_50_54 = P77,
    Perc_Maschi_50_54 = ifelse((P40 + P77) > 0, round((P40 / (P40 + P77)) * 100, 2), NA),
    Perc_Femm_50_54 = 100- Perc_Maschi_50_54,
    
    # 55-59 anni
    Maschi_55_59 = P41,
    Femmine_55_59 = P78,
    Perc_Maschi_55_59 = ifelse((P41 + P78) > 0, round((P41 / (P41 + P78)) * 100, 2), NA),
    Perc_Femm_55_59 = 100- Perc_Maschi_55_59,
    
    # 60-64 anni
    Maschi_60_64 = P42,
    Femmine_60_64 = P79,
    Perc_Maschi_60_64 = ifelse((P42 + P79) > 0, round((P42 / (P42 + P79)) * 100, 2), NA),
    Perc_Femm_60_64 = 100- Perc_Maschi_60_64,  
    
    # 65-69 anni
    Maschi_65_69 = P43,
    Femmine_65_69 = P80,
    Perc_Maschi_65_69 = ifelse((P43 + P80) > 0, round((P43 / (P43 + P80)) * 100, 2), NA),
    Perc_Femm_65_69 = 100- Perc_Maschi_65_69,
    
    # 70-74 anni
    Maschi_70_74 = P44,
    Femmine_70_74 = P81,
    Perc_Maschi_70_74 = ifelse((P44 + P81) > 0, round((P44 / (P44 + P81)) * 100, 2), NA),
    Perc_Femm_70_74 = 100- Perc_Maschi_70_74,
    
    # 75+ anni
    Maschi_75_plus = P45,
    Femmine_75_plus = P82,
    Perc_Maschi_75_plus = ifelse((P45 + P82) > 0, round((P45 / (P45 + P82)) * 100, 2), NA),
    Perc_Femm_75_plus = 100- Perc_Maschi_75_plus,
    
    # GRANDI GRUPPI D'ETÀ
    Maschi_0_14 = P30 + P31 + P32,
    Femmine_0_14 = P67 + P68 + P69,
    Perc_Maschi_0_14 = ifelse((Maschi_0_14 + Femmine_0_14) > 0, 
                              round((Maschi_0_14 / (Maschi_0_14 + Femmine_0_14)) * 100, 2), NA),
    Perc_Femmine_0_14 = ifelse((Maschi_0_14 + Femmine_0_14) > 0, 
                               round((Femmine_0_14 / (Maschi_0_14 + Femmine_0_14)) * 100, 2), NA),
    
    Maschi_15_64 = P33 + P34 + P35 + P36 + P37 + P38 + P39 + P40 + P41 + P42,
    Femmine_15_64 = P70 + P71 + P72 + P73 + P74 + P75 + P76 + P77 + P78 + P79,
    Perc_Maschi_15_64 = ifelse((Maschi_15_64 + Femmine_15_64) > 0, 
                               round((Maschi_15_64 / (Maschi_15_64 + Femmine_15_64)) * 100, 2), NA),
    Perc_Femmine_15_64 = ifelse((Maschi_15_64 + Femmine_15_64) > 0, 
                                round((Femmine_15_64 / (Maschi_15_64 + Femmine_15_64)) * 100, 2), NA),
    
    Maschi_65_plus = P43 + P44 + P45,
    Femmine_65_plus = P80 + P81 + P82,
    Perc_Maschi_65_plus = ifelse((Maschi_65_plus + Femmine_65_plus) > 0, 
                                 round((Maschi_65_plus / (Maschi_65_plus + Femmine_65_plus)) * 100, 2), NA),
    Perc_Femmine_65_plus = ifelse((Maschi_65_plus + Femmine_65_plus) > 0, 
                                  round((Femmine_65_plus / (Maschi_65_plus + Femmine_65_plus)) * 100, 2), NA),
    
    
    # DISTRIBUZIONE GENERE PER ISTRUZIONE (9+ anni)
    Maschi_9plus = P84,
    Femmine_9plus = P85,
    Perc_Maschi_9plus = ifelse((P84 + P85) > 0, round((P84 / (P84 + P85)) * 100, 2), NA),
    
    # Senza titolo
    Maschi_Senza_Titolo = P91,
    Femmine_Senza_Titolo = P96,
    Perc_Maschi_Senza_Titolo = ifelse((P91 + P96) > 0, 
                                      round((P91 / (P91 + P96)) * 100, 2), NA),
    
    # Licenza elementare
    Maschi_Elementare = P92,
    Femmine_Elementare = P97,
    Perc_Maschi_Elementare = ifelse((P92 + P97) > 0, 
                                    round((P92 / (P92 + P97)) * 100, 2), NA),
    
    # Licenza media
    Maschi_Media = P93,
    Femmine_Media = P98,
    Perc_Maschi_Media = ifelse((P93 + P98) > 0, 
                               round((P93 / (P93 + P98)) * 100, 2), NA),
    
    # Diploma superiore
    Maschi_Superiore = P94,
    Femmine_Superiore = P99,
    Perc_Maschi_Superiore = ifelse((P94 + P99) > 0, 
                                   round((P94 / (P94 + P99)) * 100, 2), NA),
    
    # Titolo terziario
    Maschi_Terziario = P95,
    Femmine_Terziario = P100,
    Perc_Maschi_Terziario = ifelse((P95 + P100) > 0, 
                                   round((P95 / (P95 + P100)) * 100, 2), NA),
    
    # DISTRIBUZIONE GENERE PER OCCUPAZIONE (15-64 anni)
    Maschi_Occupati = P102,
    Femmine_Occupate = P103,
    Perc_Maschi_Occupati = ifelse((P102 + P103) > 0, 
                                  round((P102 / (P102 + P103)) * 100, 2), NA),
    
    # Tasso di occupazione per genere
    Tasso_Occupazione_Maschi = ifelse(Maschi_15_64 > 0, 
                                      round((P102 / Maschi_15_64) * 100, 2), NA),
    Tasso_Occupazione_Femmine = ifelse(Femmine_15_64 > 0, 
                                       round((P103 / Femmine_15_64) * 100, 2), NA),
    
    # DISTRIBUZIONE GENERE PER NAZIONALITÀ
    #italiani
    Maschi_Italiani = IT4 + IT5 + IT6,
    Femmine_Italiane = IT7 + IT8 + IT9,
    Perc_Maschi_Italiani = ifelse((Maschi_Italiani + Femmine_Italiane) > 0, 
                                  round((Maschi_Italiani / (Maschi_Italiani + Femmine_Italiane)) * 100, 2), NA),
    # 0-14 italiani
    Maschi_ita_0_14 = IT4,
    Femmine_ita_0_14 = IT7,
    Perc_Maschi_ita_0_14 = ifelse((IT4+IT7) > 0, round((IT4 / (IT4+IT7)) * 100, 2), NA),
    Perc_Femm_ita_0_14 = 100- Perc_Maschi_ita_0_14,
    
    # 15-64 italiani
    Maschi_ita_15_64 = IT5,
    Femmine_ita_15_64 = IT8,
    Perc_Maschi_ita_15_64 = ifelse((IT5+IT8) > 0, round((IT5 / (IT5+IT8)) * 100, 2), NA),
    Perc_Femm_ita_15_64 = 100- Perc_Maschi_ita_15_64,
    
    # 65 + italiani
    Maschi_ita_65_plus = IT6,
    Femmine_ita_65_plus = IT9,
    Perc_Maschi_ita_65_plus = ifelse((IT6+IT9) > 0, round((IT6 / (IT6+IT9)) * 100, 2), NA),
    Perc_Femm_ita_65_plus = 100- Perc_Maschi_ita_65_plus,
    
    # occupati italiani 15-64
    Maschi_occ_ita = IT11,
    Femmine_occ_ita = IT12,
    Perc_Maschi_occ_ita = ifelse((IT11+IT12) > 0, round((IT11 / (IT11+IT12)) * 100, 2), NA),
    Perc_Femm_ita_occ_ita = 100- Perc_Maschi_occ_ita,
    
    #stranieri e apolidi
    Maschi_Stranieri = ST2,
    Femmine_Straniere = ST2_B,
    Perc_Maschi_Stranieri = ifelse((ST2 + ST2_B) > 0, 
                                   round((ST2 / (ST2 + ST2_B)) * 100, 2), NA),
    
    # 0-14 stranieri e apolidi
    Maschi_str_0_14 = ST25,
    Femmine_str_0_14 = ST28,
    Perc_Maschi_str_0_14 = ifelse((ST25+ST28) > 0, round((ST25 / (ST25+ST28)) * 100, 2), NA),
    Perc_Femm_str_0_14 = 100- Perc_Maschi_str_0_14,
    
    # 15_64 stranieri e apolidi
    Maschi_str_15_64 = ST26,
    Femmine_str_15_64 = ST29,
    Perc_Maschi_str_15_64 = ifelse((ST26+ST29) > 0, round((ST26 / (ST26+ST29)) * 100, 2), NA),
    Perc_Femm_str_15_64 = 100- Perc_Maschi_str_15_64,
    
    # 65+ stranieri e apolidi
    Maschi_str_65_plus = ST27,
    Femmine_str_65_plus = ST30,
    Perc_Maschi_str_65_plus = ifelse((ST27+ST30) > 0, round((ST27 / (ST27+ST30)) * 100, 2), NA),
    Perc_Femm_str_65_plus = 100- Perc_Maschi_str_65_plus,
    
    # occupati stranieri e apolidi 15-64
    Maschi_occ_str = ST32,
    Femmine_occ_str = ST33,
    Perc_Maschi_occ_str = ifelse((ST32+ST33) > 0, round((ST32 / (ST32+ST33)) * 100, 2), NA),
    Perc_Femm_ita_occ_str = 100- Perc_Maschi_occ_str,
    
    # stranieri UE
    Maschi_Stranieri_UE = ST17,
    Femmine_Straniere_UE = ST18,
    Perc_Maschi_Stranieri_UE = ifelse((ST17 + ST18) > 0, 
                                   round((ST17 / (ST17 + ST18)) * 100, 2), NA),
    
    # stranieri EXTRA EU
    Maschi_Stranieri_EXTRA_UE = ST20,
    Femmine_Straniere_EXTRA_UE = ST21,
    Perc_Maschi_Stranieri_EXTRA_UE = ifelse((ST20 + ST21) > 0, 
                                      round((ST20 / (ST20 + ST21)) * 100, 2), NA),
    
    # FAMIGLIE 
    # Percentuale famiglie numerose (5 o più) (considero 2 componenti come genitori --> 3 o + figli)
    Perc_Famiglie_Numerose = round(((PF7 + PF8) / PF1) * 100, 2),
    
    # INDICATORI DI SQUILIBRIO DI GENERE 
    Squilibrio_Generale = abs(50 - Perc_Maschi),
    Squilibrio_Giovani = abs(50 - Perc_Maschi_0_14),
    Squilibrio_Adulti = abs(50 - Perc_Maschi_15_64),
    Squilibrio_Anziani = abs(50 - Perc_Maschi_65_plus),
    
    # CLASSIFICAZIONI
    Maggioranza_Genere = case_when(
      Perc_Maschi > 52 ~ "Prevalenza Maschi",
      Perc_Femmine > 52 ~ "Prevalenza Femmine", 
      TRUE ~ "Equilibrato"
    )
    
  ) %>%
  # Mantieni solo le colonne del nuovo dataset (rimuovi le originali P1, P2, etc.)
  select(Sezione_ID:Maggioranza_Genere)

# 3. STATISTICHE DESCRITTIVE
# --------------------------
table(dataset_genere$Maggioranza_Genere) %>% print()

# 4. ANALISI PER FASCE D'ETÀ
# --------------------------
eta_genere_stats <- dataset_genere %>%
  summarise(
    Perc_Maschi_Giovani_Media = round(mean(Perc_Maschi_0_14, na.rm = TRUE), 2),
    Perc_Maschi_Adulti_Media = round(mean(Perc_Maschi_15_64, na.rm = TRUE), 2),
    Perc_Maschi_Anziani_Media = round(mean(Perc_Maschi_65_plus, na.rm = TRUE), 2)
  )
print(eta_genere_stats)

# 5. VISUALIZZAZIONI
# ------------------
# Confronto per fasce d'età
eta_long <- dataset_genere %>%
  select(Sezione_ID, Perc_Maschi_0_14, Perc_Maschi_15_64, Perc_Maschi_65_plus) %>%
  pivot_longer(cols = -Sezione_ID, names_to = "Fascia_Eta", values_to = "Perc_Maschi") %>%
  mutate(Fascia_Eta = case_when(
    Fascia_Eta == "Perc_Maschi_0_14" ~ "0-14 anni",
    Fascia_Eta == "Perc_Maschi_15_64" ~ "15-64 anni",
    Fascia_Eta == "Perc_Maschi_65_plus" ~ "65+ anni"
  ))

p2 <- ggplot(eta_long, aes(x = Perc_Maschi, fill = Fascia_Eta)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 50, color = "red", linetype = "dashed") +
  facet_wrap(~Fascia_Eta, ncol = 1) +
  theme_minimal() +
  labs(title = "Distribuzione Percentuale Maschi per Fascia d'Età",
       x = "Percentuale Maschi (%)", y = "Numero di Sezioni")
print(p2)

# 6. IDENTIFICAZIONE SEZIONI PARTICOLARI
# --------------------------------------
# Sezioni con più maschi
sezioni_piu_maschi <- dataset_genere %>%
  filter(Perc_Maschi >= quantile(Perc_Maschi, 0.95, na.rm = TRUE)) %>%
  select(Sezione_ID, Popolazione_Totale, Perc_Maschi, Perc_Femmine, Maggioranza_Genere) %>%
  arrange(desc(Perc_Maschi))
cat("Top 5% sezioni con più maschi:\n")
print(head(sezioni_piu_maschi))

# Sezioni con più femmine
sezioni_piu_femmine <- dataset_genere %>%
  filter(Perc_Femmine >= quantile(Perc_Femmine, 0.95, na.rm = TRUE)) %>%
  select(Sezione_ID, Popolazione_Totale, Perc_Maschi, Perc_Femmine, Maggioranza_Genere) %>%
  arrange(desc(Perc_Femmine))
cat("\nTop 5% sezioni con più femmine:\n")
print(head(sezioni_piu_femmine))


# occupati rapportati al totale? 

# 7. IDENTIFICAZIONE OUTLIER PER VARIABILI CHIAVE
# -----------------------------------------------
# Funzione per identificare outlier con metodo IQR (criterio Tuckey)
identifica_outlier_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  limite_inf <- Q1 - 1.5 * IQR
  limite_sup <- Q3 + 1.5 * IQR
  return(x < limite_inf | x > limite_sup)
}

# Identificazione outlier nel dataset principale
dataset_analisi <- dataset_genere %>%
  mutate(
    # Flag per outlier popolazione
    Outlier_Pop_IQR = identifica_outlier_iqr(Popolazione_Totale),
    
    # Flag per outlier percentuale maschi
    Outlier_Maschi_IQR = identifica_outlier_iqr(Perc_Maschi),
    
    # Flag per outlier percentuale femmine
    Outlier_Femmine_IQR = identifica_outlier_iqr(Perc_Femmine),
    
    # Conteggio totale flag outlier per sezione
    N_Flag_Outlier = as.numeric(Outlier_Pop_IQR) + 
      as.numeric(Outlier_Maschi_IQR) + 
      as.numeric(Outlier_Femmine_IQR)
  )

# Sezioni con popolazione anomala (outlier)
outlier_popolazione <- dataset_analisi %>%
  filter(Outlier_Pop_IQR) %>%
  select(Sezione_ID, Popolazione_Totale, Perc_Maschi, Perc_Femmine, Maggioranza_Genere) %>%
  arrange(desc(Popolazione_Totale))
print(outlier_popolazione)

# Sezioni con squilibri di genere estremi
outlier_genere <- dataset_analisi %>%
  filter(Outlier_Maschi_IQR) %>%
  select(Sezione_ID, Popolazione_Totale, Perc_Maschi, Perc_Femmine, Maggioranza_Genere) %>%
  arrange(desc(abs(Perc_Maschi - 50)))
print(outlier_genere)

# Sezioni con multiple anomalie
sezioni_multiple_anomalie <- dataset_analisi %>%
  filter(N_Flag_Outlier >= 2) %>%
  select(Sezione_ID, Popolazione_Totale, Perc_Maschi, Perc_Femmine, N_Flag_Outlier) %>%
  arrange(desc(N_Flag_Outlier))
print(sezioni_multiple_anomalie)


# 8. INFO SU SEZIONI OUTLIER
outlier_popolazione <- outlier_popolazione %>%
  left_join(dataset_genere %>% 
              select(Sezione_ID,
                     Perc_Maschi_0_14,
                     Perc_Femmine_0_14,
                     Perc_Maschi_15_64,
                     Perc_Femmine_15_64,
                     Perc_Maschi_65_plus,
                     Perc_Femmine_65_plus,
                     Tasso_Occupazione_Maschi,
                     Tasso_Occupazione_Femmine,
                     Perc_Maschi_Senza_Titolo,
                     Perc_Maschi_Elementare,
                     Perc_Maschi_Media,
                     Perc_Maschi_Superiore,
                     Perc_Maschi_Terziario,
                     Perc_Maschi_Italiani,
                     Perc_Maschi_Stranieri,
                     Perc_Famiglie_Numerose),
            by = "Sezione_ID")

outlier_genere <- outlier_genere %>%
  left_join(dataset_genere %>% 
              select(Sezione_ID,
                     Perc_Maschi_0_14,
                     Perc_Femmine_0_14,
                     Perc_Maschi_15_64,
                     Perc_Femmine_15_64,
                     Perc_Maschi_65_plus,
                     Perc_Femmine_65_plus,
                     Tasso_Occupazione_Maschi,
                     Tasso_Occupazione_Femmine,
                     Perc_Maschi_Senza_Titolo,
                     Perc_Maschi_Elementare,
                     Perc_Maschi_Media,
                     Perc_Maschi_Superiore,
                     Perc_Maschi_Terziario,
                     Perc_Maschi_Italiani,
                     Perc_Maschi_Stranieri,
                     Perc_Famiglie_Numerose),
            by = "Sezione_ID")


# istruzione e occupazione rappportai al totale o maschi+femmine in quella categoria (%maschi senza titolo su maschi+ femm senza titolo o su tutta pop. maschile/generale)

