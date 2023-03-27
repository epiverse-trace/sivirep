library(readr)
library(dplyr)
df_other_virus_202252 <- read_csv("data/df_other_virus_202252.csv")
df_respiratory_filmarray_202252 <- read_csv("data/df_respiratory_filmarray_202252.csv")


list_diseases <- unique(sivigila_summary_data$nombre)
list_specific <- list_diseases[stringr::str_detect(list_diseases, name_disease) == TRUE]


l<-stringr::str_detect(df_respiratory_filmarray_202252$adenovirus, "^DETECTADO$") == TRUE]

disease_data_grouped  <- df_respiratory_filmarray_202252 %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("grupo_edad")))) %>% filter(any(adenovirus == "DETECTADO")) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop") 

disease_data_grouped  <- df_respiratory_filmarray_202252 %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("adenovirus", "grupo_edad")))) %>% filter(any(adenovirus == "DETECTADO")) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop") 
colnames(disease_data_grouped)[1] <- "enfermedad"
disease_data_grouped$enfermedad[disease_data_grouped$enfermedad == 'DETECTADO'] <- "adenovirus"
disease_data_grouped2  <- df_respiratory_filmarray_202252 %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("rinovirus_enterovirus_humano", "grupo_edad")))) %>% filter(any(rinovirus_enterovirus_humano == "DETECTADO")) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
colnames(disease_data_grouped2)[1] <- "enfermedad"
disease_data_grouped2$enfermedad[disease_data_grouped2$enfermedad == 'DETECTADO'] <- "rinovirus_enterovirus_humano"
disease_data_grouped3  <- df_respiratory_filmarray_202252 %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("influenza_b", "grupo_edad")))) %>% filter(any(influenza_b == "DETECTADO")) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
disease_data_grouped4  <- df_respiratory_filmarray_202252 %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("virus_parainfluenza_1", "grupo_edad")))) %>% filter(any(virus_parainfluenza_1 == "DETECTADO")) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
disease_data_grouped5  <- df_respiratory_filmarray_202252 %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("virus_parainfluenza_2", "grupo_edad")))) %>% filter(any(virus_parainfluenza_2 == "DETECTADO")) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
disease_data_grouped6  <- df_respiratory_filmarray_202252 %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("virus_parainfluenza_3", "grupo_edad")))) %>% filter(any(virus_parainfluenza_3 == "DETECTADO")) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
disease_data_grouped7  <- df_respiratory_filmarray_202252 %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("virus_parainfluenza_4", "grupo_edad")))) %>% filter(any(virus_parainfluenza_4 == "DETECTADO")) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")

d <- merge(x = disease_data_grouped, y = disease_data_grouped2, by = "grupo_edad")

df_respiratory_filmarray_202252$coronavirus_229e
df_respiratory_filmarray_202252$coronavirus_hku1
df_respiratory_filmarray_202252$coronavirus_nl63
df_respiratory_filmarray_202252$coronavirus_oc43
df_respiratory_filmarray_202252$severe_acute_respiratoriy_syndrome_coronavi
df_respiratory_filmarray_202252$metapneumovirus_humano
df_respiratory_filmarray_202252$rinovirus_enterovirus_humano
df_respiratory_filmarray_202252$influenza_b
df_respiratory_filmarray_202252$virus_parainfluenza_


df1 <- data.frame(enfermedad  = c("H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                  grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                  casos = c(51, 77, 45, 60, 30, 49, 0, 1, 0, 0, 0, 0, 25, 11, 5, 5, 4, 3, 84, 117, 13, 10, 7, 7, 51, 43, 16, 3, 4, 11, 261, 233, 95, 48, 23, 40, 121, 97, 44, 16, 16, 23, 314, 239, 23, 8, 8, 14, 224, 247, 64, 18, 11, 18)
)

ggplot2::ggplot(df1) +
  ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
  ggplot2::theme_classic() +
  ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
  ggplot2::scale_fill_discrete(name = "Enfermedad") +
  ggplot2::theme(legend.position = "bottom")

df2 <- data.frame(enfermedad  = c("H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                  grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                  casos = c(51, 77, 45, 60, 30, 49, 0, 1, 0, 0, 0, 0, 25, 11, 5, 5, 4, 3, 84, 117, 13, 10, 7, 7, 51, 43, 16, 3, 4, 11, 261, 233, 95, 48, 23, 40, 121, 97, 44, 16, 16, 23, 314, 239, 23, 8, 8, 14, 224, 247, 64, 18, 11, 18)
)

ggplot2::ggplot(df2) +
  ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
  ggplot2::theme_classic() +
  ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
  ggplot2::scale_fill_discrete(name = "Enfermedad") +
  ggplot2::theme(legend.position = "bottom")

df3 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                  grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                  casos = c(45, 25, 3, 7, 5, 5, 3, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 2, 0, 2, 6, 14, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 3, 14, 0, 2, 0, 0, 2, 3, 0, 0, 0, 0, 7, 14, 0, 0, 0, 0, 5, 16, 1, 0, 0, 1)
)

ggplot2::ggplot(df3) +
  ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
  ggplot2::theme_classic() +
  ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
  ggplot2::scale_fill_discrete(name = "Enfermedad") +
  ggplot2::theme(legend.position = "bottom")

df4 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                  grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                  casos = c(106, 30, 13, 30, 19, 51, 11, 28, 13, 18, 8, 4, 0, 0, 0, 0, 0, 0, 14, 5, 2, 5, 0, 1, 21, 33, 5, 8, 3, 5, 3, 6, 1, 0, 0, 1, 32, 32, 7, 7, 3, 3, 13, 7, 2, 0, 2, 1, 34, 35, 1, 4, 1, 1, 19, 21, 2, 3, 0, 1)
)

ggplot2::ggplot(df4) +
  ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
  ggplot2::theme_classic() +
  ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
  ggplot2::scale_fill_discrete(name = "Enfermedad") +
  ggplot2::theme(legend.position = "bottom")

df5 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                  grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                  casos = c(106, 30, 13, 30, 19, 51, 11, 28, 13, 18, 8, 4, 0, 0, 0, 0, 0, 0, 14, 5, 2, 5, 0, 1, 21, 33, 5, 8, 3, 5, 3, 6, 1, 0, 0, 1, 32, 32, 7, 7, 3, 3, 13, 7, 2, 0, 2, 1, 34, 35, 1, 4, 1, 1, 19, 21, 2, 3, 0, 1)
)

ggplot2::ggplot(df5) +
  ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
  ggplot2::theme_classic() +
  ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
  ggplot2::scale_fill_discrete(name = "Enfermedad") +
  ggplot2::theme(legend.position = "bottom")



generate_distribution_by_age <- function(diseases_data) {
  df1 <- data.frame(enfermedad  = c("H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(51, 77, 45, 60, 30, 49, 0, 1, 0, 0, 0, 0, 25, 11, 5, 5, 4, 3, 84, 117, 13, 10, 7, 7, 51, 43, 16, 3, 4, 11, 261, 233, 95, 48, 23, 40, 121, 97, 44, 16, 16, 23, 314, 239, 23, 8, 8, 14, 224, 247, 64, 18, 11, 18)
  )
  
  plot <- ggplot2::ggplot(df1) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}


generate_distribution_by_age_and_sars <- function(diseases_data) {
  df2 <- data.frame(enfermedad  = c("H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(51, 77, 45, 60, 30, 49, 0, 1, 0, 0, 0, 0, 25, 11, 5, 5, 4, 3, 84, 117, 13, 10, 7, 7, 51, 43, 16, 3, 4, 11, 261, 233, 95, 48, 23, 40, 121, 97, 44, 16, 16, 23, 314, 239, 23, 8, 8, 14, 224, 247, 64, 18, 11, 18)
  )
  
  plot <- ggplot2::ggplot(df2) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}

generate_distribution_by_age_and_esi <- function(diseases_data) {
  df3 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(45, 25, 3, 7, 5, 5, 3, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 2, 0, 2, 6, 14, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 3, 14, 0, 2, 0, 0, 2, 3, 0, 0, 0, 0, 7, 14, 0, 0, 0, 0, 5, 16, 1, 0, 0, 1)
  )
  
  plot <- ggplot2::ggplot(df3) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}


generate_distribution_by_age_and_ira <- function(diseases_data) {
  df4 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(106, 30, 13, 30, 19, 51, 11, 28, 13, 18, 8, 4, 0, 0, 0, 0, 0, 0, 14, 5, 2, 5, 0, 1, 21, 33, 5, 8, 3, 5, 3, 6, 1, 0, 0, 1, 32, 32, 7, 7, 3, 3, 13, 7, 2, 0, 2, 1, 34, 35, 1, 4, 1, 1, 19, 21, 2, 3, 0, 1)
  )
  
  plot <- ggplot2::ggplot(df4) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}

generate_distribution_by_age_and_irag_inusitado <- function(diseases_data) {
  df5 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(106, 30, 13, 30, 19, 51, 11, 28, 13, 18, 8, 4, 0, 0, 0, 0, 0, 0, 14, 5, 2, 5, 0, 1, 21, 33, 5, 8, 3, 5, 3, 6, 1, 0, 0, 1, 32, 32, 7, 7, 3, 3, 13, 7, 2, 0, 2, 1, 34, 35, 1, 4, 1, 1, 19, 21, 2, 3, 0, 1)
  )
  
  plot <- ggplot2::ggplot(df5) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}




import_data_viral_circulation <- function(report_data = c()) {
  
  viral_circulation_data <- data.frame()
  
  for (data_path in report_data) {
    file_extension <- tools::file_ext(data_path)
    if (!is.null(file_extension)) {
      temp_data <- switch(  
        file_extension,  
        "xlsx" = readxl::read_excel(data_path, col_names = F, skip = 3),  
        "csv" = utils::read.csv(data_path, header = F, skip = 3)
      )
      temp_data <- row_to_header(data = temp_data)
      viral_circulation_data <- rbind(viral_circulation_data, temp_data)
    }
  }
  return(viral_circulation_data)
}

row_to_header <- function(data, row_num = 1) {
  if (!is.null(data)) {  
    names(data) <- as.character(unlist(data[row_num,]))
    data[-row_num,]
  }
}

clean_viral_circulation_data <- function(viral_circulation_data) {
  names(viral_circulation_data) <- epitrix::clean_labels(names(viral_circulation_data))
  return(viral_circulation_data)
}

generate_age_categories <- function(data) {
  data <- cbind(data, grupo_edad = NA)
  data[, ncol(data)] <- sapply(data$edad, define_age_categorie)
  return(data)
}

define_age_categorie <- function(age) {
  #config_path <- system.file("extdata", "config.yml", package = "sivirep")
  config_path <- "C:/Users/geral/Documents/TRACE/sivirep/inst/extdata/config.yml"
  categorie_conditionals <- config::get(file = config_path, "age_categorie_conditionals")
  categorie_labels <- config::get(file = config_path, "age_categorie_labels")
  
  age_values <- unlist(strsplit(age, " ", fixed = T))
  categorie <- categorie_labels[1]
  
  if ("AÑOS" %in% age_values) {
    age_num <- as.numeric(age_values[1])
    i <- 1;
    for (conditional in categorie_conditionals) {
      if (eval(parse(text = conditional)) == T) {
        categorie <- categorie_labels[i]
      } 
      i <- i + 1
    }
  }
  
  return(categorie)
}

group_by_columns_and_cases_total <- function(disease_data, event_name = "adenovirus", col_names, wt_percentage = FALSE, total_cases = 0, event_label) {
  config_path <- "C:/Users/geral/Documents/TRACE/sivirep/inst/extdata/config.yml"
  categorie_labels <- config::get(file = config_path, "age_categorie_labels")
  
  disease_data_grouped  <- disease_data %>% dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  if (wt_percentage) {
    disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(porcentaje = round(disease_data_grouped$casos/total_cases*100, 1))
    disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(evento = event_name, etiqueta = event_label)
  }
  
  for (label in categorie_labels) {
    if (!any(disease_data_grouped == label)) {
      new_row <- data.frame(grupo_edad = label, casos = 0, porcentaje  = 0, evento = event_name, etiqueta = event_label)
      disease_data_grouped <- rbind(disease_data_grouped, new_row)
    }
  }
  
  return(disease_data_grouped)
}

get_distribution_by_age_group <- function(report_data, positive_value = "DETECTADO") {
  config_path <- "C:/Users/geral/Documents/TRACE/sivirep/inst/extdata/config.yml"
  column_names <- config::get(file = config_path, "respiratory_virus_column_names")
  names <- config::get(file = config_path, "respiratory_virus_names")
  viruses_by_age_group <- data.frame()
  
  i <- 1
  for (column in column_names) { 
    print(column)
    positive_cases <- report_data[eval(parse(text = paste0("report_data$", column, " == ", '"', positive_value, '"'))), ]
    positive_cases_by_age_group <- group_by_columns_and_cases_total(positive_cases, "grupo_edad", event_name = column,  wt_percentage = TRUE, total_cases = nrow(report_data), event_label = names[i])
    viruses_by_age_group <- rbind(viruses_by_age_group, positive_cases_by_age_group)
    i <- i + 1
  }
  
  viruses_by_age_group <- viruses_by_age_group %>% dplyr::mutate(porcentaje_normalizado = ceiling((scales::rescale(viruses_by_age_group$porcentaje)) * 100)) 
  viruses_by_age_group$porcentaje_normalizado
  return(viruses_by_age_group)
}


d2 <- import_data_viral_circulation(report_data = c("data/1-7 AGOSTO_2022.xlsx", "data/8-14 AGOSTO_2022.xlsx"))
d2 <- clean_viral_circulation_data(d2)
d2 <- generate_age_categories(d2)


h1_2009 <- d2[d2$influenza_a_h1_2009 == "DETECTADO", ]
h1 <- d2[d2$influenza_a_h1 == "DETECTADO", ]
h3 <- d2[d2$influenza_a_h3 == "DETECTADO", ]
influenza_b <- d2[d2$influenza_b == "DETECTADO", ]

metapneumovirus <- d2[d2$metapneumovirus_humano == "DETECTADO", ]
rinovirus <- d2[d2$rinovirus_enterovirus_humano == "DETECTADO", ]

parainfluenza_1  <- d2[d2$virus_parainfluenza_1 == "DETECTADO", ]
parainfluenza_2  <- d2[d2$virus_parainfluenza_2 == "DETECTADO", ]
parainfluenza_3  <- d2[d2$virus_parainfluenza_3 == "DETECTADO", ]
parainfluenza_4  <- d2[d2$virus_parainfluenza_4 == "DETECTADO", ]
parainfluenza <- rbind(parainfluenza_1, parainfluenza_2, parainfluenza_3, parainfluenza_4)
vsr <- d2[d2$virus_sincitial_respiratorio == "DETECTADO", ]
adenovirus <- d2[d2$adenovirus == "DETECTADO", ]


adenovirus_by_age_group <- group_by_columns_and_cases_total(adenovirus, "grupo_edad", event_name = "adenovirus",  wt_percentage = TRUE, total_cases = 70, event_label = "Adenovirus")
vsr_by_age_group <- group_by_columns_and_cases_total(vsr, "grupo_edad", event_name = "virus_sincitial_respiratorio",  wt_percentage = TRUE, total_cases = 70, event_label = "VSR")
#parainfluenza_by_age_group <- group_by_columns_and_cases_total(parainfluenza, "grupo_edad", event_name = c("virus_parainfluenza_1", "virus_parainfluenza_2", "virus_parainfluenza_3", "virus_parainfluenza_4"),  wt_percentage = TRUE, total_cases = 70)

rinovirus_by_age_group <- group_by_columns_and_cases_total(rinovirus, "grupo_edad", event_name = "rinovirus_enterovirus_humano",  wt_percentage = TRUE, total_cases = 70, event_label = "Rinovirus")
metapneumovirus_by_age_group <- group_by_columns_and_cases_total(metapneumovirus, "grupo_edad", event_name = "metapneumovirus_humano",  wt_percentage = TRUE, total_cases = 70, event_label = "Metapneumovirus")
influenza_b_by_age_group <- group_by_columns_and_cases_total(influenza_b, "grupo_edad", event_name = "influenza_b",  wt_percentage = TRUE, total_cases = 70, event_label = "Influenza B")
h3_by_age_group <- group_by_columns_and_cases_total(h3, "grupo_edad", event_name = "influenza_a_h3",  wt_percentage = TRUE, total_cases = 70, event_label = "H3N1")
h1_by_age_group <- group_by_columns_and_cases_total(h1, "grupo_edad", event_name = "influenza_a_h1",  wt_percentage = TRUE, total_cases = 70, event_label = "H1N2")
h1_2009_by_age_group  <- group_by_columns_and_cases_total(h1_2009, "grupo_edad", event_name = "influenza_a_h1_2009",  wt_percentage = TRUE, total_cases = 70, event_label = "H1N1 2009")

virus_by_age_group <- rbind(adenovirus_by_age_group, vsr_by_age_group,  rinovirus_by_age_group, metapneumovirus_by_age_group, influenza_b_by_age_group, h3_by_age_group, h1_by_age_group, h1_2009_by_age_group)

viruses_by_age_group <- get_distribution_by_age_group(report_data = d2)

plot <- ggplot2::ggplot(virus_by_age_group) +
  ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = porcentaje, fill = etiqueta), alpha = 0.9) +
  ggplot2::theme_classic() +
  ggplot2::xlab("Grupo de edad") + ggplot2::ylab("porcentaje de casos") +
  ggplot2::scale_fill_discrete(name = "Virus respiratorios") +
  ggplot2::theme(legend.position = "bottom")

plot

plot2 <- ggplot2::ggplot(viruses_by_age_group) +
  ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = porcentaje_normalizado, fill = etiqueta), alpha = 0.9) +
  ggplot2::theme_classic() +
  ggplot2::xlab("Grupo de edad") + ggplot2::ylab("porcentaje de casos") +
  ggplot2::scale_fill_discrete(name = "Virus respiratorios") +
  ggplot2::theme(legend.position = "bottom")

plot2
