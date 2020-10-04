library(tabulizer)
library(dplyr)
library(pdftools)
library(openxlsx)
library(reshape)


setwd("/Users/isiscosta/Downloads/Personales/DataSCOUT/Sheridan/data/votos_vereadores/Candidatos a vereador Datapedia/")

pdffiles = system("ls *.pdf",intern=TRUE)
alldata = data.frame()
for(location in pdffiles){
   tipocandidatura <- stringr::str_length(unlist(strsplit(unlist(strsplit(location,"-"))[3],"[.]"))[1])
   tipo = if_else(tipocandidatura==3, "Senador/a",
                  if_else(tipocandidatura == 4, "Deputado/a Federal",
                          "Deputado/a Estadual")
   )
   tipo = "VEREADOR"
   out <- extract_tables(location)
   final <- data.frame()
   for(i in 1:length(out)){
      aux <- do.call(rbind, out[i])
      headers = aux[1,]
      final = rbind(as.data.frame(aux[2:nrow(aux), ], stringsAsFactors = FALSE),
                      final)
   }
   names(final) = headers
   final = as.data.frame(final[2:nrow(final), ], stringsAsFactors = FALSE)
   final$Percentual = as.numeric(sub("--","",sub(",",".",sub("%","",final$Percentual))))
   final$Eleitores = as.numeric(stringr::str_trim(sub("[.]","",sub("votantes","",final$Eleitores))))
   final$`Votos Absolutos` = as.numeric(stringr::str_trim(sub("[.]","",sub("votos","",final$`Votos Absolutos`))))
   final$Ranking = as.numeric(stringr::str_trim(stringr::str_trim(sub("--","",sub("o","",final$`Ranking`)))))
   nr_candidato = unlist(strsplit(unlist(strsplit(location,"-"))[3],"[.]"))[1]
   candidato = unlist(strsplit(extract_metadata(location)$title,"-"))[2]
   nome_candidato = stringr::str_trim(unlist(strsplit(candidato,"[(]"))[1])
   partido_candidato = stringr::str_trim(unlist(strsplit(unlist(strsplit(candidato,"[(]"))[2],"[)]"))[1])
   final$nr_candidato = nr_candidato
   final$partido_candidato = partido_candidato
   final$nome_candidato = nome_candidato
   final$arquivo = location
   final$cargo = tipo
   alldata = rbind(alldata, final)
}
data_vereador <- alldata

setwd("/Users/isiscosta/Downloads/Personales/DataSCOUT/Sheridan/data/votos_vereadores/Candidatos a deputado e senador Datapedia//")

pdffiles = system("ls *.pdf",intern=TRUE)
alldata = data.frame()
for(location in pdffiles){
   tipocandidatura <- stringr::str_length(unlist(strsplit(unlist(strsplit(location,"-"))[3],"[.]"))[1])
   tipo = if_else(tipocandidatura==3, "Senador/a",
                  if_else(tipocandidatura == 4, "Deputado/a Federal",
                          "Deputado/a Estadual")
   )
#   tipo = "VEREADOR"
   out <- extract_tables(location)
   final <- data.frame()
   for(i in 1:length(out)){
      aux <- do.call(rbind, out[i])
      headers = aux[1,]
      final = rbind(as.data.frame(aux[2:nrow(aux), ], stringsAsFactors = FALSE),
                    final)
   }
   names(final) = headers
   final = as.data.frame(final[2:nrow(final), ], stringsAsFactors = FALSE)
   final$Percentual = as.numeric(sub("--","",sub(",",".",sub("%","",final$Percentual))))
   final$Eleitores = as.numeric(stringr::str_trim(sub("[.]","",sub("votantes","",final$Eleitores))))
   final$`Votos Absolutos` = as.numeric(stringr::str_trim(sub("[.]","",sub("votos","",final$`Votos Absolutos`))))
   final$Ranking = as.numeric(stringr::str_trim(stringr::str_trim(sub("--","",sub("o","",final$`Ranking`)))))
   nr_candidato = unlist(strsplit(unlist(strsplit(location,"-"))[3],"[.]"))[1]
   candidato = unlist(strsplit(extract_metadata(location)$title,"-"))[2]
   nome_candidato = stringr::str_trim(unlist(strsplit(candidato,"[(]"))[1])
   partido_candidato = stringr::str_trim(unlist(strsplit(unlist(strsplit(candidato,"[(]"))[2],"[)]"))[1])
   final$nr_candidato = nr_candidato
   final$partido_candidato = partido_candidato
   final$nome_candidato = nome_candidato
   final$arquivo = location
   final$cargo = tipo
   alldata = rbind(alldata, final)
}
data_brasilia <- alldata

alldata <- rbind(data_brasilia, data_vereador)

alldata$Bairro <- toupper(alldata$Bairro)
unique(sort(alldata$Bairro))[which(!unique(sort(alldata$Bairro)) %in% bairro_zona$BAIRRO)]


alldata$Bairro[alldata$Bairro=="PINTOLANDIA"] <- "PINTOLÂNDIA"
alldata$Bairro[alldata$Bairro=="UNIAO"] <- "UNIÃO"
alldata$Bairro[alldata$Bairro=="CINTURAO VERDE"] <- "CINTURÃO VERDE"
alldata$Bairro[alldata$Bairro=="PRICUMA"] <- "PRICUMÃ"
alldata$Bairro[alldata$Bairro=="JOQUEI CLUBE"] <- "JÓQUEI CLUBE"
alldata$Bairro[alldata$Bairro=="JOCKEY CLUBE"] <- "JÓQUEI CLUBE"
alldata$Bairro[alldata$Bairro=="SAO VICENTE"] <- "SÃO VICENTE"
alldata$Bairro[alldata$Bairro=="SAO FRANCISCO"] <- "SÃO FRANCISCO"
alldata$Bairro[alldata$Bairro=="SAO PEDRO"] <- "SÃO PEDRO"
alldata$Bairro[alldata$Bairro=="NOVA CANAA"] <- "NOVA CANAÃ"
alldata$Bairro[alldata$Bairro=="DR. AIRTON ROCHA"] <- "DOUTOR AIRTON ROCHA"
alldata$Bairro[alldata$Bairro=="ESTADOS"] <- "DOS ESTADOS"
alldata$Bairro[alldata$Bairro=="PARAVIANA"] <- "TRINTA E UM DE MARÇO PARAVIANA"
alldata$Bairro[alldata$Bairro=="ARACELIS"] <- "PROFESSORA ARACELI SOUTO MAIOR"
alldata$Bairro[alldata$Bairro=="JD TROPICAL"] <- "JARDIM TROPICAL"
alldata$Bairro[alldata$Bairro=="TANCREDO NEVES"] <- "TANCREDO NEVE"
alldata$Bairro[alldata$Bairro=="SENADOR HELIO CAMPOS"] <- "SENADOR HÉLIO CAMPOS"
alldata$Bairro[alldata$Bairro=="CAUAME"] <- "CAUAMÉ"
alldata$Bairro[alldata$Bairro=="CALUNGÁ"] <- "CALUNGA"


alldata[alldata$Bairro=="BURITIS","Votos Absolutos"] <- 
   alldata[alldata$Bairro=="BURITIS","Votos Absolutos"] + 
   alldata[alldata$Bairro=="ASA BRANCA","Votos Absolutos"]

alldata$Bairro[alldata$Bairro=="BURITIS"] <- "ASA BRANCA BURITIS"
alldata <- alldata[-which(alldata$Bairro=="ASA BRANCA"),]

unique(sort(alldata$Bairro))[which(!unique(sort(alldata$Bairro)) %in% bairro_zona$BAIRRO)]

alldata <- alldata %>% 
   left_join(bairro_zona, by = c("Bairro"="BAIRRO")) %>% 
   filter(!is.na(ZONA)) %>%
   mutate(ZONA = sprintf("%02d",ZONA)) %>%
   mutate(Bairro = paste0(ZONA,Bairro)) %>% 
   arrange(Bairro)

matriz_aliados_ver <- alldata %>% filter(cargo == "VEREADOR") %>% 
   select(nome_candidato, partido_candidato, Bairro, `Votos Absolutos`) %>% 
   reshape(idvar=c("nome_candidato","partido_candidato"), timevar="Bairro", direction="wide")
names(matriz_aliados_ver)[-c(1,2)] <- stringr::str_remove_all(names(matriz_aliados_ver)[-c(1,2)],"Votos Absolutos.")

matriz_aliados_ver %>% write.xlsx("vereadores_por_bairro_zona.xlsx")



matriz_aliados_dep <- alldata %>% filter(cargo != "VEREADOR") %>% 
   select(nome_candidato, partido_candidato, Bairro, `Votos Absolutos`) %>% 
   reshape(idvar=c("nome_candidato","partido_candidato"), timevar="Bairro", direction="wide")
names(matriz_aliados_dep)[-c(1,2)] <- stringr::str_remove_all(names(matriz_aliados_dep)[-c(1,2)],"Votos Absolutos.")

matriz_aliados_dep %>% write.xlsx("deputados_por_bairro_zona.xlsx")


########3#


basealiada <- c("PSDB","DEM","PSD","PRB")
candidatos_aliados <- c("ANTONIO DENARIUM", 
                        "ZÉ HAROLDO", 
                        "CHICO RODRIGUES", 
                        "ABEL GALINHA", 
                        "HAROLDO CATHEDRAL", 
                        "HIRAN GONÇALVES", 
                        "MARCOS JORGE", 
                        "MECIAS DE JESUS", 
                        "JHONATAN DE JESUS")

alldata %>% filter(nome_candidato %in% candidatos_aliados) %>% count(nome_candidato)
dataaliados <- alldata %>% filter(nome_candidato %in% candidatos_aliados)
matriz_aliados <- dataaliados %>% select(nome_candidato, partido_candidato, Bairro, `Votos Absolutos`) %>% 
   reshape(idvar=c("nome_candidato","partido_candidato"), timevar="Bairro", direction="wide")
names(matriz_aliados)[-c(1,2)] <- stringr::str_remove_all(names(matriz_aliados)[-c(1,2)],"Votos Absolutos.")

matriz_ptat_votos <- alldata %>% 
   filter(nome_candidato %in% candidatos_aliados) %>% 
   inner_join(candptat, by=c("nome_candidato"="candidato")) %>%
   select(nome_candidato, partido_candidato, ptat, Bairro, `Votos Absolutos`) %>% 
   reshape(idvar=c("nome_candidato","partido_candidato","ptat"), timevar="Bairro", direction="wide")
names(matriz_ptat_votos)[-c(1,2,3)] <- stringr::str_remove_all(names(matriz_ptat_votos)[-c(1,2,3)],"Votos Absolutos.")


matriz_aliados%>% write.xlsx("MATRIZ VOTO DE CANDIDATO POR BAIRRO 2020-10-02.xlsx")
matriz_ptat_votos%>% write.xlsx("MATRIZ BAIRRO CANDIDATO PTAT 2020-10-02.xlsx")

nrow(alldata)
write.xlsx(alldata, file = "votos_vereadores_por_bairro.xlsx")
alldata %>% group_by(Bairro) %>% slice_max(order_by = Percentual, n = 5) %>% select(Bairro, nome_candidato, partido_candidato, Percentual,`Votos Absolutos`) %>% write.xlsx(file="ranking_top5_todos_partidos.xlsx")
alldata %>% filter(partido_candidato %in% c("PSDB","DEM","PSD","Republicanos")) %>% group_by(Bairro) %>% slice_max(order_by = Percentual, n = 5) %>% select(Bairro, nome_candidato, partido_candidato, Percentual,`Votos Absolutos`) %>% write.xlsx("ranking_top5_partidos_da_base.xlsx")
