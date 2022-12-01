setwd('AnalyseSpatiale-UE2/2-InteractionsSpatiales/TP/')

#####
# TP : Interaction spatiales
#

#########
# 1) Analyse du reseau Europeen de filiales d'entreprises
#   -> Aires urbaines Fonctionelles Europeennes (base GHSL, Joint Research Center Commission Europeenne)
#   -> liens d'appartenance entre entreprises agreges (poids du lien: turnover pondere)
#   Caracs des aires urbaines: turnover des entreprises, parts de differents secteurs d'activite, pays, population, gdp
#   Caracs des liens: origine, destination, poids, turnover a l'origine, turnover a destination, 
#      pays d'origine, pays de destination, distance geographique, similarite entre structure industrielle
# secteurs d'activite: https://ec.europa.eu/competition/mergers/cases/index/nace_all.html

library(readr)
library(dplyr)
library(sf)
library(mapsf)

# 1.1) Charger les donnees: data/firmsnetwork/{cities.csv,links.csv}
cities = read_csv('data/firmsnetwork/cities.csv')
links = read_csv('data/firmsnetwork/links.csv')


# 1.2) Cartographier la specialisation des aires urbaines
# mf_map(mtq, var = c("", ""), type = "prop_choro")
# fond de carte pays https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
#  -> fichier data/pays/ne_110m_admin_0_countries.shp
pays = st_read(dsn='data/pays/',layer='ne_110m_admin_0_countries')

activites = c("sectorB","sectorC","sectorM","sectorK","sectorG","sectorD","sectorJ", "sectorH","sectorF", 
              "sectorI","sectorO","sectorN","sectorL","sectorS","sectorE","sectorA","sectorR","sectorQ",
              "sectorP","sectorT","sectorU")


# definir une fonction calculant la specialisation dans un secteur donné
#  spec_j = part_locale(activ_j) / part_globale(activ_j)
specialisation <-function(activ_j){
  partlocale = cities[,activ_j] / rowSums(cities[,activites])
  partglobale = sum(cities[,activ_j])/sum(cities[,activites])
  return(partlocale/partglobale)
}

cities$specA = specialisation("sectorA")[,"sectorA"]

# cartographier avec mf_map
sfcities = st_as_sf(cities, coords = c("X","Y"))
st_crs(sfcities)<-4326

mf_base(sfcities,col = 'white')
mf_map(sfcities, type="prop_choro", var=c("turnover","specA"), leg_pos = c(1,1))

#mf_base(pays, xlim=c(-30,10), ylim=c(35,62))
#mf_map(sfcities, type="prop_choro", var=c("turnover","specA"))



#########"
# 1.3) Modeles d'interaction spatiale simples

# fitter un modele avec la distance uniquement pour expliquer le poids des liens (weight)
simplemodel = lm(data = links, formula = log(weight)~log(distance))
summary(simplemodel)

simplemodel_sim = lm(data = links, formula = log(weight)~log(sim))
summary(simplemodel_sim)


# essayer en ajoutant d'autre variables

gravitymodel = lm(data=links, formula = log(weight)~log(distance) +log(sim)+ log(from_turnover)+ log(to_turnover))
summary(gravitymodel)


# verifier la presence d'overfitting (fonction AIC)
AIC(simplemodel)-AIC(simplemodel_sim)
AIC(simplemodel)-AIC(gravitymodel)




# 1.4) Modeles contraints (origine et/ou destination)
#  -> utiliser des effets fixes

# origin
gravitymodel_origin = lm(data=links, formula = log(weight)~log(distance) +log(sim)+ log(from_turnover)+ log(to_turnover) + as.character(from_fua))
summary(gravitymodel_origin)

# destination 
gravitymodel_destination = lm(data=links, formula = log(weight)~log(distance) +log(sim)+ log(from_turnover)+ log(to_turnover) + as.character(to_fua))
summary(gravitymodel_destination)

# contrainte double
gravitymodel_OD = lm(data=links, formula = log(weight)~log(distance) +log(sim)+ log(from_turnover)+ log(to_turnover)+ as.character(from_fua) + as.character(to_fua))
summary(gravitymodel_OD)
AIC(simplemodel) - AIC(gravitymodel_OD)


# effets fixes pays origin / pays destination
gravitymodel_pays = lm(data=links, formula = log(weight)~log(distance) +log(sim)+ log(from_turnover)+ log(to_turnover)+ from_country + to_country)
summary(gravitymodel_pays)
AIC(simplemodel) - AIC(gravitymodel_pays)


##########
# 1.5) Modeles de poisson
#  utiliser glm(...,family = poisson(link='log')) : generalized linear model

links$weight_integer = round(links$weight)
gravity_poisson = glm(data=links, formula = weight_integer~log(distance) +log(sim)+ log(from_turnover)+ log(to_turnover)+ from_country + to_country
                      ,family = poisson(link='log'))
summary(gravity_poisson)

1 - sum((links$weight_integer - fitted(gravity_poisson))^2)/sum((links$weight_integer - mean(links$weight_integer))^2)






#########
# 2) Flux quotidiens en ile-de-france par mode de transport, issus de l'EGT 2010


# Table des flux
idfflows = readRDS('data/mobility/tabflows.Rds')


# Fitter des modèles simples (pour chaque mode, pour l'ensemble des modes)
summary(lm(data=idfflows, log(FLOW)~log(DIST)))

summary(lm(data=idfflows[idfflows$MODE=='TC',], log(FLOW)~log(DIST)))
summary(lm(data=idfflows[idfflows$MODE=='VP',], log(FLOW)~log(DIST)))
summary(lm(data=idfflows[idfflows$MODE=='NM',], log(FLOW)~log(DIST)))

# Matrices de temps de trajet
times = readRDS('data/mobility/listtimes.Rds') 

# jointure
idfflows = left_join(idfflows, times$TC, by=c('ORI'='ORI','DES'='DES'))
names(idfflows)[9] = "TIME_TC"
idfflows = left_join(idfflows, times$VPM, by=c('ORI'='ORI','DES'='DES'))
idfflows = left_join(idfflows, times$VPS, by=c('ORI'='ORI','DES'='DES'))
idfflows$TIME_VP = (idfflows$VAL.x + idfflows$VAL.y) / 2 # temps de trajet en VP moyenne de l'heure de pointe du matin et du soir
idfflows$VAL.x = NULL ; idfflows$VAL.y = NULL

# Fitter des modèles prenant en compte la distance-temps réseau
summary(lm(data=idfflows[idfflows$MODE=='TC',], log(FLOW)~log(TIME_TC)))
# -> meilleur modele en R2 pour le reseau TC

summary(lm(data=idfflows[idfflows$MODE=='VP',], log(FLOW)~log(TIME_VP)))
# -> moins bon pour la voiture


# - donnees socio-economiques (a l'IRIS): raffiner les modèles

# charger dans une liste
socioeco <- mget(load('data/mobility/socioeco.RData', envir=(temp <- new.env())), envir=temp)
#  -> variables intéressantes : pops (populations), incomes, employment

populations = socioeco$pops
# garder seulement la population en 2011
populations = populations[populations$year=="11",]
populations$code_com = substr(populations$id,1, 5)
populations_aggr = as_tibble(populations) %>% group_by(code_com) %>% summarise(population = sum(var,na.rm=T))

# ajouter a la table des flux (origine)
idfflows = left_join(idfflows,populations_aggr,by=c('ORI'='code_com'))
names(idfflows)[11] = "POP_ORI"
# idem destination
idfflows = left_join(idfflows,populations_aggr,by=c('DES'='code_com'))
names(idfflows)[12] = "POP_DES"

# idem avec income et employment
incomes = socioeco$incomes
incomes = incomes[incomes$year=="11",]
incomes$code_com = substr(incomes$id,1, 5)
incomes_aggr = as_tibble(incomes) %>% group_by(code_com) %>% summarise(income = median(var,na.rm=T)) # revenu median
idfflows = left_join(idfflows,incomes_aggr,by=c('ORI'='code_com'))
names(idfflows)[13] = "INC_ORI"
idfflows = left_join(idfflows,incomes_aggr,by=c('DES'='code_com'))
names(idfflows)[14] = "INC_DES"

employment = socioeco$employment
employment_aggr = as_tibble(employment) %>% group_by(id) %>% summarise(employment = sum(var,na.rm=T))
idfflows = left_join(idfflows,employment_aggr,by=c('ORI'='id'))
names(idfflows)[15] = "EMP_ORI"
idfflows = left_join(idfflows,employment_aggr,by=c('DES'='id'))
names(idfflows)[16] = "EMP_DES"


# Modele complet pour chaque mode

summary(lm(data=idfflows[idfflows$MODE=='TC',], log(FLOW)~log(TIME_TC)+log(POP_ORI)+log(POP_DES)+log(INC_ORI)+log(INC_DES)+log(EMP_ORI)+log(EMP_DES)))

summary(lm(data=idfflows[idfflows$MODE=='VP',], log(FLOW)~log(TIME_VP)+log(POP_ORI)+log(POP_DES)+log(INC_ORI)+log(INC_DES)+log(EMP_ORI)+log(EMP_DES)))



