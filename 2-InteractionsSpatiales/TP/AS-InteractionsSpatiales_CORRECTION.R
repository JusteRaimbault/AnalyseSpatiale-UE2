setwd('/Users/Formation/AnalyseSpatiale-UE2-master/AnalyseSpatiale-UE2-master/2-InteractionsSpatiales/TP/')

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


# Matrices de temps de trajet
times = readRDS('data/mobility/listtimes.Rds') 

# Fitter des modèles prenant en compte la distance réseau



# donnees socio-economiques (a l'IRIS): raffiner les modèles
load('data/mobility/socioeco.RData')








