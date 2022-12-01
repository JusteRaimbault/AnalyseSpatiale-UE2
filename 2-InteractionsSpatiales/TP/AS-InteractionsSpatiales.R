setwd('~/ComplexSystems/Teaching/2023-DataScience/AnalyseSpatiale-UE2/2-InteractionsSpatiales/TP/')

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



# cartographier avec mf_map





# 1.3) Modeles d'interaction spatiale simples

# fitter un modele avec la distance uniquement pour expliquer le poids des liens (weight)



# essayer en ajoutant d'autre variables



# verifier la presence d'overfitting (fonction AIC)





# 1.4) Modeles contraints (origine et/ou destination)
#  -> utiliser des effets fixes






# 1.5) Modeles de poisson
#  utiliser glm(...,family = poisson(link='log'))) : generalized linear model








#########
# 2) Flux quotidiens en ile-de-france par mode de transport, issus de l'EGT 2010


# Table des flux
idfflows = readRDS('data/mobility/tabflows.Rds')


# Fitter des modèles simples (pour chaque mode, pour l'ensemble des modes)



# Matrices de temps de trajet
times = readRDS('data/mobility/listtimes.Rds') 

# Fitter des modèles prenant en compte la distance réseau



# donnees socio-economiques (a l'IRIS): raffiner les modèles
load('data/mobility/socioeco.RData')








