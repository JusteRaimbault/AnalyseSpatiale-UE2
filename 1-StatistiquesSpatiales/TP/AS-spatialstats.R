
#####
# TP : Statistiques spatiales


#########
# 1 ) Analyse d'un semis de points : statistiques de synthèse
# Aires urbaines françaises (50 plus grandes) - evolution du centre de gravité - population (1831 - 2000)

#' Données: coordonnées et nom des villes (data/FRUrbanAreas/coords.csv), 
#' populations dans le temps (data/FRUrbanAreas/pop50.csv),
#' dates (data/FRUrbanAreas/dates.csv)
#'  * pas de header
#'  * utiliser de préférence dplyr et readr (tidyverse) 

library(dplyr)
library(readr)

# 1.1) charger les données
# - Question : système de coordonnées?

coords <- read_csv("data/FRUrbanAreas/coords.csv",col_names = F)
colnames(coords)<-c("name","x","y")
  
populations <- read_csv("data/FRUrbanAreas/pop50.csv",col_names = F)
dates <- read_csv("data/FRUrbanAreas/dates.csv", col_names = F)

colnames(populations)<- as.character(dates$X1)

# -  coordonnees coherentes ?



#  - population totale par annee?




# 1.2) calculer point moyen, point moyen pondéré, distance-type pour chaque date

#  - point moyen



#  - point moyen pondere

#  - pour 1831


#  - boucle pour chaque annee


#  -  meme chose avec apply



# 1.3) cartographier les villes et l'évolution du point moyen
#  Question : quel package pour cartographier "simplement" ? -> ggplot (function geom_sf)
#  Données supplémentaires: limites des régions: data/regions/regions_2015_metropole_region.shp
#   ! systèmes de coordonnées à gérer

library(sf)
library(ggplot2)

regions <- st_read(dsn='data/regions/',layer='regions_2015_metropole_region')


#  - voir les drivers disponibles (formats de fichiers)



# - plot point moyen uniquement



#  - "carte" avec point moyen et regions


# -  meme carte "zoomee"



#  - distance type ponderee




# 1.4) faire de même avec le point médian et point médian pondéré
#  package pour calculer le point median en 2d:

library(ICSNP)

#  - point median


#  (difficile) - point median "pondere" ?
#  -> utiliser une technique type bootstrap





#######
#  2 ) Analyse d'un semis de points: 


# 2.1) Charger les données d'OpenStreetMap:
#  * données gpkg
#  * fichiers disponibles: data/osmdata/
#   c("Architecte.gpkg","Courtier_immobilier.gpkg","Hôtels.gpkg",
#    "Auberge.gpkg","École_élémentaire.gpkg","Lycée.gpkg",
#    "Cabinet_avocats.gpkg","École_maternelle.gpkg","Motel.gpkg",
#    "Chambredhôte.gpkg","Ecole_primaire.gpkg","Notaires.gpkg",
#    "Collège.gpkg","Enseignement_Supérieur.gpkg","Salon_de_coiffure.gpkg",
#     "Comptable.gpkg","Géomètre.gpkg")
#   -> a regrouper par type d'activité: éducation, prof. libérales, logements, coiffeurs
#   (choisir une activité ou ne charger qu'un seul fichier pour l'instant)

library(sf)
coiffeurs = st_read("data/osmdata/Salon_de_coiffure.gpkg")
facs = st_read("data/osmdata/Enseignement_Supérieur.gpkg")

st_read(dsn = 'data/regions/',layer = 'regions_2015_metropole_region')

# - systeme de coordonnees?


#  - reprojection vers "EPSG:2154"



# 2.2) Calculer l'indice de plus proche voisin dans le cas d'un faible nombre de points (universités par exemple)



# 2.3) Cartographier la densité des points



# 2.4) Charger le recensement 2017 au niveau départemental (niveau d'agrégation pour lanalyse statistique)
#   * fichier csv population data/insee/Departements.csv
#   * fichier shapefile data/departements/DEPARTEMENT.shp
#  puis agréger les aménités au niveau départemental

library(readr)
library(dplyr)
library(ggplot2)

popdeps = read_delim('data/insee/Departements.csv', delim=";")
deps = read_sf(dsn='data/departements/',layer='DEPARTEMENT')

# - jointure


# 2.5) Corréler les effectifs à la population





# -  joindre les resultats au sf departements



# 2.6) Calculer des indices de concentration

#  - Construction des comptages pour l'ensemble des activites par departement
#  avec une boucle sur les noms de fichier, repeter l'operation precedente d'aggregation et jointure
activityfiles = c(archi="Architecte.gpkg",immo="Courtier_immobilier.gpkg",hotel="Hôtels.gpkg",
                  auberge="Auberge.gpkg",ecole="École_élémentaire.gpkg",lycee="Lycée.gpkg",avocats="Cabinet_avocats.gpkg",
                  maternelle="École_maternelle.gpkg",motel="Motel.gpkg",chambrehote="Chambredhôte.gpkg",
                  primaire="Ecole_primaire.gpkg",notaires="Notaires.gpkg",
                  college = "Collège.gpkg",enssup="Enseignement_Supérieur.gpkg",coiffeur="Salon_de_coiffure.gpkg",
                  comptable= "Comptable.gpkg",geometre="Géomètre.gpkg")




#  - specialisation en ens sup parmi education


# - prof liberales

# - cartographie

library(mapsf)


# 2.7) Calculer l'autocorrélation spatiale

# - a la main (produits de matrices)

#  - Moran avec le package spdep, fonction moran.test
library(spdep)

# - indice de geary


# - indice de Moran local



#########
#  3 ) Geographically weighted regression
#
# Analysis from Bergeaud, A., & Raimbault, J. (2020). An empirical analysis of the spatial variability of fuel prices in the United States. Transportation Research Part A: Policy and Practice, 132, 131-143.
# -> Determinants of US fuel prices at the county level
#

library(sf)
library(dplyr)
library(readr)
library(mapsf)

# 3.1) Charger les données: data/energyprice
countysocioeco <- st_read(dsn='data/energyprice/',layer = 'bea009p020')
counties <- st_read(dsn='data/energyprice',layer = 'county_us_metro')

countydata = read_delim('data/energyprice/county_daily_data.csv',delim = ";",col_names = T)

states <- st_read('data/energyprice',layer = 'us_metro')
taxes<- read_delim('data/energyprice/taxstate.csv',delim=";")
states = left_join(states,taxes,by=c("STUSPS"="state"))

sdata = data.frame(countydata[countydata$type=="Regular",] %>%
                     group_by(countyid) %>%
                     summarise(price=mean(meanprice)))
counties=left_join(counties,sdata,by=c("GEOID"="countyid"))

joineddata = left_join(counties,as_tibble(countysocioeco)[,c("BEA_FIPS","B13_2008","A34_2008","B34_2008","POP_2008","JOB_2008")],by=c("GEOID"="BEA_FIPS"))
joineddata = joineddata[!duplicated(joineddata),]
joineddata[is.na(joineddata)]=0
alldata=joineddata[,c(5,6,10:15)]
names(alldata)<-c("GEOID","name","price","income","jobs","wage","population","percapjobs","geometry")


#  - carte des prix moyens



# 3.2) Tester des modèles GWR à bandwidth fixe
# package GWmodel
library(GWmodel)

# - fitter le modele


# - recuperer les coefficients



# 3.3) Optimiser la bandwidth



# 3.4) Cartographier les coefficients




