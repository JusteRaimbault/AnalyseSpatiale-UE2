
##
# TP Reseaux

library(igraph)
library(ggplot2)

#####
## Partie 1
# graphes aleatoires


# 1.1) tester la generation de graphe aléatoire: igraph::erdos.renyi.game()


# calculer la densite


# noeuds, liens, attributs


# definir des poids pour les liens


# matrice d'adjacence



# nombre de composante connexes?



# extraire le sous-graphe correspondant a la plus grosse composante



# diametre du graphe



# diametre pondere



# diametre en fonction taille et proba du graphe aleatoire




# plotter le graphe




# layouts: algorithme de spatialisation du graphe
# -> tester layout fruchterman reingold : layout_with_fr




# 1.2) Generer et plotter un graphe en grille (lattice): igraph::make_lattice




# 1.3) Supprimer des liens aléatoirement;
#  étudier la taille de la plus grande composante connexe
#  en fonction du nommbre de lien supprimés




# 1.4) perturber les coordonnées des noeuds de la grille pour obtenir
#  des plus courts chemins uniques;
# étudier le diametre en fonction des liens supprimes
# algos: shortest_paths()/ distances() : algorithme adapte au cas (voir doc)




# tous les plus courts chemins: distances



# certains plus courts chemins: shortest_paths



# plus court chemin entre coins dans le reseau en grille









#####
## Partie 2
# Analyse de reseau social
# Data : co-occurence des personnages de A Song of Ice and Fire
#  https://github.com/mathbeveridge/asoiaf

library(readr)
library(igraph)

# 1.1) charger les donnees
# Data available under a CC-BY-NC-SA Licence at https://github.com/mathbeveridge/asoiaf
nodes <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-nodes.csv")
edges <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv")



# construire le graphe: graph_from_data_frame



# 1.2) ploter le graph avec un layout adapte



# pour bien visualiser: gephi, par exemple apres export en gml
# https://gephi.org/
# 
#igraph::write_graph(g,file="",format="gml")

# Alternatives:
# package ggnetwork ~ compatible avec igraph
#  https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html



# 1.3) distribution des degres


# histogramme


# loi rang-taille : log(degre) ~ log(rang)



# Facultatif : ajuster des power law avec plus de parametres, ou des distributions log-normale
# package poweRlaw




# 1.4) centralites : closeness, betwenness




# 1.5) detection de communautes : cluster_... -> methode de Louvain cluster_louvain



# 1.6) plotter avec multiples infos: communaute, centralite, degre



