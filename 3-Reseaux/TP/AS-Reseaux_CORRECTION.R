
##
# TP Reseaux

library(igraph)
library(ggplot2)

#####
## Partie 1
# graphes aleatoires


# 1.1) tester la generation de graphe aléatoire: igraph::erdos.renyi.game()
n = 1000
g = erdos.renyi.game(n = n, p.or.m = 0.005)

# calculer la densite
2 * ecount(g) / (vcount(g) * (vcount(g)-1))

# noeuds, liens, attributs
V(g)
E(g)
V(g)$name = paste0("sommet_",1:n)


# definir des poids pour les liens
E(g)$weight = runif(ecount(g))


# matrice d'adjacence
A = as_adj(g)
B = A%*%A
sum(as.numeric(B==0))/(n*n)

# nombre de composante connexes?
composantes = components(g)
composantes

# taille de la plus grande composante en fonction de p
sizes = c()
numcomps = c()
params = seq(from=0.0001,to=0.01, by = 0.0001)
for(p in params){
  g = erdos.renyi.game(n = n, p.or.m = p)
  numcomps=append(numcomps,components(g)$no)
  sizes=append(sizes, max(components(g)$csize))
}
plot(params,numcomps, type='l')
plot(params,sizes, type='l')


# extraire le sous-graphe correspondant a la plus grosse composante
g = erdos.renyi.game(n = n, p.or.m = 0.005)
comps = components(g)
largestcomp = which(comps$csize==max(comps$csize))
v_in_largest = comps$membership==largestcomp
subg = induced_subgraph(g, v_in_largest)



# diametre du graphe
diameter(subg)


# diametre pondere
E(subg)$weight = runif(ecount(subg))
diameter(subg)
diameter(subg, weights = E(subg)$weight)



# diametre en fonction taille et proba du graphe aleatoire
res=data.frame()
for(n in seq(100,500,100)){
  show(n)
  for(logp in seq(-3,-0.5,0.1)){
    for(k in 1:10){
      g = erdos.renyi.game(n = n, p.or.m = 10^logp)
      d=diameter(g, unconnected = T)
      res = rbind(res,c(n,logp,k,d))
    }
  }
}
colnames(res)<-c("n","p","k","d")

ggplot(res,aes(x=p,y=d,col=n,group=n))+geom_smooth()



# plotter le graphe
g = erdos.renyi.game(n = 100, p.or.m = 0.1)
plot(g, vertex.size=0, vertex.label=NA)


# layouts: algorithme de spatialisation du graphe
# -> tester layout fruchterman reingold : layout_with_fr
coords = layout_with_fr(g)
V(g)$x = coords[,1]
V(g)$y = coords[,2]
plot(g, vertex.size=0, vertex.label=NA)



# 1.2) Generer et plotter un graphe en grille (lattice): igraph::make_lattice
g=make_lattice(dimvector = c(50,50))
coords = layout_on_grid(g)
V(g)$x = coords[,1]
V(g)$y = coords[,2]
plot(g, vertex.size=0, vertex.label=NA)


# 1.3) Supprimer des liens aléatoirement;
#  étudier la taille de la plus grande composante connexe
#  en fonction du nombre de lien supprimés
p = 1
liens_gardes = sample.int(n=ecount(g),size=floor(p*ecount(g)),replace = F)
gsub = subgraph.edges(g, liens_gardes, delete.vertices = T)
plot(gsub, vertex.size=0, vertex.label=NA)

# ...



# 1.4) perturber les coordonnées des noeuds de la grille pour obtenir
#  des plus courts chemins uniques;
# étudier le diametre en fonction des liens supprimes
# algos: shortest_paths()/ distances() : algorithme adapte au cas (voir doc)

V(gsub)$x = jitter(factor = 1.0,V(gsub)$x)
V(gsub)$y = jitter(factor = 1.0,V(gsub)$y)
plot(gsub, vertex.size=0, vertex.label=NA)

v_ends = ends(gsub,1:ecount(gsub))
lengths = apply(v_ends, 1, function(e){
  sqrt( (V(gsub)$x[e[1]] - V(gsub)$x[e[2]])^2 + (V(gsub)$y[e[1]] - V(gsub)$y[e[2]])^2 )
})

E(gsub)$weight = lengths


# tous les plus courts chemins: distances
d = distances(gsub)
min(d[d>0])
max(d)
diameter(gsub,unconnected = F)


# certains plus courts chemins: shortest_paths
path = shortest_paths(gsub, from = sample.int(vcount(gsub),1), to = sample.int(vcount(gsub),1))
plot(gsub, vertex.size=5, vertex.label=NA,
     vertex.color = ifelse(V(gsub)%in%path$vpath[[1]],'green', 'black')
     )


# plus court chemin entre coins dans le reseau en grille

# ...







#####
## Partie 2
# Analyse de reseau social
# Data : co-occurence des personnages de A Song of Ice and Fire
#  https://github.com/mathbeveridge/asoiaf

library(readr)
library(igraph)

# 2.1) charger les donnees
# Data available under a CC-BY-NC-SA Licence at https://github.com/mathbeveridge/asoiaf
nodes <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-nodes.csv")
edges <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv")

names(edges)
names(edges)[5] = "occs"

edges[edges$occs==max(edges$occs),]


# construire le graphe: graph_from_data_frame
g = graph_from_data_frame(edges, directed = F, vertices = nodes)

E(g)$weight = 1/E(g)$occs

# 2.2) ploter le graph avec un layout adapte
coords = layout_with_fr(g)
V(g)$x = coords[,1]
V(g)$y = coords[,2]
plot(g, vertex.size=1, vertex.label.cex=0.4)


# pour bien visualiser: gephi, par exemple apres export en gml
# https://gephi.org/
# 
#igraph::write_graph(g,file="",format="gml")

# Alternatives:
# package ggnetwork ~ compatible avec igraph
#  https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html



# 1.3) distribution des degres
degree(g)
deg_pondere = strength(g,weights = E(g)$occs)
deg_pondere[deg_pondere>quantile(deg_pondere,0.99)]

# histogramme
hist(deg_pondere, breaks=200)

# loi rang-taille : log(degre) ~ log(rang)
plot(log(1:length(V(g))), sort(log(strength(g)), decreasing = T))


# Facultatif : ajuster des power law avec plus de parametres, ou des distributions log-normale
# package poweRlaw

# ...



# 1.4) centralites : closeness, betwenness

clos = closeness(g)
clos[clos>quantile(clos,0.99)]

betw = betweenness(g)
betw[betw>quantile(betw,0.99)]

cor.test(clos,betw)

eig = eigen_centrality(g)$vector
eig[eig>quantile(eig,0.99)]


# 1.5) detection de communautes : cluster_... -> methode de Louvain cluster_louvain
coms = cluster_fast_greedy(g)
coms

coms = cluster_louvain(g)
coms



# 1.6) plotter avec multiples infos: communaute, centralite, degre
png('graph.png',width=20,height = 20, units='cm',res=300)
plot(
  g,
  vertex.size = log(strength(g,weights = E(g)$occs))/2,
  vertex.frame.color = NA,
  vertex.color = coms$membership,
  vertex.label.cex = eig
)
dev.off()






#########
## Partie 3 : OSM et reseaux de transports



library(osmdata)
library(sf)

bb <- getbb('paris fr', format_out = 'polygon')
roads <- opq(bbox = bb, timeout = 200) %>% add_osm_feature(key='highway',value='primary') %>% osmdata_sf()
ggplot()+geom_sf(data=roads$osm_lines)
#st_write(...)

restaurants <- opq(bbox = bb, timeout = 200) %>% add_osm_feature(key='amenity',value='restaurant') %>% osmdata_sf()
ggplot()+geom_sf(data=restaurants$osm_points)

# exporter en sp (donnees non compatibles )
#roads <- opq(bbox = bb, timeout = 200) %>% add_osm_feature(key='highway',value='primary') %>% osmdata_sp()

