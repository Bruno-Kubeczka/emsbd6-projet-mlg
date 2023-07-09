Fichiers
========

<projet_mlg.R>                  : étude sous la forme d'un script R
<projet_mlg.pdf>                : rapport de l'étude au format PDF
<ref.training_validation.rdata> : image du vecteur aléatoire de partitionnement Training Set / Validation 
                                  utilisé par le script R pour reproduire le contenu du rapport 

<meteo.test.prediction.csv>     : copie du fichier meteo.test.csv dans lequel a été ajoutée par le script R une colonne pluie.demain 
                                  contenant les prédictions

<README.txt>                    : ce fichier


Remarques
=========

** script R et rapport pdf

Le script projet_mlg.R contient l'ensemble du code ayant permis 
- l'analyse exploratoire des jeux de données, 
- l'identification des modèles
- la mesure des capacités prédictives
- la prédiction et la décision

Le rapport au format PDF présente la démarche de l'étude et l'analyse des résultats obtenus.
Il reprend le séquencement du code R pour simplifier les allers-retours entre code et analyse.

** Reproductibilité des résultats présenté dans le rapport

Le script R implémente une méthode de sélection de modèle Holdout partitionnant le jeu de données meteo.train 
en 2 jeux de données : Training Set (80%) et Validation Set (20%), selon un tirage aléatoire des individus.

Afin de reproduire le contenu du rapport, le vecteur aléatoire de booléens ayant servi à l'étude a été sauvegardé dans le fichier <ref.training_validation.rdata>

Quand ce fichier est présent, le script R le charge et l'utilise pour séparer les données meteo.train en un jeu d'entraînement et un jeu de validation.

Quand ce fichier n'est pas présent, le script R crée un nouveau partionnement du jeu de données meteo.train selon un ratio 80/20.
et en crée une sauvegarde dans le fichier ref.training_validation.rdata.
                              