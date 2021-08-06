

1_suivi_adultes.R permet d'extraire les données de la bdd du Raspberry terrain (captures d'hiver) et de produire un fichier geojson pour l'application de suivi de terrain.

2_drivetopostgresql_suivi_adultes.R permet de récupérer le fichier de suivi de terrain et de mettre à jour la base de données.

3_suivi_f2ad permet, en fin de saison de suivi des faons (mi-septembre), d'inclure les faons toujours vivants dans le fichier geojson de suivi des adultes pour completer le suivi jusqu'aux captures hivernales suivantes.

