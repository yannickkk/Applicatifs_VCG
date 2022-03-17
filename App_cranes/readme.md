<h3> Cette application permis le référencement des crânes de chevreuils d'Aurignac entrés en collection. </h3>

Elle alimente la table t_animal_ani pour entrer le code collection pour chaque animal connu.
Elle alimente la table listes.t_osteo_os qui référence tous les crânes et permet de générer le csv d' "import de masse" pour collec-science

La page d'accueuil se présente ainsi:

![image](https://user-images.githubusercontent.com/39738426/158818633-7e2e77b1-8a0d-45f9-9030-2cd2a6203b1d.png)

En haut à gauche, l'onglet Documentation permet d'accéder à la présente documentation.

Une liste de choix nommée Animal affiche par défaut "inconnu" et permet de sélectionner un animal de la bd_cefs.

"inconnu" permet d'entrer en collection un animal inconnu (non i.e. non marqué).

à coté de ce menu déroulant est affiché la code collection de l'individu.

Il est composé de deux chiffres et d'une ou deux lettres. 

- Le premier triplet est un chiffre automatique permettant de s'assurer que le numéro est unique.

- Le second triplet est un chiffre ou un caractère alpha numérique qui reprend ani_etiq de db_cefs.

- enfin la lettre A dsigne le sit d'étude (A pour Aurignac aussi appelé VCG)
