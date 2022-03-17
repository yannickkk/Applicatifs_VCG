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

Etagère (douchette) est un champ texte. Les étagères de la salle de collection sont identifié avec une étiquette collée dessus et faisant apparaitre un Qrcode.

Pour indiquer sur quelle étagère se trouve le crâne, il faut cliquer sur la zone de texte, celle ci est alors surlignée en bleu comme sur cette capture d'écran:

![image](https://user-images.githubusercontent.com/39738426/158820681-eeb28a65-2aa4-4225-8a91-400a8feb78ae.png)

Flasher alors le Qrcode avec la douchette et le résultat de la lecture s'affiche alors dan sla zone de texte.

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png) Ne jamais entrer ce code à la main. En effet la 

moindre erreur typographique empèchera Collec Science de reconnaitre la localisation du crâne.

