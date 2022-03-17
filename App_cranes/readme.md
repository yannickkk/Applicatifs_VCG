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

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png) Ce code doit être reporté scrupuleusement au **marqueur indélébile** fin sur **chaque pièce ostéologique** de façon à pouvoir les réaffecter si d'aventure elles étaient mélangées lors de leur utilisation.

Etagère (douchette) est un champ texte. Les étagères de la salle de collection sont identifié avec une étiquette collée dessus et faisant apparaitre un Qrcode.

Pour indiquer sur quelle étagère se trouve le crâne, il faut cliquer sur la zone de texte, celle ci est alors surlignée en bleu comme sur cette capture d'écran:

![image](https://user-images.githubusercontent.com/39738426/158820681-eeb28a65-2aa4-4225-8a91-400a8feb78ae.png)

Flasher alors le Qrcode avec la douchette et le résultat de la lecture s'affiche alors dan sla zone de texte.

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png) Ne jamais entrer ce code à la main. En effet la moindre erreur typographique empèchera Collec Science de reconnaitre la localisation du crâne.

On doit ensuite renseigner le positionnement du crâne sur l'étagère.

Pour les lignes, 1 signifie que le crâne est sur la première rangé de l'étagère (devant), 2 qu'il se trouve sur al seconde (derrière). Si les crânes sont 

dans un carton, celui-ci occupera certainement les deux rangées. Dans ce cas laisser 0 pour les lignes. 

Les colonnes quand à elles sont numérotées de gauche à droite.

La date de collecte est par défaut la date du jour elle doit être renseignée avec les indications fournies avec le crâne (pour les animaux inconnus 

seulement, pour les animaux connus c'est la date mort (ani_date_mort) qui sera automatiquement affecté au crâne. Si la date n'est pas connues, laisser la 

date en blanc.

Un champ remarque permet à l'utilisateur de reporter toute information qu'il juge utile.

Dnas le cas d'un animal marqué, on ne connait pas toujours son identifiant dans la base de données. Il s'agit généralement de la plus petite étiquette 

oriculaire placé sur l'animal mais au fil du temps ses marquas peuvent être perdues et changées sans que l'animal ne change de nom.

<h3> Retrouver le numéro de l'animal:</h3>

lorsqu'une marque plastique ou métal est présente dans le sachet qui contient le crâne, l'animal est marqué. on peut alors essayé d'entrer le numéro de la 

bague dans cap_tag_ droit puis dans cap_tag_gauche. Par exemple dans les captures d'écran ci-dessous je recherche l'animal portant la marque 1119
