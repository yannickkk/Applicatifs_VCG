
![image](https://user-images.githubusercontent.com/39738426/124928916-53791c80-e000-11eb-8fcd-44d378d3c8eb.png)

*vu de l'application de suivi des adultes. Cette application est remise à jour après chaque campagne de capture hivernale à partir de la base de données. Elle recence tous les animaux équipés d'un collier et d'un boitier GPS ou VHF actif. la couleur du collier associée à celle du boitier forme un code qui permet de reconnaitre l'animal visuellement (en plus de sa fréquence d'émission qui est propre au boitier). l'application affiche les animaux en les représentant par des cercles. La bordure du cercle représente la couleur du collier de l'animal et la couleur du fond du cercle est celle du boitier. Chaque cercle est labelisé avec le numéro de mémoire de l'animal et la semaine de dernier contact. Cet application permet aux opérateurs chargés du suivi de localiser la position des animaux lors de leur controle hebdomadaire. La semaine suivante de nouveaux opérateurs auront instentanément la dernière position connue de chaque animal ce qui offre un gain de temps considérable dans la détection des animaux.*

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
Il est possible de rechercher un animal en cliquant sur l'icone loupe en haut à droite de l'écran et entrant son numéro de mémoire dans la zone de recherche. La carte est alors centrée (au niveau de zoom actuel) sur l'individu concerné.  


![image](https://user-images.githubusercontent.com/39738426/124930439-a0112780-e001-11eb-8a05-0a14a57cf621.png)

*lorsque l'on clique sur un animal, l'application affiche des informations sur celui-ci. Sa mémoire, la semaine de dernier contact que l'on peut changer, le type de collier que porte l'animal, son identifiant (et son nom), les couleurs du collier et du boitier, l'état du collier qui peut être modifié durant le suivi (dysfonctionnement, alarme GPS, alarme mortalité, arrêt total), la cause de fin de suivi, la date de fin de suivi, la date approximative de fin de suivi (champ texte), la date de mort, la cause de mort, le poids à la mort, la longueur de la patte arrière à la mort, si le cadavre a été congelé pour des analyses ultérieures.*

Ses informations permettent une remise à jour de la table animal de la base de données: t_animal_ani pour les individus concernés par le suivi c'est à dire les animaux qui ont des **colliers émetteurs VHF actifs.**

## description des champs du formulaire

### Mémoire:  (champ non éditable): 

c'est la mémoire du collier, elle permet à l'opérateur de rechercher l'animal sur le récepteur VHF avec leuel il travaille sur le terrain.

### Semaine de dernier contact:  (champ éditable): 

numéro de semaine durant laquelle le signal VHF de l'individu a été entendu la dernière fois. Cette information a deux fonctions:

Pour un opérateur de terrain regardant l'application, elle permet de savoir si il doit investir du temps pour rechercher l'individu. 

- C'est le cas si l'individu a été entendu les semaines précédentes et si le collier n'est pas marqué hors_service (voir la section correspondante plus bas). Une attention particulière est à apporter aux animaux yearling (aussi appelés sub adultes voir la section Animal du formulaire) au printemps (Mars à fin Avril) qui peuvent réliser un évènement de dispersion (déplacement rapide à longue distance). Dans ce cas la semaine de dernier contact peut donner une idée de la zone à prospecter pour retrouver l'animal.

- A contrario si l'animal n'a pas été entendu depuis longtemps ou à plus forte raison si la cas Hors_service est cochée, l'opérateur pourra rapidement passer à un autre individu.

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
Il n'est pas complètement inutile d'écouter de temps à autre un individu dont le collier est déclaré hors-service et/ou qui n'a pas été entendu depuis longtemps car leur écoute procure parfois des surprises. 
 
### Type de collier:(champ non éditable)

permet de savoir de quel type de collier est équipé l'animal (VHF/GPS ou GSM).

### Animal:(champ non éditable)

codification reprenant le sexe et l'age (à la capture d'hiver) de l'animal _ l'identifiant humain unique de la base de données (ani_etiq).

Codification pour les femelles et les mâles respectivement: 

**Adultes:**

f_XXXX

m_XXXX

**Yearling:**

fy_XXXX

my_XXXX

**Jeunes:**

fj_XXXX

mj_XXXX


### Collier: (champ non éditable)

indique la couleur du collier de l'animal. Sur l'écran de géolocalisation le pourtour du cercle représentant l'individu est de cette couleur.

### Boitier: (champ non éditable)

indique la couleur du boitier de l'animal. Sur l'écran de géolocalisation le remplissage du cercle représentant l'individu est de cette couleur.

### Alarme_GPS: (champ éditable)

case à cocher qui permet de transmettre aux opérateurs suivant si le collier indique une alarme GPS.


### Alarme_mortalité: (champ éditable)

case à cocher qui permet de transmettre aux opérateurs suivant si le collier indique indique que l'animal est mort (peut être lié à un dysfonctionnement).


### Alarme_intermittente: (champ éditable)

divers alarmes peuvent apparaitre et disparaitre.


### Hors_service: (champ éditable)

plus de signal VHF, l'animal est perdu.

### Cause_fin_suivi: (champ éditable) 

ce champ permet de rendre compte de la cause qui a conduit à la fin du suivi de l'animal. C'est une liste déroulante qui comporte les choix suivant.

**Mort récupération du collier sur cadavre ou au sol avec signe de prédation** : le collier a été récupéré sur un cadavre de chevreuil ou le collier a été retrouvé au sol avec des marques évidentes de prédation.

**problème VHF**: le signal VHF du collier ne fonctionne plus.

**drop off**: l'opérateur a activé le drop off pour faire tomber le collier.

**collier au sol**: l'animal a perdu le collier sans qu'il y ai une trace de prédation.

**disperseur collier perdu**: l'animal est un yearling équipé d'un GPS ou d'un VHF et il a disparu entre Mars et fin Avril.

### Date_fin_suivi: (champ éditable) 

L'application affiche un calendrier qui permet de choisir la date de fin de suivi. Cette date ne correspond pas forcément à la date de mort. Par exemple dans le cas d'un collier au sol sans trace de prédation ou d'un collier en panne, la date de de fin de suivi sera renseignée et la date de mort ne le sera pas. 

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
La date de mort est forcément égale ou postérieure à la date de fn de suivi.

### Date_fin_suivi_text: (champ éditable) 

Si la date n'est pas connue de façon précise, elle peut être renseignée par exemple "mi-Juillet".

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
Si ce champ est renseigné à mi-juillet 2021 par exemple, on pourra aussi mettre la date Date_fin_suivi à 15/07/2021. Cela permet d'avoir une date exploitable en terme d'analyse tout en gardant l'information que celle-ci est approximative.

### Date_mort: (champ éditable) 

L'application affiche un calendrier qui permet de choisir la date de mort. La date de mort n'est pas forcément la date de fin de suivi. Par exemple un animal peut avoir un collier qui tombe en panne le 15 Juillet 2021 et qui est retrouvé mort le 15 Novembre de la même année. Dans ce cas la Date_fin_suivi = 15/07/2021 et Date_mort = 15/11/2021.

### Date_mort_text: (champ éditable) 

Si la date n'est pas connue de façon précise, elle peut être renseignée par exemple "mi-Juillet".

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
Si ce champ est renseigné à mi-juillet 2021 par exemple, on pourra aussi mettre la date Date_mort à 15/07/2021. Cela permet d'avoir une date exploitable en terme d'analyse tout en gardant l'information que celle-ci est approximative.

### Cause_mort: (champ éditable) 

C'est un champ texte qui permet de renseigner la cause de la mort.

### Cause_mort_classe: (champ éditable)

C'est un menu déroulant qui permet de classifier la cause de la mort (permet aux analystes de gagné beaucoup de temps pour trier les données).

Les choix disponibles sont: Chasse, Predation_chien, Predation_loup, Predation_renard, Predation_sanglier, Predation, Accident, Parasitisme/maladie, Collision, inconnue

### Poids_mort: (champ éditable)

Le poids de l'animal mort. Un animal mort n'est pesé que si sa masse est comparable à ce qu'elle serait si l'animal était vivant.

### Lpa_mort: (champ éditable)

La longueur de la patte arrière est mesuré à la maison de terrain avec une toise, sa valeur est données en centimètres

### Lpa_mort: (champ éditable)

Est une cas à coché si l'animal a été conservé au congélateur pour des analyses ultérieures.

### Remarque (champ éditable)

En début de saison ce champ est alimenté pour chaque animal avec des données de capture consernant l'état général de l'animal (blessures, diarrhée) à la capture d'hiver.

### Ani_id (champ non éditable)

est l'identifiant automatique de l'animal dans la base de données.

### Nom

Le nom de l'animal. Seul les animaux capturés faons ont un nom. Le préfixe représente la lignée et le suffixe l'année de naissance.

