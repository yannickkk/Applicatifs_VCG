
![image](https://user-images.githubusercontent.com/39738426/124928916-53791c80-e000-11eb-8fcd-44d378d3c8eb.png)

*vu de l'application de suivi des adultes. Cette application est remise à jour après chaque campagne de capture hivernale à partir de la base de données. Elle recence tous les animaux équipés d'un collier et d'un boitier GPS ou VHF actif. la couleur du collier associée à celle du boitier forme un code qui permet de reconnaitre l'animal visuellement (en plus de sa fréquence d'émission qui est propre au boitier). l'application affiche les animaux en les représentant par des cercles. La bordure du cercle représente la couleur du collier de l'animal et la couleur du fond du cercle est celle du boitier. Chaque cercle est labelisé avec le numéro de mémoire de l'animal et la semaine de dernier contact. Cet application permet aux opérateurs chargés du suivi de localiser la position des animaux lors de leur controle hebdomadaire. La semaine suivante de nouveaux opérateurs auront instentanément la dernière position connue de chaque animal ce qui offre un gain de temps considérable dans la détection des animaux.*

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

**Collier:** (champ non éditable): indique la couleur du collier de l'animal. Sur l'écran de géolocalisation le pourtour du cercle représentant l'individu est de cette couleur.

**Boitier:** (champ non éditable): indique la couleur du boitier de l'animal. Sur l'écran de géolocalisation le remplissage du cercle représentant l'individu est de cette couleur.

**Alarme_GPS:** (champ éditable): case à cocher qui permet de transmettre aux opérateurs suivant si le collier indique une alarme GPS.


**Alarme_mortalité:** (champ éditable): case à cocher qui permet de transmettre aux opérateurs suivant si le collier indique indique que l'animal est mort (peut être lié à un dysfonctionnement).


**Alarme_intermittente:** (champ éditable): divers alarmes peuvent apparaitre et disparaitre.


**Hors_service:** (champ éditable): plus de signal VHF, l'animal est perdu.

**Cause de fin de suivi:** ce champ permet de rendre compte de la cause qui a conduit à la fin du suivi de l'animal. C'est une liste déroulante qui comporte les choix suivant.

**Mort récupération du collier sur cadavre ou au sol avec signe de prédation** : le collier a été récupéré sur un cadavre de chevreuil ou le collier a été retrouvé au sol avec des marques évidentes de prédation.

**problème VHF**: le signal VHF du collier ne fonctionne plus.

**drop off**: l'opérateur a activé le drop off pour faire tomber le collier.

**problème GPS suivi VHF**: le GPS est en alarme mais l'animal est suivi jusqu'à la perte du signal VHF.

**problème drop off, fin suivi = fin VHF**: le drop off ne fonctionne pas et l'animal est suivi jusqu'à la perte du signal VHF.

**collier au sol**: l'animal a perdu le collier sans qu'il y ai une trace de prédation.

**disperseur collier perdu**: l'animal est un yearling équipé d'un GPS ou d'un VHF et il a disparu entre Mars et fin Avril.


