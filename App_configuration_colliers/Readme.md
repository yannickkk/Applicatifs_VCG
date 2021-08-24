# Documentation de l'application de saisie des nouveaux colliers et leur configuration annuelle pour le site d'étude des Vallons et Coteaux de Gascogne

Cet applicatif sert à alimenter la base de données du site de suivi in-natura de Vallons et Coteaux de Gascogne (db_cefs).

Les tables alimentées par cet applicatif sont contenues dans le schéma main. Elles se nomment:

> t_equipement_eqt
>
> tj_equiment_conf_eqc
>

<h2 align="center">Vue général de l'application de saisie des colliers.</h2>

![image](https://user-images.githubusercontent.com/39738426/130599362-d0a8be18-3b64-4784-ae44-71d3b31dd6b9.png)

*L'application se compose de deux parties: 1-le formulaire de saisie des nouveaux colliers; 2-le formulaire de saisie de la confguration annuelle des colliers.*

## 1- Documentation

Un doucle click sur "Docmention" en haut à gauche renvoi vers la présente documentation

## 2- Caractéristiques du collier

Cet onglet sert à saisir les caractéristiques d'un nouveau collier.

Certains champs sont obligatoires. Celà signifie que si ils ne sont pas remplis, on en peut pas enregistrer les informations saisies.
Ces champs sont signalés par un astérisque ![image](https://user-images.githubusercontent.com/39738426/125034814-596b0e00-e091-11eb-8d1c-7e83ac4d02fe.png)

C'est le cas de :

**N°collier**: est l'identifiant usuel du collier donné par le constructeur pour les colliers GSM et GPS, il n'existe pas pour les colliers VHF. Pour ces derniers il est définit automatiquement lorsque VHF est sélectionné dans la liste déroulante Type de collier.

**Type de collier**: est une liste déroulante comprenant les types de colliers "GPS", "GSM", "VHF".

**Marque du collier**: liste les marques utilisées au laboratoire.

**Modèle du collier**: liste des modèles utilisés au laboratoires.

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
*Les choix sont imbriqués, ainsi le choix d'un type de collier, contraint les marques disponibles et les marques contraignent les modèles disponibles.* 

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
*Seule la fréquence de l'émetteur VHF n'est pas un champ obligatoire. cependant son alimentation est fortement recommendée de façon à pouvoir ensuite rappeler le collier sans avoir à retenir son identifiant automatique.*

Une fois les caractéristiques intrinsecques du collier saisie, la validation se fait en pressant el bouton "submit"

![image](https://user-images.githubusercontent.com/39738426/130601782-4ce1e93f-eae4-418a-b2f4-7318105d853a.png)
*exemple de la saisie d'un collier fictif montrant les différents modèles de colliers GPS disponibles pour la marque Vectronic Aerospace.*

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
*Lors de la commande de nouveaux modèles de colliers, s'adresser au responsable des systèmes d'information pour mettre à jour les listes de choix.*

## 3- Configuration du collier

Une fois le collier créé, l'utilisateur doit le configurer pour l'année de suivi durant laquelle il va être utilisé.

Cet onglet de l'application est constitué d'une zone de saisie (en haut) et d'une zone d'affichage qui permet de consulter les configurations présentent dans la base de données en choisissant une année de suivi. 

![image](https://user-images.githubusercontent.com/39738426/130606353-b8c0d135-7fbd-4853-9afc-4e9b7c7a5576.png)
*Dans l'exemple ci-dessus, l'utisateur affiche toutes les configurations pour l'année de suivi 2019*

**Configuration d'un collier**


![image](https://user-images.githubusercontent.com/39738426/130607007-b5ccf67c-bebc-439d-82ee-0d9b94b766a1.png)
*recherche du collier précédemment créé à l'aide de son identifiant. L'autocomplétion permet de faciliter cette recherche*

Certains champs sont obligatoires. Celà signifie que si ils ne sont pas remplis, on en peut pas enregistrer les informations saisies.
Ces champs sont signalés par un astérisque ![image](https://user-images.githubusercontent.com/39738426/125034814-596b0e00-e091-11eb-8d1c-7e83ac4d02fe.png)

C'est le cas de :

**N° du collier**: c'est l'identifiant usuel du collier. Dans le cas d'un colliers VHF pour lequel l'identifiant constructeur n'existe pas, il est conseillé d'effectuer la recherche par la fréquence du collier (champ Recherche freq VHF). La fréquence n'est pas nécessairement unique aussi l'application sélectionnera le cas échéant, le dernier collier entré pour cette valeur de fréquence.

**Année de suivi**: c'est l'année au cours de laquelle le collier va être déployé sur un animal et assurer son suivi.

**Mémoire récepteur**: un entier de 1 et 257 qui défini la mémoire du collier dans les récepteurs utilisés sur le terrain.

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
*Le numéro de mémoire doit être unique. Si le numéro de mémoire existe déjà pour l'année sélectionnée, l'application affiche une pop-up d'alerte et propose l'annulation de la saisie ou la mise à jour du collier qui correspond à cette mémoire*

![image](https://user-images.githubusercontent.com/39738426/130608527-b0c49275-9655-4bd8-892f-2b7520378365.png)
*exemple d'une tentative d'utilisaion d'une mémoire déjà attribuée à un autre collier pour l'année sélectionnée. Un message d'alerte laisse le choix d'annuler la saisie ou de mettre à jour les données de collier pour cette mémoire*

**Capteurs associés**: Capteurs associés au collier GPS/GSM/VHF. Donne le choix d'un ensemble de capteurs: activité, proximité, accéléromètre, magnétomètre.

