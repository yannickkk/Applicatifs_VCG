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
Ces champs sont signaléS par un astérisque ![image](https://user-images.githubusercontent.com/39738426/125034814-596b0e00-e091-11eb-8d1c-7e83ac4d02fe.png)

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
*Lors de la commande de nouveau modèles de colliers, s'adresser au responsable des systèmes d'information pour mettre à jour les listes de choix.*

