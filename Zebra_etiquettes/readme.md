
Le script **labels_38_12.7_Zebra420T.R** permet à partir d'une liste de prélèvements entrés dans un fichier excel de produire les étiquettes correspondantes pour l'imprimante zebra de terrain.


**labels_38_12.7_Zebra420T_V02** fait la même chose mais en permettant l'identification de chaque échantillons prélevé. On définit un vecteur avec le nom des échantillons qui constituent les prélèvements réalisés sur l'animal et le script génère les étiquettes correspondantes classées dans des sous dossiers portant le nom de l'échantillon.
Le QRcode est au format text. Pour être lu par [collec-science](https://www.collec-science.org/) il ne doit comporter que le code échantillon métier.


![image](https://user-images.githubusercontent.com/39738426/126763694-c196d80f-9a24-4a86-8caf-e6e63cb8160d.png)
*Exemple d'étiquette généré pas les scripts. Attention les _ sont remplacés par des - dans le data-matrix*

Le script **labels_38_12.7_Zebra420T_V03.R** code le qrcode en json pour la lecture dans [collec-science](https://www.collec-science.org/)

![image](https://user-images.githubusercontent.com/39738426/125033795-0fcdf380-e090-11eb-93de-3538ba08a5b4.png)
**ATTENTION**: après discussion avec Eric Quinton créateur de collec-science le champ id n'est pas lu par la version actuelle du logiciel: 2.6.0. Seuls les champs db et uid sont lus dans le json, labels_38_12.7_Zebra420T_V03.R est donc à modifier dasn ce sens, utilisez plutôt **labels_38_12.7_Zebra420T_V02** pour l'instant.

![image](https://user-images.githubusercontent.com/39738426/139062499-2b5e8235-1c5e-4602-ba2c-59ef4848d6bd.png)
*Exemple d'étiquette généré pas le script **labels_38_12.7_Zebra420T_V03.R**. Le code lu dans le Qrcode n'est pas A_20220222_3156_serum mais {"id":"A_20220222_3156_serum"} 
Attention les _ sont remplacés par des - dans le data-matrix*


