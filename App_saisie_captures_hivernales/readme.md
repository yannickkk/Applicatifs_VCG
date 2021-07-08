
Cet applicatif permet la saisie des données des captures hivernales de chevreuils sur tablette. La tablette communique avec Raspberry via un réseau wifi adhoc. le raspberry héberge le seveur shiny et un serveur localhost Postgresql avec les données historiques des animaux.

Le raspberry est également connecté à un lecteur RFID permettant la lecture des tag RFID implantés dans le coup des chevreuils.

![image](https://user-images.githubusercontent.com/39738426/124924562-e1063d80-dffb-11eb-9fb3-e91e9c4a2001.png)

*vue de l'outil en utilisation sur el terrain. L'opérateur point mauve saisie les données sur une tablette android par l'intermédiaire du navigateur internet. A gauche vue du premier onglet de l'app. Sur cet onglet de l'application, l'utilisateur dispose d'un bouton "lire RFID" (en haut à droite). Lorsque l'utilisateur appui sur ce bouton l'appli lit la dernière entrée de la mémoire du lecteur RFID (point rouge). Si l'individu est connu et qu'il dispose d'un transpondeur (tage RFID point jaune), son identifiant (N° Animal) s'affiche sur l'appli ainsi que le code du RFID lu. l'onglet historique permet d'afficher l'ensemble des informations sur l'individus (nombre de captures, données morphologiques lors des précédentes captures, équipement embarqués, durées de suivis, nombre de localisations etc..)*


 ![image](https://user-images.githubusercontent.com/39738426/124926204-87067780-dffd-11eb-8c7e-4978d715afa0.png)

*prélèvement d'achantillon de sang sur un animal. Un prélèvement sur l'application se caractérise par un type (ici sang), une localisation (ici jugulaire), un conditionnement (ici tube rouge), un solvant (ici sec) et un nombre d'échantillons collectés. une fois le prélèvement validé, il s'affiche dans un data table en dessous de la zone de saisie. Les données et les prélèvements front l'objet d'une surveillance de la part de l'applicatif. Ainsi si une mesure ou un prélèvement n'a pas été réalisé, il s'affiche dans l'onglet checklist1 ce qui permet à tout moment à l'opérateur de vérifier quelles mesures ou prélèvements manque encore.* 

![image](https://user-images.githubusercontent.com/39738426/124926974-668aed00-dffe-11eb-9361-20f23b1d0fc2.png)

*la base données est alimentée en temps réelle lors de la saisie comme le montre cette vue de la base de données. Des metadonnées sur le contenant sont ajoutées.*

![image](https://user-images.githubusercontent.com/39738426/124927220-af42a600-dffe-11eb-873d-df555f101853.png)

*ces métadonnées permettent à un utilisateur de la base de données de savoir quelles recherches peuvent être entreprise sur ce type d'échantillons de sang*
