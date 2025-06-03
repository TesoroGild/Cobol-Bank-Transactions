       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloWorld.
       AUTHOR. BG

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(12) VALUE "Jean Dupont".
       01 WS-CHOICE PIC 9(1) VALUE 1.
       01 WS-FIN-PROG PIC X VALUE 'N'.


       PROCEDURE DIVISION.
           DISPLAY 'Bienvenu.'.
           DISPLAY "Quel test veux-tu me faire passer aujourd'hui?".
           DISPLAY "".
           DISPLAY "##################".
           DISPLAY "       MENU       ".
           DISPLAY "##################".
           DISPLAY "".
           DISPLAY "MANIPULATION DE STRING".
           DISPLAY "1 - Extraction de chaine.".
           DISPLAY "2 - Recherche d'un mot dans une chaine.".
           DISPLAY "".
           DISPLAY "MANIPULATION DE TABLEAU".
           DISPLAY "3 - Ajout de valeur à un tableau.".
           DISPLAY "4 - Rechercher un élément (avec un indice).".
           DISPLAY "5 - Trier les valeurs dans un tableau.".
           DISPLAY "6 - Supprimer des valeurs dans un tableau.".
           DISPLAY "7 - Comtper les doublons du tableau.".
           DISPLAY "".
           DISPLAY "CALCULS ARITHMETIQUES".
           DISPLAY "8 - Somme, moyenne, max, min.".
           DISPLAY "9 - Déterminer si une année est bissextile.".
           DISPLAY "0 - Quitter.".

           PERFORM UNTIL WS-CHOICE = 0
              DISPLAY "Votre choix : "
              ACCEPT WS-CHOICE
              DISPLAY ""
              EVALUATE WS-CHOICE
                 WHEN 0
                    EXIT
                 WHEN 1
                    PERFORM PROC-EXTRACT-STRING
                    MOVE 'O' TO WS-FIN-PROG
                 WHEN OTHER
                    DISPLAY "Choix invalide."
                    DISPLAY "Sélectionner une valeur entre 1 et 9."
              END-EVALUATE
           END-PERFORM.

           IF WS-FIN-PROG = "O"
               DISPLAY "A bientôt!"
               STOP RUN.

       PROC-EXTRACT-STRING.
           DISPLAY "EXTRACTION DE CHAINE"
           DISPLAY "Entrer votre nom : "
           EXIT.

       PROC-SEARCH.
      *    COMPTER LE NOMBRE ET RECHERCHE DE MOTS 
           DISPLAY "Entrer votre phrase : "
           EXIT.

       PROC-SCOOL-MANAGMENT.
           DISPLAY "Nom de l'étudiant : "
           DISPLAY "Maths : "
           DISPLAY "Anglais : "
           DISPLAY "Physique : "
           DISPLAY "Histoire : "
           DISPLAY "Philosophie : "
      *    TODO   
           DISPLAY "Rechercher un étudiant..."  
           DISPLAY "Trier le tableau..."
           DISPLAY "Supprimer un étudiant..."
           DISPLAY "Empêcher les doublons..."
           DISPLAY "Moyenne des cours..."
           DISPLAY "Meilleure/pire note dans un cours..."
           EXIT.

       PROC-IS-LEAP-YEAR.
           DISPLAY "Vérifier si une année est bissextile..."
           EXIT.



