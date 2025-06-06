       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloWorld.
       AUTHOR. BG

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(12) VALUE "Jean Dupont".
       01 WS-CHOICE PIC 9(1) VALUE 1.
       01 WS-FIN-PROG PIC A(1).
       01 WS-LOOP PIC X VALUE 'N'.
           88 OUT-MENU-CHOICE-TRUE VALUE 'O'.
           88 OUT-MENU-CHOICE-FALSE VALUE 'N'.

       PROCEDURE DIVISION.
           DISPLAY 'Bienvenu.'.
           DISPLAY "".
           
           PERFORM PROC-MENU.
           PERFORM PROC-SELECT-MENU

           IF WS-CHOICE NOT = 0
              PERFORM PROC-CONTINUE UNTIL WS-FIN-PROG = "O" 
              OR WS-FIN-PROG = "N"
              .

           DISPLAY "".
           DISPLAY "A bientôt!".
           STOP RUN.

       PROC-CONTINUE.
      *     PERFORM UNTIL WS-FIN-PROG NOT = "O" OR WS-FIN-PROG NOT = "N"
              DISPLAY "Voulez-vous continuer ? (O/N)".
   
              ACCEPT WS-FIN-PROG.
   
              IF WS-FIN-PROG = "N"
                  DISPLAY "Arrêt du programme."
              ELSE
                 IF WS-FIN-PROG = "O"
                    SET OUT-MENU-CHOICE-FALSE TO TRUE
                    PERFORM PROC-SELECT-MENU
                 ELSE
                    DISPLAY "Choix invalide."
                    DISPLAY "Sélectionner une valeur entre O et N."
                 END-IF
              END-IF.
      *     END-PERFORM
           EXIT.
           
       PROC-SELECT-MENU.
           PERFORM UNTIL WS-CHOICE = 0 OR OUT-MENU-CHOICE-TRUE
              DISPLAY "Votre choix : "
              ACCEPT WS-CHOICE
              
              EVALUATE WS-CHOICE
                 WHEN 0
                    DISPLAY "Arrêt du programme."
                 WHEN 1
                    PERFORM PROC-EXTRACT-STRING
                    SET OUT-MENU-CHOICE-TRUE TO TRUE
                 WHEN OTHER
                    DISPLAY "Choix invalide."
                    DISPLAY "Sélectionner une valeur entre 1 et 9."
              END-EVALUATE
           END-PERFORM
           EXIT.

       PROC-MENU.
           DISPLAY "##################"
           DISPLAY "       MENU       "
           DISPLAY "##################"
           DISPLAY ""
           DISPLAY "MANIPULATION DE STRING"
           DISPLAY "1 - Extraction de chaine."
           DISPLAY "2 - Recherche d'un mot dans une chaine."
           DISPLAY ""
           DISPLAY "MANIPULATION DE TABLEAU"
           DISPLAY "3 - Ajout de valeur à un tableau."
           DISPLAY "4 - Rechercher un élément (avec un indice)."
           DISPLAY "5 - Trier les valeurs dans un tableau."
           DISPLAY "6 - Supprimer des valeurs dans un tableau."
           DISPLAY "7 - Comtper les doublons du tableau."
           DISPLAY ""
           DISPLAY "CALCULS ARITHMETIQUES"
           DISPLAY "8 - Somme, moyenne, max, min."
           DISPLAY "9 - Déterminer si une année est bissextile."
           DISPLAY "0 - Quitter."
           EXIT.

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
