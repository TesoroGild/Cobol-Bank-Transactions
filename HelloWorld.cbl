       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloWorld.
       AUTHOR. BG

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CLIENT-FILE ASSIGN TO "clients.txt"
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS WS-FSC.
 
       DATA DIVISION.
       FILE SECTION.
       FD CLIENT-FILE.
       01 CLIENT-LINE PIC X(100).
       
       WORKING-STORAGE SECTION.
       01 WS-CHOICE PIC 9(1) VALUE 1.
       01 WS-FIN-PROG PIC A(1).
       01 WS-LOOP PIC X VALUE 'N'.
           88 OUT-MENU-CHOICE-TRUE VALUE 'O'.
           88 OUT-MENU-CHOICE-FALSE VALUE 'N'.
       01 WS-EOF-FLAG PIC X VALUE "F".
           88 WS-EOF VALUE "T".
       01 WS-NAME PIC X(50).
       01 WS-BIRTHDAY PIC X(50).
       01 WS-LOCATION PIC X(50).
       01 WS-AMOUNT PIC +++B+++B+++B++9.99.
       01 WS-AMOUNT-DISPLAY PIC X(30).
       01 WS-FSC PIC X(2).

       PROCEDURE DIVISION.
           DISPLAY 'Bienvenu.'.
           DISPLAY "".
           
           PERFORM PROC-MENU.
           PERFORM PROC-SELECT-MENU.

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
                    PERFORM PROC-READ-FILE
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
           DISPLAY "MANIPULATION DE FICHIERS"
           DISPLAY "1 - Lecture d'un fichier."
           DISPLAY "2 - Création d'un fichier."
           DISPLAY "3 - Copier un fichier."
           DISPLAY "4 - Afficher les soldes positifs."
           DISPLAY "5 - Trier par ordre décroissant de solde."
           DISPLAY ""
           DISPLAY "GESTION D'UN BULETTIN SCOLAIRE"
           DISPLAY "6 - Ajouter un étudiant."
           DISPLAY "7 - Rechercher un étudiant (avec et sans indice)."
           DISPLAY "8 - Trier le bulettin par nom."
           DISPLAY "9 - Supprimer des valeurs dans un tableau."
           DISPLAY "10 - Comtper les doublons du tableau."
           DISPLAY ""
           DISPLAY "CALCULS ARITHMETIQUES"
           DISPLAY "11 - Somme, moyenne, max, min."
           DISPLAY "12 - Déterminer si une année est bissextile."
           DISPLAY ""
           DISPLAY "0 - Quitter."
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

       PROC-READ-FILE.
           OPEN INPUT CLIENT-FILE
       
           READ CLIENT-FILE
           PERFORM UNTIL WS-EOF
              
              PERFORM PROC-LOOP-FILE
           END-PERFORM
       
           CLOSE CLIENT-FILE
           EXIT.
       
       PROC-LOOP-FILE.
           READ CLIENT-FILE
              AT END SET WS-EOF TO TRUE
              NOT AT END
                 INSPECT CLIENT-LINE REPLACING ALL ";" BY "|"
                 UNSTRING CLIENT-LINE DELIMITED BY "|"
                     INTO WS-NAME, WS-BIRTHDAY, WS-LOCATION, WS-AMOUNT
                 PERFORM PROC-DISPLAY-USERS
           END-READ
           EXIT.

       PROC-DISPLAY-USERS.
           DISPLAY "NOM : " WS-NAME
           DISPLAY "NAISSANCE : " WS-BIRTHDAY
           DISPLAY "LOCALISATION : " WS-LOCATION
           DISPLAY "MONTANT1 :" WS-AMOUNT "$"
           DISPLAY ""
           EXIT.