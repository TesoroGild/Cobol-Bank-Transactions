      *#################################################################
      *    IDENTIFICATION
      *#################################################################
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloWorld.
       AUTHOR. BG


      *#################################################################
      *    ENVIRONMENT
      *#################################################################
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTS-FILE ASSIGN TO "clients.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS FC-CLI.
    
           SELECT STUDENTS-FILE ASSIGN TO "etudiants.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS FC-STU.
    
           SELECT FILE-IN  ASSIGN TO "etudiants.txt".
           SELECT FILE-OUT ASSIGN TO "copy.txt".
    
           SELECT FILE-TO-SORT ASSIGN TO "filetosort.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS FC-TOSTR.

           SELECT FILE-SORTED ASSIGN TO "filesorted.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS FC-SORT.

           SELECT WORKFILE ASSIGN TO "OHOOOOO".
 

      *#################################################################
      *    DATA
      *#################################################################
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTS-FILE.
       01 CLIENT-LINE PIC X(100).
       FD STUDENTS-FILE
           RECORD CONTAINS 50 CHARACTERS
           BLOCK CONTAINS 800 CHARACTERS
           DATA RECORD IS FS-STUDENT-RECORD.
       01 FS-STUDENT-RECORD.
           05 FS-ST-NAME PIC X(20).
           05 FS-ST-MATHS PIC ZZ9.
           05 FS-ST-PHYS PIC ZZ9.
           05 FS-ST-SVT PIC ZZ9.
           05 FS-ST-PHILO PIC ZZ9.
           05 FILLER PIC X(18) VALUE SPACES.
        
       FD FILE-IN.
       01 FS-IN-RECORD PIC X(100).
       FD FILE-OUT.
       01 FS-OUT-RECORD PIC X(100).

       FD FILE-TO-SORT.
       01 FS-LINE1 PIC X(100).
       FD FILE-SORTED.
       01 FS-LINE2 PIC X(100).
       SD WORKFILE.
       01 FS-SORT-RECORD.
           05 FS-SRT-ID PIC 9999.
           05 FS-SRT-NAME PIC X(30).
           05 FS-SRT-COUNTRY PIC X(30).
           05 FS-SRT-AMOUNT PIC S9(11)V99.
           05 FILLER PIC X(23).
       

      *#################################################################
      *    WORKING-STORAGE
      *################################################################# 
       WORKING-STORAGE SECTION.
       01 WS-CHOICE PIC 9(1) VALUE 1.
       01 WS-FIN-PROG PIC A(1).

       01 WS-NAME PIC X(50).
       01 WS-BIRTHDAY PIC X(50).
       01 WS-LOCATION PIC X(50).
       01 WS-AMOUNT PIC +++B+++B+++B++9.99.
       01 WS-AMOUNT-DISPLAY PIC X(30).
       01 FC-CLI PIC X(2).

       01 WS-LOOP PIC X VALUE 'N'.
           88 OUT-MENU-CHOICE-TRUE VALUE 'O'.
           88 OUT-MENU-CHOICE-FALSE VALUE 'N'.

       01 WS-EOF-SW PIC 9(1).
           88 WS-EOF VALUE "T".
           88 WS-NOT-EOF VALUE "F".
       
       01 FC-STU PIC 9(2).
       01 WS-NB-ESPACES PIC 99.

       01 FC-TOSTR PIC X(02).
       01 FC-SORT PIC X(02).

       01 WS-TEST1 PIC X(13).
       01 WS-TEST2 PIC +ZZZZZZZZZZ.99.

       01 WS-STORE-CHOICE PIC 9(1) VALUE 1.

       01 WS-YEAR PIC 9(4).
       01 WS-RESLT-DIV PIC 9(4).

       01 WS-SCH-LOOP PIC X VALUE 'N'.
           88 WS-OUT-OF-SCH-LOOP VALUE 'O'.
           88 WS-NOUT-OF-SCH-LOOP VALUE 'N'.

       01 TAB-ITEMS.
           05 WS-ITEMS-RECORD OCCURS 4 TIMES.
              10 WS-IT-NAME PIC X(30).
              10 WS-IT-PRICE PIC ZZZ9.99.
              10 WS-IT-QTE PIC ZZ9.
              10 WS-IT-RATE PIC 9V9.
       01 WS-TAB-IND PIC 9 VALUE 4.
       01 WS-TMP PIC 9(8).
       01 I PIC 9(1).
       01 J PIC 9(1).
       01 WS-STOP-SORT PIC X VALUE 'N'.
           88 WS-STOP-YES VALUE 'O'.
           88 WS-STOP-NO VALUE 'N'.
       01 TMP-ITEM.
           05 TMP-NAME  PIC X(30).
           05 TMP-PRICE PIC ZZZ9.99.
           05 TMP-QTE   PIC ZZ9.
           05 TMP-RATE  PIC 9V9.


      *#################################################################
      *    PROCEDURE (MAIN)
      *#################################################################
       PROCEDURE DIVISION.
           DISPLAY 'Bienvenu.'.
           DISPLAY " ".
           
           PERFORM PROC-MENU.
           PERFORM PROC-SELECT-MENU.

           IF WS-CHOICE NOT = 0
              PERFORM PROC-CONTINUE UNTIL WS-FIN-PROG = "O" 
              OR WS-FIN-PROG = "N".

           DISPLAY "A bientôt!".
           STOP RUN.


      *#################################################################
      *    FUNCTIONS
      *#################################################################
       PROC-CONTINUE.
           DISPLAY "Voulez-vous continuer ? (O/N)".
   
           ACCEPT WS-FIN-PROG.
   
           IF WS-FIN-PROG = "N"
              DISPLAY " "
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
                 WHEN 2
                    PERFORM PROC-SCHOOL-TRANSCRIPT
                    SET OUT-MENU-CHOICE-TRUE TO TRUE
                 WHEN 3
                    PERFORM PROC-COPY-PASTE
                    SET OUT-MENU-CHOICE-TRUE TO TRUE
                 WHEN 4
                    PERFORM PROC-SORT
                    SET OUT-MENU-CHOICE-TRUE TO TRUE
                 WHEN 5
                    PERFORM PROC-STORE-MANAGMENT
                    SET OUT-MENU-CHOICE-TRUE TO TRUE
                 WHEN 6
                    PERFORM PROC-LEAP-YEAR
                    SET OUT-MENU-CHOICE-TRUE TO TRUE
                 WHEN OTHER
                    DISPLAY "Choix invalide."
                    DISPLAY "Sélectionner une valeur entre 0 et 6."
              END-EVALUATE
           END-PERFORM
           EXIT.

       PROC-MENU.
           DISPLAY "##################"
           DISPLAY "       MENU       "
           DISPLAY "##################"
           DISPLAY " "
           DISPLAY "MANIPULATION DE FICHIERS"
           DISPLAY "1 - Lecture d'un fichier."
           DISPLAY "2 - Enregistrement des notes des étudiants."
           DISPLAY "3 - Copie d'un fichier."
           DISPLAY "4 - Triage par ordre croissant."
           DISPLAY " "
           DISPLAY "GESTION D'UN TABLEAU"
           DISPLAY "5 - Simulation de ecommerce." 
           DISPLAY " "
           DISPLAY "CALCULS ARITHMETIQUES"
           DISPLAY "6 - Déterminer si une année est bissextile."
           DISPLAY " "
           DISPLAY "0 - Quitter."
           EXIT.

       PROC-IS-LEAP-YEAR.
           DISPLAY "Vérifier si une année est bissextile..."
           EXIT.

       PROC-READ-FILE.
           OPEN INPUT CLIENTS-FILE
       
           READ CLIENTS-FILE
           PERFORM UNTIL WS-EOF
              
              PERFORM PROC-LOOP-FILE
           END-PERFORM
       
           CLOSE CLIENTS-FILE
           EXIT.
       
       PROC-LOOP-FILE.
           READ CLIENTS-FILE
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
           DISPLAY " "
           EXIT.

       PROC-SCHOOL-TRANSCRIPT.
           OPEN OUTPUT STUDENTS-FILE
           INITIALIZE  FS-STUDENT-RECORD

           PERFORM UNTIL FS-ST-NAME = "0"
              DISPLAY " "
              DISPLAY "Nom de l'étudiant : "
              ACCEPT FS-ST-NAME

              IF FS-ST-NAME NOT = "0"
                 DISPLAY "    Mathématiques : "
                 ACCEPT FS-ST-MATHS
                 DISPLAY "    Physiques : "
                 ACCEPT FS-ST-PHYS
                 DISPLAY "    SVT : "
                 ACCEPT FS-ST-SVT
                 DISPLAY "    Philosophie : "
                 ACCEPT FS-ST-PHILO
                 PERFORM PROC-WRITE
              END-IF   
              DISPLAY " "           
           END-PERFORM

           CLOSE STUDENTS-FILE
           EXIT.

       PROC-WRITE.
           WRITE FS-STUDENT-RECORD

           IF FC-STU NOT EQUAL ZERO
              DISPLAY "Erreur lors de l'ajout"
           END-IF
           EXIT.

       PROC-COPY-PASTE.
           OPEN INPUT FILE-IN
              OUTPUT FILE-OUT

           PERFORM UNTIL WS-EOF
              READ FILE-IN
                 AT END
                    SET WS-EOF TO TRUE
                    DISPLAY "Copie effectué avec succès!"
                    DISPLAY " "
                 NOT AT END
                    MOVE FS-IN-RECORD TO FS-OUT-RECORD
                    
                    IF FS-OUT-RECORD NOT = SPACES
                       WRITE FS-OUT-RECORD
                    END-IF
              END-READ
           END-PERFORM

           CLOSE FILE-IN FILE-OUT
           EXIT.

       PROC-SORT.
           OPEN INPUT FILE-TO-SORT
           IF FC-TOSTR NOT = "00"
              DISPLAY "Erreur à l'ouverture"
              DISPLAY "FS FILE TO SORT: " FC-TOSTR
           END-IF

           OPEN OUTPUT FILE-SORTED
           IF FC-SORT NOT = "00"
              DISPLAY "Erreur à l'ouverture"
              DISPLAY "FS FILE SORTED: " FC-SORT
           END-IF

      *    KEY = FS-SRT-ID, FS-SRT-NAME, FS-SRT-COUNTRY, FS-SRT-AMOUNT
           SORT WORKFILE ON ASCENDING KEY FS-SRT-ID
              USING FILE-TO-SORT
              GIVING FILE-SORTED

           CLOSE FILE-TO-SORT
           CLOSE FILE-SORTED
           EXIT.

       
       PROC-STORE-MANAGMENT.
           PERFORM VARYING I FROM 1 BY 1 
              UNTIL WS-IT-NAME(I) = "0" OR I > 4
              DISPLAY " "
              DISPLAY "Nom de l'article : "
              ACCEPT WS-IT-NAME(I)

              IF WS-IT-NAME(I) NOT = "0"
                 DISPLAY "Prix : "
                 ACCEPT WS-IT-PRICE(I)
                 DISPLAY "Quantité : "
                 ACCEPT WS-IT-QTE(I)
                 DISPLAY "Note : "
                 ACCEPT WS-IT-RATE(I)
              END-IF
           END-PERFORM

           PERFORM PROC-STORE-MENU
           ACCEPT WS-STORE-CHOICE
           PERFORM PROC-LOOP-STORE-CHOICE

           EXIT.

       PROC-STORE-MENU.
           DISPLAY " "
           DISPLAY "--------------------------------------------------"
           DISPLAY "|                Tableau de bord                 |"
           DISPLAY "|------------------------------------------------|"
           DISPLAY "|                                                |"
           DISPLAY "| 1 - Afficher.                                  |"
           DISPLAY "| 2 - Rechercher.                                |"
           DISPLAY "| 3 - Trier.                                     |"
           DISPLAY "| 4 - Supprimer des valeurs dans un tableau.     |"
           DISPLAY "| 5 - Compter les doublons du tableau.           |"
           DISPLAY "| 6 - Empêcher les doublons.                     |"
           DISPLAY "| 7 - Somme, moyenne, max, min.                  |"
           DISPLAY "| 0 - Quitter.                                   |"
           DISPLAY "|                                                |"
           DISPLAY "--------------------------------------------------"
           DISPLAY " "
           DISPLAY "Que voulez-vous faire?"
           EXIT.

       PROC-LOOP-STORE-CHOICE.
           PERFORM UNTIL WS-STORE-CHOICE = 0 OR WS-OUT-OF-SCH-LOOP
              EVALUATE WS-STORE-CHOICE
                    WHEN 0
                       DISPLAY " "
                    WHEN 1
                       PERFORM PROC-DISPLAY-ITEMS
                       SET WS-OUT-OF-SCH-LOOP TO TRUE
                    WHEN 2
                       PERFORM PROC-SEARCH-ITEM
                       SET WS-OUT-OF-SCH-LOOP TO TRUE
                    WHEN 3
                       PERFORM PROC-SORT-ITEMS
                       SET WS-OUT-OF-SCH-LOOP TO TRUE
                    WHEN 4
                       PERFORM PROC-DELETE-ITEMS
                       SET WS-OUT-OF-SCH-LOOP TO TRUE
                    WHEN 5
                       PERFORM PROC-COUNT-ITEMS
                       SET WS-OUT-OF-SCH-LOOP TO TRUE
                    WHEN 6
                       PERFORM PROC-DOUBLE-ITEMS
                       SET WS-OUT-OF-SCH-LOOP TO TRUE
                    WHEN 7
                       PERFORM PROC-CALC-ITEMS
                       SET WS-OUT-OF-SCH-LOOP TO TRUE
                    WHEN OTHER
                       DISPLAY "Choix invalide."
                       DISPLAY "Sélectionner une valeur entre 0 et 6."
                 END-EVALUATE
           END-PERFORM
           EXIT.

       PROC-DISPLAY-ITEMS.
           MOVE 1 TO I
           DISPLAY " "
           DISPLAY "Article                          Prix       "
                 "Qte     Note"
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
              DISPLAY WS-IT-NAME(I) WS-IT-PRICE(I) "       " 
                 WS-IT-QTE(I) "      " WS-IT-RATE(I)
           END-PERFORM
           
           DISPLAY " "
           EXIT.

       PROC-LEAP-YEAR.
           DISPLAY "Année : "
           ACCEPT WS-YEAR

           IF FUNCTION MOD (WS-YEAR, 4) = 0
              AND (FUNCTION MOD (WS-YEAR, 100) NOT = 0
                 OR FUNCTION MOD (WS-YEAR, 400) = 0)
              DISPLAY WS-YEAR " est une année bissextile."
           ELSE
              DISPLAY WS-YEAR " n'est pas une année bissextile."
           END-IF
           EXIT.

       PROC-SEARCH-ITEM.
           EXIT.
       PROC-SORT-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
              SET WS-STOP-NO TO TRUE
              PERFORM VARYING J FROM 4 BY -1 UNTIL J < I 
                 OR WS-STOP-YES
                 IF WS-IT-PRICE(J) < WS-IT-PRICE(J - 1)
                    MOVE WS-ITEMS-RECORD(J) TO TMP-ITEM
                    MOVE WS-ITEMS-RECORD(J - 1) TO WS-ITEMS-RECORD(J)
                    MOVE TMP-ITEM TO WS-ITEMS-RECORD(J - 1)
                    SET WS-STOP-YES TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM

           PERFORM PROC-DISPLAY-ITEMS
           EXIT.
       PROC-DELETE-ITEMS.
           EXIT.
       PROC-COUNT-ITEMS.
           EXIT.
       PROC-DOUBLE-ITEMS.
           EXIT.
       PROC-CALC-ITEMS.
           EXIT.

      *    TODO
      *    APPROFONDISSEMENT/AMELIORATION (VERIF TOUS LES INPUTS)
      *    2- ARRETER D'ECRIRE APRES LA DERNIERE NOT, PAS DE NEW LINE
      *    3- PAS DE LIGNE SUPPLEMENTAIRE APRES LA DERNIERE LIGNE
      *    4- TRIER LES CARACTERES SPECIAUX ET LES SOLDES