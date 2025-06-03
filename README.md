# Cobol-Bank-Transactions

# Intentions intiales
Ce projet simule un système de gestion de comptes bancaires, similaire à ce que l'on trouve dans les environnements bancaires utilisant COBOL sur mainframe. L'objectif est d'acquérir une vision réaliste des traitements COBOL et des interactions avec les fichiers.

# Résultat final
...

# Installation
...
Installer Visual Studio Code.
Installer les extensions suivantes : IBM Z Open Editor, Zowe explorer, Cobol et Wsl.

Installer Ubuntu (20.04.6 LTS dans ce projet).
Faire les mises à jours : 
```bash
sudo apt update
sudo apt-get update
sudo apt-get install open-cobol.
```

Lancer vscode depuis Ubuntu avec 
```bash
code .
```

Une fois lancé, installer les extensions si nécesaires.

# Créer un fichier
...
Dans vscode, cliquer sur File > New File.. et enregistrer dans le dossier du projet.

# Lancement du projet
...
Ouvrir le terminal. 
Aller dans le dossier du projet
```bash
cd ../../mnt/c/Users/Admin/Desktop/Dossiers/Portfolio/Cobol-Bank-Transactions/
```

Compiler le programme 
```bash
cobc -x fichier.cbl
```

Exécuter le programme 
```bash
./fichier
```