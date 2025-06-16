#!/bin/bash

# Demander à l'utilisateur un message de commit
read -p "Message de commit : " message

# Ajouter tous les fichiers
git add .

# Commit avec le message saisi
git commit -m "$message"

# Push vers le dépôt distant
git push