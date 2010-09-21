rsync -rav --exclude .git --exclude '**~' --delete --delete-excluded . rubeneshuis@rubeneshuis.remworks.net:app
ssh -t rubeneshuis.remworks.net sudo restart rubeneshuis