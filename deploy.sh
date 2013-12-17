#!/bin/bash

./site build
echo "Syncing to web server..."
rsync -raz _site/* kyleterry.com:/var/www/kyte.io/
echo "Deploy finished."
