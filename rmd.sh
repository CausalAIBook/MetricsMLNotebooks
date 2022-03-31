#! /bin/bash

for i in `find . -name "*.irnb" -type f`; do
    cp "$i" "$i".ipynb
    jupytext --to "Rmd" "$i".ipynb
    rm "$i".ipynb
done