# Crossword Helper

[![Netlify Status](https://api.netlify.com/api/v1/badges/6eba296d-4a6e-4527-8e7c-fc4efd1fedf3/deploy-status)](https://app.netlify.com/sites/wfl-crossword/deploys)

a set of tooling to help construct a NYT-style crossword puzzle.

## Features

- ability to toggle cells between black and white
- symmetry options for painting (180º, horizontal, vertical, none)
- fill in letters (no rebus)
- import/export to text
- indicate the beginnings of clues
- indicate clue numbers

## Future Features

- adjust the size of the grid
- automatically calculate the following statistics:
    - across quantity
    - down quantity
    - quantity per length
    - indication of invalid word lengths (1 or 2)
- fill in rebuses
- encircle spaces
- save/load to localstorage
- randomly generate valid grid
- randomly generate partially-filled, valid grid when given a set of fill words
