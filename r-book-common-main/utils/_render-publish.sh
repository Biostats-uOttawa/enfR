#!/bin/bash

  echo Rendering

  quarto render

    echo fixing links

  Rscript utils/fix-links.r
  
    echo publishing

  quarto publish --no-prompt --no-render --no-browser gh-pages



