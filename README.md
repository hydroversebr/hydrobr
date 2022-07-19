[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) 
[![license](https://img.shields.io/badge/license-GPL3-lightgrey.svg)](https://choosealicense.com/)

<img align='right' src='https://github.com/hydroversebr/hydrobr/blob/main/man/figures/logo.png' width="100">

<p align="center">
  <span>English</span> |
  <a href="https://github.com/hydroversebr/hydrobr/tree/main/lang/portugues">Português</a>

# hydrobr 


## Motivation
<div style="text-align: justify">
Os dados fluviométricos e pluviométriocos da Agência Nacional de Águas são uma das bases para modelagem hidrológica no Brazil. O pacote hydrobr foi desenvolvido para baixar, limpar e selecionar estações da ANA. Funções com análises estatísticas preliminares em desenvolvimento. O aprimoramento do pacote é aberto à estusiastas. Faça parte da equipe e ajudenos a desenvolver esse projeto. This work is a volunteer initiative from a few Brazilian hydrologists. 
</div>

## Installing this package

You can download and install the most up-to-date version directly from this repository. The procedure is
1. Install the package "devtools" (you only have to do this once. Note that this will also install several dependancies)
2. Load the devtools library
3. Install the package.

The commands are:
``` R
if (!require(devtools)) install.package("devtools")
library(devtools)
install_github("hydroversebr/hydrobr")
```
To read the vignettes and examples of how to use the package:

``` R
install_github("hydroversebr/hydrobr", build_vignettes = TRUE)
vignette(package = 'hydrobr', topic = 'intro_to_hydrobr')
```

## Contact

<div> 
  <a href = "mailto:hydroversebr@gmail.com; tcalegario@gmail.com; daniel_althoff@hotmail.com;"><img src="https://img.shields.io/badge/Gmail-D14836?style=for-the-badge&logo=gmail&logoColor=white" target="_blank"></a>



