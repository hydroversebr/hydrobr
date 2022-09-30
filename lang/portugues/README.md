[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) 
[![license](https://img.shields.io/badge/license-GPL3-lightgrey.svg)](https://choosealicense.com/)

<img align='right' src='https://github.com/hydroversebr/hydrobr/blob/main/man/figures/logo.png' width="100">

<p align="center">
  <span>Português</span> |
  <a href="https://github.com/hydroversebr/hydrobr">English</a>

# hydrobr 

## Descrição

O pacote hydrobr foi desenvolvido para ajudar os usuários a selecionar, baixar e limpar dados de estações pluviométricas e fluviométricas da Agência Nacional de Águas. Os dados são disponibilizados pela ANA é uma das principais bases de dados para estudos hidrológicos no Brasil. Esta é uma versão beta (versão 0.0.0.9000), que foi testada pelos desenvolvedores em algumas bacias brasileiras. Por favor, certifique-se de que os processamentos estejam corretos em seu estudo de caso por meio de uma verificação cruzada e nos informe. Funções para análises estatísticas estão atualmente em desenvolvimento.

Este é uma iniciativa voluntária de alguns hidrólogos brasileiros e faz parte do <a href="https://github.com/hydroversebr/">hydroversebr</a>. O aprimoramento do pacote é aberto à entusiastas. Contate-nos se você quer fazer parte do time e ajude-nos a desenvolver esse projeto.

## Instalando o pacote

Você pode baixar e instalar a versão mais atualizada do pacote a partir desse diretório. O procedimento é:
1. Instalar o pacote "devtools" (Você só precisa fazer isso uma vez. Note que serão instaladas várias dependências.)
2. Carregar o pacote "devtools"
3. Instalar o pacote hydrobr

Os comandos são:
``` R
if (!require(devtools)) install.package("devtools")
library(devtools)
install_github("hydroversebr/hydrobr", build_vignettes = TRUE)
```

Para ler os vignettes e examplos de como usar:
``` R
vignette(package = 'hydrobr', topic = 'intro_to_hydrobr')
```


## Contato

<div> 
  <a href = "mailto:hydroversebr@gmail.com; tcalegario@gmail.com; daniel_althoff@hotmail.com;"><img src="https://img.shields.io/badge/Gmail-D14836?style=for-the-badge&logo=gmail&logoColor=white" target="_blank"></a>

