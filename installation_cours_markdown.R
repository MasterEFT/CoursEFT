Library <- function(Packages) {
  InstallAndLoad <- function(Package) {
    if (!Package %in% installed.packages()[, 1]) {install.packages(Package, repos="https://cran.rstudio.com/")}
    require(Package, character.only = TRUE)
  }
  invisible(sapply(Packages, InstallAndLoad))
}
# Ajouter les packages nécessaires ici
Library(c("rmarkdown", "bookdown","knitr"))

# télécharger et installer MikTex selon votre susytème d'exploitation
# https://miktex.org/download

library(installr)
install.MikTeX(page_with_download_url = "https://miktex.org/download")
              

# télécharger et installer Rtools depuis le  site
#https://cran.r-project.org/bin/windows/Rtools/
  
  
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

Sys.which("make") ## si cette commande doit ceci "C:\\rtools40\\usr\\bin\\make.exe"


install.packages("devtools")
library("devtools")

install_github("EricMarcon/EcoFoG")
library("EcoFoG")

        
        
        
install.packages('tinytex') # tinytex permet une mise à jour de latex avant chaque tricotages
tinytex::install_tinytex()  # install TinyTeX

library(tinytex)
library(rmarkdown)
library(bookdown)

# ajout commentaire


