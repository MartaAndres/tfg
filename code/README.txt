# Scripts de R usados en el análisis de las medidas

## Software necesario
Es necesario tener R instalado. Además, se requiere la instalación de algunos paquetes de R, que son (posiblemente lista no exhaustiva):

* caret
* lattice
* ggplot2
* RKEEL
* smotefamily
* R.utils
* R.oo
* R.methodsS3


## Uso

Los scripts que se lanzan desde la terminal son los ficheros que tienen como
extensión .Rscript. Los demás ficheros, de extensión .R contienen las funciones
que usan los scripts pero si se ejecutan solos no harán nada.

### Generar datasets artificiales

Usar en la terminal el comando:
   Rscript generate-datasets.Rscript
o para usar la semilla que yo he usado, 28, y obtener los mismos dataset, usar:
   Rscript generate-datasets.R 28

### Hacer pruebas sobre los datasets artificiales

Usar en la terminal el comando:
   Rscript test-artificial.Rscript

Los resultados aparecerán en el directorio ./results/ en un fichero llamado
results-artificial tanto en formato .csv como en un .txt que se puede leer
a una variable de R con dget.

### Hacer pruebas sobre los datasets artificiales

Usar en la terminal el comando:
   Rscript test-real.Rscript

Los resultados aparecerán en el directorio ./results/ en un fichero llamado
results-real tanto en formato .csv como en un .txt que se puede leer a una
variable de R con dget.
