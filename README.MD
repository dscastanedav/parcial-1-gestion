# Instrucciones para ejecutar los scripts R

1. **Clona o descarga esta carpeta en tu computadora.**

2. **Instala R**
   - Descarga R desde [https://cran.r-project.org/](https://cran.r-project.org/) e instálalo con las opciones por defecto.

3. **Instala los paquetes necesarios**
   - Abre la consola de R y ejecuta:
     ```r
     install.packages(c("readr", "dplyr", "ggplot2", "MASS", "strucchange", "zoo", "igraph", "ggraph"))
     ```

4. **Configura la ruta de librerías si es necesario**
   - Si al ejecutar un script aparece un error de paquetes no encontrados o de permisos, abre el archivo `.R` correspondiente (por ejemplo, `01_trend_analysis.R`).
   - Busca el bloque de comentarios al inicio del archivo:
     ```r
     # Si tienes problemas de permisos o R no encuentra los paquetes instalados,
     # puedes especificar manualmente la ruta a tu carpeta de librerías personal.
     # Descomenta y edita la siguiente línea según tu usuario y versión de R:
     # .libPaths("C:/Users/tu_usuario/AppData/Local/R/win-library/4.x")
     ```
   - Descomenta la línea y reemplaza `tu_usuario` y `4.x` por tu usuario de Windows y la versión de R que tienes instalada.

5. **Ejecuta los scripts**
   - Puedes ejecutar los archivos `.R` desde VS Code (con la extensión de R instalada) o desde la consola de R usando:
     ```r
     source("01_trend_analysis.R")
     ```
   - Los resultados y gráficos se guardarán en la carpeta `outputs/figures`.

---

¡Listo! Así puedes ejecutar los análisis sin problemas.
