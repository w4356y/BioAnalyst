FROM rocker/tidyverse:3.6.2
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("golem")'
RUN R -e 'remotes::install_cran("processx")'
RUN R -e 'remotes::install_cran("attempt")'
RUN R -e 'remotes::install_cran("DT")'
RUN R -e 'remotes::install_cran("glue")'
RUN R -e 'remotes::install_cran("htmltools")'
RUN R -e 'remotes::install_cran("bs4Dash")'
RUN R -e 'remotes::install_cran("dplyr")'
RUN R -e 'remotes::install_cran("RPostgres")'
RUN R -e 'remotes::install_cran("pkgload")'
RUN R -e 'remotes::install_cran("officer")'
RUN R -e 'remotes::install_cran("flextable")'
COPY BioAnalyst_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz")'
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');  BioAnalyst::runApp()"]