FROM rocker/shiny-verse

RUN  echo 'install.packages(c("shinyWidgets"), \
repos=c("http://cran.us.r-project.org", "http://lib.stat.cmu.edu/R/CRAN/"), \
dependencies=TRUE)' > /tmp/packages.R \
  && Rscript /tmp/packages.R

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
