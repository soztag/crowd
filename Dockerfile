FROM rocker/verse:3.5.2

RUN install2.r checkmate
RUN install2.r svglite
RUN install2.r printr
RUN install2.r DT
RUN install2.r GGally
RUN install2.r paran
RUN install2.r psych
RUN install2.r ggrepel
RUN installGithub.r rstudio/radix
RUN installGithub.r maxheld83/pensieve@e36a32f02ec364183197214f9c18c61cc2df1cee
