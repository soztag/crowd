FROM rocker/verse:3.5.2

RUN install2.r checkmate svglite printr DT GGally paran psych ggrepel
RUN installGithub.r maxheld83/pensieve@e36a32f02ec364183197214f9c18c61cc2df1cee