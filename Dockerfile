FROM rocker/verse:3.5.2

RUN install2.r checkmate
RUN install2.r radix
RUN install2.r svglite
RUN install2.r printr
RUN install2.r DT
RUN install2.r GGally
RUN install2.r paran
RUN install2.r psych
RUN install2.r ggrepel
RUN install2.r pensieve
RUN installGithub.r rstudio/radix
RUN installGithub.r maxheld83/pensieve@6defc5333dafb6710144ea44b05ddecc6aa06cc3