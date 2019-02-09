# this step fails when action is not already running, but doesn't matter will continue
docker stop crowd
# this builds the image anew, but this is really fast if there are no changes
docker build --tag=crowd .
# this starts the image and maps the repo directory
docker run \
  --env=PASSWORD=foo `# some environmental vars` \
  --rm  `# make container ephemeral` \
  -d \
  --volume=/Users/Verena/GitHub/crowd:/home/rstudio/crowd \
  --publish=8787:8787  `# port mapping, host left, cont right`\
  --name="crowd" \
  crowd `# the image`
