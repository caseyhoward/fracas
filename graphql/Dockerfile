# For development only

# Change after this is resolved: https://github.com/avh4/elm-format/issues/589
FROM node:12.13.0
# FROM node:12.13.0-alpine

ENV ENV_NAME development
ENV NODE_ENV development
ENV NODE_CONFIG_ENV development
ENV NPM_CONFIG_PREFIX=/usr/src/app/.npm-global

RUN apt-get update -y
RUN apt-get upgrade
RUN apt-get install -y curl inotify-tools

WORKDIR /usr/src/app
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin/
RUN npm install -g npx

WORKDIR /usr/src/app

COPY package.json .
RUN npm install

COPY ./watch.sh watch.sh 
CMD [ "watch.sh" ]
ENTRYPOINT [ "sh" ]