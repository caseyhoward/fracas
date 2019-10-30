FROM node:12.13.0-alpine

ENV TIME_ZONE=America/Los_Angeles
ENV ENV_NAME development
ENV NODE_ENV development
ENV NODE_CONFIG_ENV development
RUN apk --update add tzdata \
  && cp /usr/share/zoneinfo/America/Los_Angeles /etc/localtime \
  && echo "America/Los_Angeles" > /etc/timezone \
  && apk del tzdata
RUN apk add curl
WORKDIR /usr/src/app
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin/
RUN npm install --global --unsafe-perm create-elm-app@4.0.0
COPY package.json .
COPY package-lock.json .
RUN npm install
EXPOSE 3000
CMD [ "elm-app", "start" ]