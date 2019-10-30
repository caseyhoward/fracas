# For development only

FROM node:12.13.0-alpine

ENV TIME_ZONE=America/Los_Angeles
ENV ENV_NAME development
ENV NODE_ENV development
ENV NODE_CONFIG_ENV development

RUN apk --update add tzdata \
  && cp /usr/share/zoneinfo/America/Los_Angeles /etc/localtime \
  && echo "America/Los_Angeles" > /etc/timezone \
  && apk del tzdata

WORKDIR /usr/src/app

COPY graphql-generator/graphql-watch.sh /usr/src/app/graphql-generator/graphql-watch.sh 
COPY package.json .
COPY package-lock.json .
RUN npm install



# RUN apk add curl
# WORKDIR /usr/src/app
# RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
# RUN gunzip elm.gz
# RUN chmod +x elm
# RUN mv elm /usr/local/bin/
CMD [ "graphql-generator/graphql-watch.sh" ]



