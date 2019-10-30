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
COPY package.json .
RUN npm install
EXPOSE 4000
# ADD . /usr/src/app
CMD [ "npm", "start" ]