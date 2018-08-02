FROM node:latest

RUN mkdir /app

WORKDIR /app

COPY package.json package.json

COPY package-lock.json package-lock.json

RUN npm install

RUN npm install elm

COPY elm-package.json elm-package.json

RUN ./node_modules/.bin/elm-package install -y

COPY . /app

RUN npm run build

RUN npm prune --production

EXPOSE 8000

CMD ["npm", "start"]
