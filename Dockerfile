FROM node:alpine

RUN mkdir /app

WORKDIR /app

COPY package.json package.json

COPY package-lock.json package-lock.json

# Skip development dependencies
ENV NODE_ENV production

RUN npm install

COPY . /app

EXPOSE 8000

CMD ["npm", "start"]
