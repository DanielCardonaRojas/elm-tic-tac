# Install dependencies

```shell
npm install
```

# Deploy 

Deployment is done with now.sh so have that first

```shell
npm run build
now run deploy
```

# Build and run docker image

```shell
docker build -t decaroj/elm-tic-tac .
docker run -d --name elm-tic-tac -p 8000:8000 decaroj/elm-tic-tac
```
  
