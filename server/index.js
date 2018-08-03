var express = require('express');
var path = require("path");

var app = express();
app.use(express.static(__dirname +'./../'));

var server = require('http').Server(app);
var io = require('socket.io')(server);
server.listen(8000);


app.get('/', function (req, res) {
  res.sendFile(path.resolve('index.html') );
});

io.on('connection', function (socket) {
  socket.emit('news', { hello: 'world' });
  socket.send({ hello: 'world' });
  socket.on('my other event', function (data) {
    console.log(data);
  });
});
