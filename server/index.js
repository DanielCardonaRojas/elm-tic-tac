var express = require('express');


var app = express();
app.use(express.static("dist"));
var server = require('http').Server(app);
var io = require('socket.io')(server);
server.listen(8000);


app.get('/', function (req, res) {
  res.sendfile('index.html');
});

io.on('connection', function (socket) {
  socket.emit('news', { hello: 'world' });
  socket.on('my other event', function (data) {
    console.log(data);
  });
});
