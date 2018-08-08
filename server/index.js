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
    console.log('new connection');
    socket.on('move', function(data){
        console.log("emiting move");
        console.log(data);
        socket.broadcast.emit('move', data);
    });

    socket.on('join', function(data){
        console.log("emiting join");
        console.log(data);
        socket.broadcast.emit('join', data);
    });

    socket.on('rematch', function(data){
        console.log("emiting rematch");
        console.log(data);
        socket.broadcast.emit('rematch', data);
    });
});
