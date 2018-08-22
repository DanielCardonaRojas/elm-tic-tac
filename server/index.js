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
    console.log('new connection socket id: ' + socket.id);
    //socket.broadcast.emit('new_player', socket.id);
    socket.emit('socketid', socket.id);

    socket.on('move', function(payload){
        console.log('move');
        console.log(payload);
        const data = payload.data;
        const room = payload.room;
        socket.broadcast.to(room).emit('move', data);
    });

    socket.on('createGame',function(data){
        console.log('createGame');
        console.log(data);
        var roomName = data;
        socket.join(roomName);
        socket.emit('newGame', roomName);
    });

    socket.on('joinGame', function(data){
        console.log('joinGame');
        console.log(data);
        const room = data;
        socket.join(room);
        socket.broadcast.to(room).emit('joinedGame', socket.id);
    });

    socket.on('chosePlayer', function(payload){
        console.log('chosePlayer');
        console.log(payload);
        const data = payload.data;
        const room = payload.room;
        socket.broadcast.to(room).emit('chosePlayer', data);
    });

    socket.on('rematch', function(payload){
        console.log('rematch');
        const data = payload.data;
        const room = payload.room;
        console.log(data);
        socket.broadcast.to(room).emit('rematch', data);
    });
});
