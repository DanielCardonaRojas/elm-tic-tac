import Elm from "../src/Application.elm";

var socketio = require('socket.io-client');

var app = Elm.Elm.Application.init({
    node: document.getElementById("application")
});


const storage = window.localStorage || {
    setItem(k, v) {
        this[k] = v;
    },
    getItem(k) {
        return this[k];
    }
};

// SocketIO 
app.ports.connect.subscribe(function(str){
    console.log("attempting connection to: " + str);
    const socket = socketio(str);
    app.ports.emit_.subscribe(function(obj) {
        //Index tuple to extract message event and data
        socket.emit(obj[0], obj[1]);
    });

    app.ports.listen.subscribe(function(eventName){
        console.log('Listening to event: ' + eventName);
        socket.on(eventName, function (data) {
            console.log('Got event: ' + eventName);
            console.log(data);
            app.ports.on_.send([eventName, data]);
        });
    });
});
