// src/Websocket.js
"use strict";

exports.connectRaw = function(){
  return new WebSocket(
    window.location.protocol
      .replace(/^http:/,'ws:')
      .replace(/^https:/,'wss:') +
      "//" +
      window.location.host +
      window.location.pathname
  );
}

exports.sendRaw = function(conn){
  return function(string){
    return function(){
      conn.send(string);
    };
  };
};

exports.onmessageRaw = function(conn){
  return function(handler){
    return function(){
      conn.onmessage = function(event){
        handler(event.data)();
      }
    };
  };
};

exports.onopenRaw = function(conn){
  return function(handler){
    return function(){
      conn.onopen = function(){
        handler();
      }
    };
  };
};
