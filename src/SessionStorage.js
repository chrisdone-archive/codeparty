// src/SessionStorage.js
"use strict";

exports.putItem = function(key){
  return function(value){
    return function(){
      window.sessionStorage.setItem(key,value);
    };
  };
};

exports.getItem = function(key){
  return function(){
    return window.sessionStorage.getItem(key);
  };
};
