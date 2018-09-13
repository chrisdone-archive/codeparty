// src/Halogen/CodeMirror.js
"use strict";

var mirrors = {};

exports.codeMirror = function(parent){
  return function(config){
    return function(){
      return CodeMirror(parent, config);
    };
  };
}

exports.on = function(codemirror){
  return function(event){
    return function(callback){
      return function(){
        codemirror.on(event,function(doc, change){
          if (change.origin != 'setValue')
            callback();
        });
      }
    };
  };
}

exports.getValue = function(codemirror){
  return function(){
    return codemirror.getValue();
  };
}

exports.setValue = function(codemirror){
  return function(string){
    return function(){
      return codemirror.setValue(string);
    };
  };
}
