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
          if (!change || change.origin != 'setValue')
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

exports.getSelection = function(codemirror){
  return function(){
    if (codemirror.listSelections().length < 1)
      throw "assert failed: codemirror.listSelections.length";
    console.log('getSelection: %o', codemirror.listSelections()[0]);
    return codemirror.listSelections()[0];
  };
}

exports.setSelection = function(codemirror){
  return function(sel){
    return function(){
      return codemirror.setSelection(sel.anchor, sel.head);
    };
  };
}
