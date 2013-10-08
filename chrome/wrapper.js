// Copyright (C) 2013 Che-Liang Chiou.


var socket = chrome.socket || chrome.experimental.socket;

// TODO(clchiou): Be sure that chrome.experimental.dns is not removed in
// future version of Chrome.
var dns = chrome.experimental.dns;


function js_logging(message) {
  console.log(message);
}


function js_resolve(host, callback) {
  dns.resolve(host, function(result) {
    A(callback, [[0, result.address], 0]);
  });
}


function js_connect(addr, port, callback) {
  socket.create('tcp', {}, function (createInfo) {
    var socketId = createInfo.socketId;
    if (socketId > 0) {
      socket.connect(socketId, addr, port, function (resultCode) {
        A(callback, [[0, socketId], [0, resultCode], 0]);
      });
    } else {
      console.log('js_connect: Could not create socket');
      A(callback, [[0, socketId], [0, 0], 0]);
    }
  });
}
