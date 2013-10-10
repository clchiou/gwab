// Copyright (C) 2013 Che-Liang Chiou.


var socket = chrome.socket || chrome.experimental.socket;

// TODO(clchiou): Be sure that chrome.experimental.dns is not removed in
// future version of Chrome.
var dns = chrome.experimental.dns;


var Storage = {};


function js_get(key) { return Storage[key]; }
var js_getBool   = js_get
var js_getInt    = js_get
var js_getString = js_get


function js_put(key, value) { Storage[key] = value; }
var js_putBool   = js_put
var js_putInt    = js_put
var js_putString = js_put


function js_setInterval(msecs, callback) {
  return setInterval(function () { A(callback, [0]); }, msecs);
}


function js_clearInterval(id) {
  clearInterval(id);
}


function js_resolve(host, callback) {
  dns.resolve(host, function(result) {
    A(callback, [[0, result.address], 0]);
  });
}


function js_connect(addr, port, callback) {
  socket.create('tcp', {}, function (createInfo) {
    var socketId = createInfo.socketId;
    if (socketId === 0) {
      console.log('js_connect: Could not create socket');
      A(callback, [[0, socketId], [0, 0], 0]);
      return;
    }
    socket.connect(socketId, addr, port, function (resultCode) {
      console.log('js_connect: socketId=' + socketId);
      A(callback, [[0, socketId], [0, resultCode], 0]);
    });
  });
}


function js_disconnect(socketId) {
  console.log('js_disconnect: socketId=' + socketId);
  socket.disconnect(socketId);
  socket.destroy(socketId);
}


function js_send(socketId, message, callback) {
  stringToArrayBuffer(message, function (arrayBuffer) {
    socket.write(socketId, arrayBuffer, function (writeInfo) {
      A(callback, [[0, writeInfo.bytesWritten], 0]);
    });
  });
}


function js_recv(socketId, callback) {
  socket.read(socketId, null, function (readInfo) {
    if (readInfo.resultCode === 0) {
      console.log('onRead: Could not read: socketId=' + socketId);
      A(callback, [[0, readInfo.resultCode], [0, ''], 0]);
      return;
    }
    arrayBufferToString(readInfo.data, function (str) {
      // NOTE: str could be empty!
      A(callback, [[0, readInfo.resultCode], [0, str], 0]);
    });
  });
}


function stringToArrayBuffer(str, callback) {
  var blob = new Blob([str]);
  var fileReader = new FileReader();
  fileReader.onload = function (e) {
    callback(e.target.result);
  };
  fileReader.readAsArrayBuffer(blob);
}


function arrayBufferToString(buf, callback) {
  var blob = new Blob([new Uint8Array(buf)]);
  var fileReader = new FileReader();
  fileReader.onload = function(e) {
    callback(e.target.result);
  }
  fileReader.readAsText(blob);
}
