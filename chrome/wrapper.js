// Copyright (C) 2013 Che-Liang Chiou.


//// jQuery


var js_jquery = $;


function jquery_on(events, elements, callback) {
  return elements.on(events, function(event) {
    A(callback, [[0, event.which], 0]);
  });
}
var js_keyup    = jquery_on.bind(window, 'keyup');
var js_keydown  = jquery_on.bind(window, 'keydown');
var js_keypress = jquery_on.bind(window, 'keypress');


//// Store


var Storage = {};


function js_get(key) { return Storage[key]; }
var js_getBool   = js_get
var js_getInt    = js_get
var js_getString = js_get


function js_put(key, value) { Storage[key] = value; }
var js_putBool   = js_put
var js_putInt    = js_put
var js_putString = js_put


//// Utils


function js_convertEncoding(encoding, str, callback) {
  var array = new Uint8Array(str.length);
  for (var i = 0; i < str.length; i++) {
    array[i] = str.charCodeAt(i);
  }
  var fileReader = new FileReader();
  fileReader.onloadend = function (e) {
    var blob = new Blob([e.target.result], {type: 'application/octet-binary'});
    fileReader.onloadend = function (e) {
      A(callback, [[0, e.target.result], 0]);
    };
    fileReader.readAsText(blob, encoding);
  };
  fileReader.readAsArrayBuffer(new Blob([array]));
}


function js_setInterval(msecs, callback) {
  return setInterval(function () { A(callback, [0]); }, msecs);
}


function js_clearInterval(id) {
  clearInterval(id);
}


//// Socket


var socket = chrome.socket || chrome.experimental.socket;

// TODO(clchiou): Be sure that chrome.experimental.dns is not removed in
// future version of Chrome.
var dns = chrome.experimental.dns;


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
    var rawData = new Uint8Array(readInfo.data);
    var str = [];
    for (var i = 0; i < rawData.length; i++) {
      str.push(String.fromCharCode(rawData[i]));
    }
    str = str.join('');
    A(callback, [[0, readInfo.resultCode], [0, str], 0]);
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
