// Copyright (C) 2013 Che-Liang Chiou.


var socket = chrome.socket || chrome.experimental.socket;

// TODO(clchiou): Be sure that chrome.experimental.dns is not removed in
// future version of Chrome.
var dns = chrome.experimental.dns;


function ConnectionManager() {
  var self = this;

  var buffers = {};

  function register(socketId) {
    var doFirstPoll = isObjectEmpty(buffers);
    buffers[socketId] = [];
    if (doFirstPoll) {
      poll();
    }
  }
  self.register = register;

  function unregister(socketId) {
    delete buffers[socketId];
  }
  self.unregister = unregister;

  function read(socketId) {
    if (!(socketId in buffers)) {
      console.log('read: Could not find socketId=' + socketId);
      return '';
    }
    var message = buffers[socketId].join('');
    buffers[socketId] = []; // Clear buffer.
    return message;
  }
  self.read = read;

  function poll(socketId) {
    if (isObjectEmpty(buffers)) {
      return;
    }

    for (socketId in buffers) {
      socket.read(parseInt(socketId), null, onRead.bind(self, socketId));
    }

    // Set up next polling.
    setTimeout(poll, 1000);
  }

  function onRead(socketId, readInfo) {
    if (readInfo.resultCode === 0) {
      console.log('poll: Could not read: socketId=' + socketId);
      return;
    }
    arrayBufferToString(readInfo.data, function (str) {
      if (str.length === 0) {
        return;
      }
      buffers[socketId].push(str);
    });
  }

  function isObjectEmpty(obj) {
    return Object.getOwnPropertyNames(obj).length === 0;
  }

  return self;
}


var CONNECTION_MANAGER = new ConnectionManager();


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
    if (socketId === 0) {
      console.log('js_connect: Could not create socket');
      A(callback, [[0, socketId], [0, 0], 0]);
      return;
    }
    socket.connect(socketId, addr, port, function (resultCode) {
      CONNECTION_MANAGER.register(socketId);
      A(callback, [[0, socketId], [0, resultCode], 0]);
    });
  });
}


function js_disconnect(socketId) {
  CONNECTION_MANAGER.unregister(socketId);
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


function js_recv(socketId) {
  return CONNECTION_MANAGER.read(socketId);
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
