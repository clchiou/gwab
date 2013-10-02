// Copyright (C) 2013 Che-Liang Chiou.

chrome.app.runtime.onLaunched.addListener(function() {
  chrome.app.window.create('gwab.html', {
    bounds: {
      width:  960,
      height: 288
    }
  });
});
