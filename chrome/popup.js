chrome.runtime.onMessage.addListener(function(request, sender) {
  if (request.action == "getSource") {
    //message.innerText = request.source;
    var client = new XMLHttpRequest();
    client.open("POST", "http://localhost:3000/linkedin/profile");
    client.setRequestHeader("Content-Type", "text/plain;charset=UTF-8");
    client.send(request.source);
    }
    message.innerText = "done!";

});

function onWindowLoad() {

  var message = document.querySelector('#message');

  chrome.tabs.executeScript(null, {
    file: "getPageSource.js"
  }, function() {
    // If you try and inject into an extensions page or the webstore/NTP you'll get an error
    if (chrome.runtime.lastError) {
      message.innerText = 'There was an error injecting script : \n' + chrome.runtime.lastError.message;
    }
  });

}

window.onload = onWindowLoad;

