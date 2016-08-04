// This recieves messages of type "testmessage" from the server.
Shiny.addCustomMessageHandler("simpleAlert",
  function(message) {
    alert(message);
  }
);

function messengerAlert(type) {
  return function(msg) {
    Messenger().post({
      message: msg,
      type: type
    });
  }
}

Shiny.addCustomMessageHandler("successMessage", messengerAlert("success"));
Shiny.addCustomMessageHandler("warningMessage", messengerAlert("warning"));
Shiny.addCustomMessageHandler("errorMessage", messengerAlert("error"));

Shiny.addCustomMessageHandler("resetFile", function(id) {
  $(id).val('');
  $(id + "_progress").css("visibility", "hidden");
  $(id + " .bar").css("width", "0%");
});

$(function() {
  $("#nav li a").on('click', function(e){
    Shiny.onInputChange(this.getAttribute("data-value") + "-tabChange", Date.now());
  })
});
