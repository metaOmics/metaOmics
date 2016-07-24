// This recieves messages of type "testmessage" from the server.
Shiny.addCustomMessageHandler("simpleAlert",
  function(message) {
    alert(message);
  }
);

Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
  var id = "#" + x + "_progress";
  var idBar = id + " .bar";
  $(id).css("visibility", "hidden");
  $(idBar).css("width", "0%");
});

function reset_select() {
  var $dataset = $('#preproc-dataset').selectize();
  var control = $dataset[0].selectize;
  control.clear();
}

function reset_file(value, $item) {
  var id = '#preproc-file'
  $(id).val('');
  $(id + "_progress").css("visibility", "hidden");
  $(id + " .bar").css("width", "0%");
}

$(function() {
  $("#nav li a").on('click', function(e){
    Shiny.onInputChange(this.getAttribute("data-value") + "-tabChange", Date.now());
    console.log("tab change");
  })
  $("input:file").change(reset_select);
});
