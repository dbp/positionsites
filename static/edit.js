$(function () {

  $(document).on("click", "a[data-box=1]", function () {
    var box = $("<div class='ps-box'>");

    var overlay = $("<div class='ps-overlay'>");
    var close = $("<div class='close'>&times;</div>");

    // clicking on background should close
    overlay.on("click", function () { close.click(); });
    $(document.body).append(overlay);

    // clicking on close button should close
    box.append(close);
    close.on("click", function () {
      box.remove();
      overlay.remove();
    });

    // escape should close
    $(document).keyup(function(e){
      if (e.keyCode === 27) {
        close.click();
      }
    });


    var div = $("<div class='inner'>");
    box.append(div);
    div.html("Loading...");

    $(document.body).append(box);


    var target = $(this).attr("href");

    var refresh_option = $(this).attr("data-refresh") || "none";

    $.ajax(target, {
        success: load_box
      });

    return false;

    function load_box(resp, status, xhr) {
      div.html(resp);
      div.find("form").attr("action", target).ajaxForm({success: handle_response}).on("submit", function () {
        $(this).ajaxSubmit();
        div.html("Submitting...");
        return false;
      });
    }

    function handle_response(resp, status, xhr) {
        console.log(xhr.status);
      if (xhr.status === 201) {
        close.click();
        refresh(refresh_option);
      } else {
        load_box(resp, status, xhr);
      }
    }

  });

  function refresh(option) {
    if (option === "page") {
      location.reload(true);
    }
  }

});
