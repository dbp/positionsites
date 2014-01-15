$(function () {

  $(document).on("click", "a[data-box=1]", function () {
    var box = $("<div class='box'>");

    var close_container = $("<div class='close-container'>");
    var close = $("<div class='close'>");
    close_container.append(close);
    box.append(close_container);
    close.on("click", function () { box.remove() });

    var div = $("<div class='inner' class='inner'>");
    box.append(div);
    div.html("Loading...");

    $(document.body).append(box);

    var target = $(this).attr("href");

    var refresh_option = $(this).attr("data-refresh") || "none";

    $.ajax(target, {
        success: function (resp, status, xhr) {
            div.html(resp);
            div.find("form").on("submit", function () {
              $.post(target, $(this).serialize(), function () {
                close.click();
                refresh(refresh_option);
              });
              div.html("Submitting...");
              return false;
            });
          }
      });

    return false;
  });

  function refresh(option) {
    if (option === "page") {
      location.reload(true);
    }
  }

});
