<apply template="base">
  <script>
    $(function () {
    $(".body-textarea").each(function (_, elem) {
    var editor = CodeMirror.fromTextArea(elem, {
      mode: "text/html",
      lineNumbers: true,
      readOnly: "nocursor",
      viewportMargin: Infinity
    });
    });
    $(".fields-textarea").each(function (_, elem) {
    var editor = CodeMirror.fromTextArea(elem, {
      mode: "javascript",
      readOnly: "nocursor",
      viewportMargin: Infinity
    });
    });
    });
  </script>

  <h1 class="title">Dashboard for domain <domain/> (<a href="/site/${site_id}/edit">edit</a>).</h1>

  <h3 class="section-heading">Data Types (<a href="/site/${site_id}/data/new">add</a>)</h3>
  <div class="section">
    <data>
      <div class="section-elem">
        <div class="section-name"><name/></div>
        <table class="section-table">
          <tr>
            <td class="name">Definition</td>
            <td><textarea class='fields-textarea'><fields/></textarea></td>
          </tr>
          <tr>
            <td class="name">Items</td>
            <td><item-count/></td>
          </tr>
        </table>
      </div> <!-- .section-elem -->
    </data>
  </div> <!-- .section -->


  <h3 class="section-heading">Pages (<a href="/site/${site_id}/page/new">add</a>)</h3>
  <div class="section">
    <pages>
      <div class="section-elem">
        <div class="section-name">
          <a target="_blank" href="http://${domain}${server-port}${flat}">
            <structured/>
          </a>
        </div>
        <table class="section-table">
          <tr>
            <td class="name">Meta</td>
            <td>
              <a href="/site/${site_id}/page/edit/${id}">edit</a>
            </td>
          </tr>
          <tr>
            <td class="name">Content</td>
            <td class="textarea-readonly">
              <textarea class='body-textarea'><body-html/></textarea>
            </td>
          </tr>
        </table>
      </div>
    </pages>

  </div> <!-- .section -->

</apply>
