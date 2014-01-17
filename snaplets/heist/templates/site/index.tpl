<apply template="base">
  <script>
    $(function () {
    $(".body-textarea").each(function (_, elem) {
    var editor = CodeMirror.fromTextArea(elem, {
      mode: "text/html",
      lineNumbers: true,
      readOnly: "nocursor"
    });
    });
    });
  </script>

  <h1>Site <domain/></h1>

  <h3>Data</h3>
  <a href="/site/${site_id}/data/new">New Data</a><br/>
  <hr/>
  <data>
    <name/>
    <p><fields/></p>
    <hr/>
  </data>


  <h3>Pages</h3>
  <a href="/site/${site_id}/page/new">New Page</a><br/>
  <hr/>
  <table id="pages">
    <tr><th>URL</th><th>Links</th><th>Content</th>
    <pages>
      <tr>
        <td><flat/></td>
        <td><a href="http://${domain}:8000${flat}">visit</a> |
          <a href="/site/${site_id}/page/edit/${id}">edit</a></td>
        <td class="textarea-readonly"><textarea class='body-textarea'><body-html/></textarea></td>
      </tr>
    </pages>
  </table>
</apply>
