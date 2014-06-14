<script>
  $(function () {
  var editor = CodeMirror.fromTextArea($("#base-textarea")[0], {
  mode: "text/html",
  lineNumbers: true,
  autoCloseTags: true,
  matchTags: {bothTags: true},
  extraKeys: {"Ctrl-J": "toMatchingTag"}
  });
  });
</script>


<div class="section">
  <div class="section-elem">
    <dfForm method="post">
      <table class="editor">
        <tr>
          <td class="name"><dfLabel ref="token">Analyze Token</dfLabel></td>
          <td><dfInputText ref="token" size="20" /></td>
        </tr>
        <dfIfChildErrors ref="token">
          <tr>
            <td></td><td><dfErrorList ref="token" /></td>
          </tr>
        </dfIfChildErrors>

        <tr>
          <td class="name"><dfLabel ref="base">Template</dfLabel></td>
          <td class="textarea"><dfInputTextArea ref="base" rows="40" id="base-textarea" /></td>
        </tr>
        <dfIfChildErrors ref="base">
          <tr>
            <td></td><td><dfErrorList ref="base" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td></td>
          <td><dfInputSubmit value="${submit-text}"/></td>
        </tr>
      </table>
    </dfForm>

    <table>
      <tr>
        <td class="name">Domains</td>
        <td>
          <ul>
            <li><a href="/site/${site_id}/domain/new">new</a></li>
            <domains>
              <li><url/> <a onclick="return confirm('Are you sure?');" href="/site/${site_id}/domain/${url_id}/delete">delete</a></li>
            </domains>
          </ul>
        </td>
      </tr>
    </table>

  </div>
</div>
