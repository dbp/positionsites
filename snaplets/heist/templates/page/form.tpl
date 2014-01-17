<script>
  $(function () {
    var editor = CodeMirror.fromTextArea($("#body-textarea")[0], {
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
          <td class="name"><dfLabel ref="flat">Flat URL</dfLabel></td>
          <td><dfInputText ref="flat" size="20" /></td>
        </tr>
        <dfIfChildErrors ref="flat">
          <tr>
            <td></td><td><dfErrorList ref="flat" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td class="name"><dfLabel ref="structured">Structured URL</dfLabel></td>
          <td><dfInputText ref="structured" size="20" /></td>
        </tr>
        <dfIfChildErrors ref="structured">
          <tr>
            <td></td><td><dfErrorList ref="structured" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td class="name"><dfLabel ref="body">Body</dfLabel></td>
          <td class="textarea"><dfInputTextArea ref="body" rows="40" id="body-textarea" /></td>
        </tr>
        <dfIfChildErrors ref="body">
          <tr>
            <td></td><td><dfErrorList ref="body" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td></td>
          <td><dfInputSubmit value="${submit-text}"/></td>
        </tr>
      </table>
    </dfForm>
  </div>
</div>
