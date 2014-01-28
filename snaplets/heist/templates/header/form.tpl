<script>
  $(function () {
    var editor = CodeMirror.fromTextArea($("#content-textarea")[0], {
      mode: "css",
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
          <td class="name"><dfLabel ref="name">Name</dfLabel></td>
          <td><dfInputText ref="name" size="20" /></td>
        </tr>
        <dfIfChildErrors ref="name">
          <tr>
            <td></td><td><dfErrorList ref="name" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td class="name"><dfLabel ref="type">Type</dfLabel></td>
          <td><dfInputSelect ref="type"/></td>
        </tr>
        <dfIfChildErrors ref="type">
          <tr>
            <td></td><td><dfErrorList ref="type" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td class="name"><dfLabel ref="content">Content</dfLabel></td>
          <td class="textarea"><dfInputTextArea ref="content" rows="40" id="content-textarea" /></td>
        </tr>
        <dfIfChildErrors ref="content">
          <tr>
            <td></td><td><dfErrorList ref="content" /></td>
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
