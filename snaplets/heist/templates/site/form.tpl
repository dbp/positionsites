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
          <td><dfLabel ref="domain">Domain</dfLabel></td>
          <td><dfInputText ref="domain" size="20" /></td>
        </tr>
        <dfIfChildErrors ref="domain">
          <tr>
            <td></td><td><dfErrorList ref="domain" /></td>
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
  </div>
</div>
