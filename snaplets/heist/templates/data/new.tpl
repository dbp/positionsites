<apply template="base">

<script>
  $(function () {
    var editor = CodeMirror.fromTextArea($("#fields-textarea")[0], {
      mode: "javascript"

    });
  });
</script>

<h3 class="section-heading">Add a new Data Type</h3>
<hr/>

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
          <td class="name"><dfLabel ref="fields">Fields</dfLabel></td>
          <td class="textarea"><dfInputTextArea ref="fields" rows="10" cols="40" id="fields-textarea" /></td>
        </tr>
        <dfIfChildErrors ref="fields">
          <tr>
            <td></td><td><dfErrorList ref="fields" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td></td>
          <td><dfInputSubmit value="Add Data Type"/></td>
        </tr>
      </table>
    </dfForm>
  </div>
</div>

</apply>
