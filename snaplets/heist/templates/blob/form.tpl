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
          <td class="name"><dfLabel ref="admin">Admin Only</dfLabel></td>
          <td><dfInputCheckbox ref="admin"/></td>
        </tr>
        <dfIfChildErrors ref="admin">
          <tr>
            <td></td><td><dfErrorList ref="admin" /></td>
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
