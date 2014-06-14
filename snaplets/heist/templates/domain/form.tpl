<div class="section">
  <div class="section-elem">
    <dfForm method="post">
      <table class="editor">
        <tr>
          <td class="url"><dfLabel ref="url">Domain</dfLabel></td>
          <td><dfInputText ref="url" size="20" /></td>
        </tr>
        <dfIfChildErrors ref="url">
          <tr>
            <td></td><td><dfErrorList ref="url" /></td>
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
