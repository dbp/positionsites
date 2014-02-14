<h3>Set content for "<name/>"</h3>

<dfForm method="post">
  <table>
    <tr>
      <td><dfLabel ref="content">Content</dfLabel></td>
      <td><dfInputTextArea ref="content" rows="20" cols="50" /></td>
    </tr>
    <dfIfChildErrors ref="content">
      <tr>
        <td></td><td><dfErrorList ref="content" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><dfInputSubmit value="Update"/></td>
    </tr>
  </table>
</dfForm>
