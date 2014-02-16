<dfForm method="post">
  <table>
    <tr>
      <td><dfLabel ref="name">Name</dfLabel></td>
      <td><dfInputText ref="name"/></td>
    </tr>
    <dfIfChildErrors ref="name">
      <tr>
        <td></td><td><dfErrorList ref="name" /></td>
      </tr>
    </dfIfChildErrors>

    <tr>
      <td><dfLabel ref="file">File</dfLabel></td>
      <td><dfInputFile ref="file"/></td>
    </tr>
    <dfIfChildErrors ref="file">
      <tr>
        <td></td><td><dfErrorList ref="file" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><input type="submit"/></td>
    </tr>
  </table>
</dfForm>
