<apply template="api_base">

  <h3>Set a new image</h3>

<dfForm method="post">
  <table>
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

</apply>
