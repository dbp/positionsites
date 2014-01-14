<apply template="base">

<dfForm method="post">
  <table>
    <tr>
      <td><dfLabel ref="name">Name</dfLabel></td>
      <td><dfInputText ref="name" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="name">
      <tr>
        <td></td><td><dfErrorList ref="name" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td><dfLabel ref="fields">Fields</dfLabel></td>
      <td><dfInputTextArea ref="fields" rows="10" cols="40" /></td>
    </tr>
    <dfIfChildErrors ref="fields">
      <tr>
        <td></td><td><dfErrorList ref="fields" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><dfInputSubmit/></td>
    </tr>
  </table>
</dfForm>

</apply>
