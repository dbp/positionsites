<apply template="base">

<dfForm method="post">
  <table>
    <tr>
      <td><dfLabel ref="flat">Flat URL</dfLabel></td>
      <td><dfInputText ref="flat" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="flat">
      <tr>
        <td></td><td><dfErrorList ref="flat" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td><dfLabel ref="structured">Structured URL</dfLabel></td>
      <td><dfInputText ref="structured" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="structured">
      <tr>
        <td></td><td><dfErrorList ref="structured" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td><dfLabel ref="body">Body</dfLabel></td>
      <td><dfInputTextArea ref="body" rows="10" cols="40" /></td>
    </tr>
    <dfIfChildErrors ref="body">
      <tr>
        <td></td><td><dfErrorList ref="body" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><dfInputSubmit/></td>
    </tr>
  </table>
</dfForm>

</apply>
