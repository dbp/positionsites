<apply template="base">

<dfForm method="post">
  <table>
    <tr>
      <td><dfLabel ref="url">URL</dfLabel></td>
      <td><dfInputText ref="url" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="url">
      <tr>
        <td></td><td><dfErrorList ref="url" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><dfInputSubmit/></td>
    </tr>
  </table>
</dfForm>

</apply>
