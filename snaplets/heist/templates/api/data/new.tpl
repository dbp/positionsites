<apply template="api_base">

<dfForm method="post">
  <table>
    <fields>
      <tr>
        <td><dfLabel ref="${field-ref}"><field-ref/></dfLabel></td>
        <td><dfInput type="${field-type}" ref="${field-ref}"/></td>
      </tr>
      <dfIfChildErrors ref="${field-ref}">
      <tr>
        <td></td><td><dfErrorList ref="${field-ref}" /></td>
      </tr>
    </dfIfChildErrors>
    </fields>
    <tr>
      <td></td>
      <td><dfInputSubmit value="Save"/></td>
    </tr>
  </table>
</dfForm>

</apply>
