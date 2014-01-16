<apply template="base">

<dfForm method="post">
  <table>
    <tr><td><field-ref/></td><td><dfInput type="${field-type}" ref="${field-ref}"/></td></tr>
    <dfIfChildErrors ref="${field-ref}">
      <tr>
        <td></td><td><dfErrorList ref="${field-ref}" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><input type="submit"/></td>
    </tr>
  </table>
</dfForm>

</apply>
