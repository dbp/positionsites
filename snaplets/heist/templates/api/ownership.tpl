<apply template="api_base">

<dfForm method="post">
  <table>
    <tr>
      <td><dfLabel ref="owner">New Owner</dfLabel></td>
      <td><dfInputSelect ref="owner"/></td>
    </tr>
    <dfIfChildErrors ref="owner">
      <tr>
        <td></td><td><dfErrorList ref="owner" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><input type="submit"/></td>
    </tr>
  </table>
</dfForm>

</apply>
