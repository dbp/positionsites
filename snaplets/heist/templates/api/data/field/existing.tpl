<apply template="api_base">

<dfForm method="post">
  <table>
    <tr>
      <td><dfLabel ref="item">Item</dfLabel></td>
      <td><dfInputSelect ref="item"/></td>
    </tr>
    <dfIfChildErrors ref="item">
      <tr>
        <td></td><td><dfErrorList ref="item" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><input type="submit"/></td>
    </tr>
  </table>
</dfForm>

</apply>
