
<dfForm method="post">
  <table id="info">
    <tr><td colspan=2><message/></td></tr>
    <tr>
      <td><dfLabel ref="email">Email:</dfLabel></td>
      <td><dfInputText ref="email" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="email">
      <tr>
        <td></td><td><dfErrorList ref="email" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td><dfLabel ref="password">Password:</dfLabel></td>
      <td><dfInputPassword ref="password" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="password">
      <tr>
        <td></td><td><dfErrorList ref="password" /></td>
      </tr>
    </dfIfChildErrors>
    <tr>
      <td></td>
      <td><dfInputSubmit/></td>
    </tr>
  </table>
</dfForm>
