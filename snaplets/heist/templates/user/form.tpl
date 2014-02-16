
<dfForm method="post">
  <table id="info">
    <tr><td colspan=2><message/></td></tr>
    <tr>
      <td><dfLabel ref="username">Username:</dfLabel></td>
      <td><dfInputText ref="username" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="username">
      <tr>
        <td></td><td><dfErrorList ref="username" /></td>
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
