<apply template="base">

  <h3 class="section-heading">Edit User</h3>
  <hr/>

  <bind tag="submit-text">Save</bind>


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
      <td><dfLabel ref="admin">Admin:</dfLabel></td>
      <td><dfInputCheckbox ref="admin" size="20" /></td>
    </tr>
    <dfIfChildErrors ref="admin">
      <tr>
        <td></td><td><dfErrorList ref="admin" /></td>
      </tr>
    </dfIfChildErrors>

    <tr>
      <td></td>
      <td><dfInputSubmit/></td>
    </tr>
  </table>
</dfForm>

  <hr/>

  <site>
    <a onclick="return confirm('Are you sure?')" href="/site/${id}/user/delete/${user-id}">Remove from <domain/></a>
    <br/>
    <a onclick="return confirm('Are you sure?')" href="/site/${id}/user/delete/${user-id}?permanent">Remove from all of Position Sites</a>
  </site>

</apply>
