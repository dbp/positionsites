<apply template="base">

  <h3 class="section-heading">Edit User</h3>
  <hr/>

  <bind tag="submit-text">Save</bind>
  <apply template="form"></apply>

  <hr/>

  <site>
    <a onclick="return confirm('Are you sure?')" href="/site/${id}/user/delete/${user-id}">Remove from <domain/></a>
    <br/>
    <a onclick="return confirm('Are you sure?')" href="/site/${id}/user/delete/${user-id}?permanent">Remove from all of Position Sites</a>
  </site>

</apply>
