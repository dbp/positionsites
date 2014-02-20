<apply template="base">

<h3 class="section-heading">Add a new Field</h3>
<hr/>

<div class="section">
  <div class="section-elem">
    <dfForm method="post">
      <table>
        <tr>
          <td class="name"><dfLabel ref="name">Name</dfLabel></td>
          <td><dfInputText ref="name" size="20" /></td>
        </tr>
        <dfIfChildErrors ref="name">
          <tr>
            <td></td><td><dfErrorList ref="name" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td class="name"><dfLabel ref="type">Type</dfLabel></td>
          <td class="textarea"><dfInputText ref="type" size="20" /></td>
        </tr>
        <dfIfChildErrors ref="type">
          <tr>
            <td></td><td><dfErrorList ref="type" /></td>
          </tr>
        </dfIfChildErrors>
        <tr>
          <td></td>
          <td><dfInputSubmit value="Add Field"/></td>
        </tr>
      </table>
    </dfForm>
  </div>
</div>

</apply>
