<apply template="base">

  <h3 class="section-heading">Add a new Site</h3>
<hr/>

<div class="section">
  <div class="section-elem">
    <dfForm method="post">
      <table>
        <tr>
          <td><dfLabel ref="url">Domain</dfLabel></td>
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
  </div>
</div>

</apply>
