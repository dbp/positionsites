<apply template="api_base">

  <script>
    $(function () {
      $(".ps-fields textarea").autosize();
    });
  </script>

  <h3>Add a new "<name/>"</h3>

<dfForm method="post">
  <table class="ps-fields">
    <fields>
      <tr>
        <td class="ps-label"><dfLabel ref="${field-ref}"><field-ref/></dfLabel></td>
        <td><psInput type="${field-type}" ref="${field-ref}"/></td>
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
