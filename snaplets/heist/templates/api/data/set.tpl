<apply template="api_base">

  <script>
    $(function () {
      $(".ps-fields textarea").autosize();
    });
  </script>

  <dfForm method="post">
    <table class="ps-fields">
      <tr>
        <td class="ps-label"><dfLabel ref="${field-ref}"><field-ref/></dfLabel></td>
        <td><psInput type="${field-type}" ref="${field-ref}"/></td>
      </tr>
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
