<apply template="base">
  <script>
    $(function () {


    $(".body-textarea").each(function (_, elem) {
    var editor = CodeMirror.fromTextArea(elem, {
      mode: "text/html",
      lineNumbers: true,
      readOnly: "nocursor",
      viewportMargin: Infinity
    });
    });
    $(".fields-textarea").each(function (_, elem) {
    var editor = CodeMirror.fromTextArea(elem, {
      mode: "javascript",
      readOnly: "nocursor",
      viewportMargin: Infinity
    });
    });
    });
  </script>

  <h1 class="title">Dashboard for domain <domains><url/>;</domains>.</h1>

  <h3 class="section-heading">Domains and Template (<a href="/site/${site_id}/edit">edit</a>)</h3>

  <h3 class="section-heading">Users (<a href="/site/${site_id}/user/new">add</a>)</h3>
  <div class="section">
    <div class="section-elem">
      <table class="section-table">
        <users>
          <tr>
            <td class="name">Username</td>
            <td><login/><is-admin> (admin)</is-admin></td>
            <td>
              <a href="/site/${site_id}/user/edit/${id}">edit</a>
            </td>
          </tr>
        </users>
      </table>
    </div> <!-- .section-elem -->
  </div> <!-- .section -->


  <h3 class="section-heading">Headers (<a href="/site/${site_id}/header/new">add</a>)</h3>

  <div class="section">
    <div class="section-elem">
      <table class="section-table">
        <headers>
          <tr>
            <td class="name">Name</td>
            <td><name/> (<is-css>CSS</is-css><is-js>Javascript</is-js>)</td>
            <td><a href="/site/${site_id}/header/edit/${id}">edit</a></td>
            <td><a onclick="return confirm('Are you sure you want to delete this?');"
                   href="/site/${site_id}/header/delete/${id}">delete</a></td>
          </tr>
        </headers>
      </table>
    </div> <!-- .section-elem -->
  </div> <!-- .section -->


  <h3 class="section-heading">Files (<a href="/site/${site_id}/file/new">add</a>)</h3>

  <div class="section">
    <div class="section-elem">
      <table class="section-table">
        <files>
          <tr>
            <td class="name">Name</td>
            <td><name/></td>
            <td><a onclick="return confirm('Are you sure you want to delete this?');"
                   href="/site/${site_id}/file/delete/${id}">delete</a></td>
          </tr>
        </files>
      </table>
    </div> <!-- .section-elem -->
  </div> <!-- .section -->


  <h3 class="section-heading">Data Types (<a href="/site/${site_id}/data/new">add</a>)</h3>
  <div class="section">
    <data>
      <div class="section-elem">
        <div class="section-name"><name/></div>
        <table class="section-table">
          <tr>
            <td class="name">Definition</td>
            <td><textarea class='fields-textarea'><fields/></textarea><a href="/site/${site_id}/data/${id}/add">+field</a></td>
          </tr>
          <tr>
            <td class="name">Items</td>
            <td><item-count/></td>
          </tr>
        </table>
      </div> <!-- .section-elem -->
    </data>
  </div> <!-- .section -->


  <h3 class="section-heading">Pages (<a href="/site/${site_id}/page/new">add</a>)</h3>
  <div class="section">
    <pages>
      <div class="section-elem">
        <div class="section-name">
          <a target="_blank" href="http://${domain}${server-port}${flat}">
            <structured/>
          </a>
        </div>
        <a href="/site/${site_id}/page/edit/${id}">edit</a>
      </div>
    </pages>
  </div> <!-- .section -->


  <h3 class="section-heading">Blobs (<a href="/site/${site_id}/blob/new">add</a>)</h3>
  <div class="section">
    <div class="section-elem">
      <table class="section-table">
        <blobs>
          <tr>
            <td class="name">Name</td>
            <td><name/> (
              <is-plain>Plain</is-plain>
              <is-html>HTML</is-html>
              <is-markdown>Markdown</is-markdown>
              )</td>
            <td><is-admin-only>Admin Only</is-admin-only></td>
            <td><a href="/site/${site_id}/blob/edit/${id}">edit</a></td>
          </tr>
        </blobs>
      </table>
    </div> <!-- .section-elem -->
  </div> <!-- .section -->

</apply>
