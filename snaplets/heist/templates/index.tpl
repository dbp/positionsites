<apply template="base">

  <h1 class="title">Position Sites</h1>

  <ifLoggedIn>
    <h3 class="section-heading">Sites (<a href="/new">add</a>)</h3>

    <div class="section">
      <sites>
        <div class="section-elem">
          <div class="section-name"><a href="/site/${id}"><domains><url/>;</domains></a></div>
        </div>
      </sites>
    </div>
  </ifLoggedIn>

  <ifLoggedOut>
    <p>A data-driven website platform designed to make it easy for developers to build completely customized websites.</p>

    <p>If you've gotten here from a site built on this platform, feel free to contact us at <a href="http://positionstudios.com">positionstudios.com</a> about building you an easily modifiable website.</p>

    <p>If you're curious about how it works, you can also get in touch, or check out the source code from the link in the footer. The design process is:</p>

    <ol>
      <li>Define the data that the site will be made up of.</li>
      <li>Write html for pages, using tags that render data, and special tags that allow data to be added or edited.</li>
      <li>Add users, css and javascript, and any static pieces of text that will be editable.</li>
    </ol>

    <p><a href="/login">Login</a></p>
  </ifLoggedOut>

</apply>
