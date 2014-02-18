<html>
  <head>
    <title>Position Sites</title>
    <script src="/static/jquery-2.0.3.min.js"></script>
    <link rel="stylesheet" href="/static/lib/codemirror.css" />
    <link rel="stylesheet" href="/static/management.css" />
    <script src="/static/lib/codemirror.js"></script>
    <script src="/static/addon/edit/closetag.js"></script>
    <script src="/static/addon/fold/xml-fold.js"></script>
    <script src="/static/addon/edit/matchtags.js"></script>
    <script src="/static/mode/xml/xml.js"></script>
    <script src="/static/mode/javascript/javascript.js"></script>
    <script src="/static/mode/css/css.js"></script>
    <script src="/static/mode/htmlmixed/htmlmixed.js"></script>

    <script type="text/javascript" src="//use.typekit.net/vwl6mum.js"></script>
    <script type="text/javascript">try{Typekit.load();}catch(e){}</script>
  </head>
  <body>
    <div id="content">

      <div id="wrap">

        <div id="auth">
          <ifLoggedIn><loggedInUser/> <a href="/logout">Logout</a></ifLoggedIn>
        </div>


        <apply-content/>


        <div id="footer">
          designed and built by <a href="http://positionstudios.com" target="_blank">Position Studios</a>. &copy; 2014. <a href="/docs">documentation</a>. it's all <a href="http://github.com/dbp/positionsites" target="_blank">open source</a>.
        </div>
      </div> <!-- #wrap -->
    </div>
  </body>
</html>
