<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>Kernel density</title>
    <!--[if lte IE 8]><script language="javascript" type="text/javascript" src="http://people.iola.dk/olau/flot/excanvas.min.js"></script><![endif]-->
    <script language="javascript" type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>
    <script language="javascript" type="text/javascript" src="http://people.iola.dk/olau/flot/jquery.flot.js"></script>
 </head>
    <body>
    <h1>Kernel density</h1>

    <div id="placeholder" style="width:600px;height:450px;"></div>

    <p>This is a 64-point kernel density estimate
      of <a href="http://stat.ethz.ch/R-manual/R-patched/library/datasets/html/faithful.html">wait
        times between eruptions</a> of
      the <a href="http://en.wikipedia.org/wiki/Old_Faithful">Old
      Faithful</a> geyser.</p>

<script type="text/javascript">
$(function () {
    $.plot($("#placeholder"), [ {{data}} ]);
});
</script>

 </body>
</html>
