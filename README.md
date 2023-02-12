# oec
studies of the oxygen-evolving complex


## quick overview

### shiny app with XFELs S0-4 sequence structures

Warning: a cors-compliant web-server is needed here, currently just
available on my lap top.  let me know when this becomes a priority for
you.

<pre>
cd  ngl/myWidget/oec-ibrahim/
make run  (with nginx webserver already started)
</pre>


### three.js with pdb reader, OEC &amp; water can both be loaded

Still very simple molecular view, little changed from the three.js
demo set.   Load OEC from the menu in the top-right, the 
<pre>
cd explore/three/oec+water/
</pre>
need a simple http server (cors not needed):
<pre>
python3 -m http.server 8008
open http://localhost:8008/oecApp.html
</pre>

