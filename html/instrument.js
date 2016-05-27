function instrument(message, nodes, links) {
    var r = size_corr(nodes);
    var b = bounding_box(nodes);
    console.info(message, "size_corr", r);
    console.info(message, "bounding_box", b);
}

// http://mathbits.com/MathBits/TISection/Statistics2/correlation.htm
function size_corr(nodes) {
    var n = nodes.length;
    var x = nodes.map(function(n) {return n.rawsize;});
    var y = nodes.map(function(n) {return n.size;});
    var rxy = 0; // \sum xy
    var rx = 0; // \sum x
    var ry = 0; // \sum y
    var rxx = 0; // \sum x*x
    var ryy = 0; // \sum y*y
    for(var i=0; i < n; i++) {
        rxy += x[i] * y[i];
        rx += x[i];
        ry += y[i];
        rxx += x[i] * x[i];
        ryy += y[i] * y[i];
    }
    var r = rxy / Math.sqrt(rxx * ryy);
    return r;
}

function bounding_box(nodes) {
    var minx, miny, maxx, maxy;

    for(var i=0; i < nodes.length; i++) {
        var n = nodes[i];
        var x0 = n.x - n.size;
        var y0 = n.y - n.size;
        var x1 = n.x + n.size;
        var y1 = n.y + n.size;

        if(minx == null || x0 < minx) minx = x0;
        if(miny == null || y0 < miny) miny = y0;
        if(maxx == null || x1 > maxx) maxx = x1;
        if(maxy == null || y1 > maxy) maxy = y1;
    }
    return (maxx - minx) * (maxy - miny);
}
