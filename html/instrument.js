function instrument(message, nodes, links) {
    // console.info(message, "size_corr", size_corr(nodes));
    // console.info(message, "bounding_box", bounding_box(nodes));
    console.info(message, "compactness", compactness(nodes));
    // console.info(message, "crossings", crossing(nodes, links), "out of", links.length);
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
    var dx = (maxx - minx);
    var dy = (maxy - miny);
    // var d = Math.min(dx, dy);
    return [dx, dy];
}

function compactness(nodes) {
    var box = bounding_box(nodes);
    var area = 0;
    nodes.forEach(function(v) {
        area += Math.PI * v.size * v.size;
    });
    return area / box[0] / box[1];
}

function crossing(nodes, links) {
    var c = 0;
    for(var i = 0; i < links.length; i++) {
        for(var j = i+1; j < links.length; j++) {
            var e1 = links[i];
            var e2 = links[j];
            var u1 = e1.source, v1 = e1.target;
            var u2 = e2.source, v2 = e2.target;

            var inter = segment_intersection(u1.x, u1.y, v1.x, v1.y,   u2.x, u2.y, v2.x, v2.y);
            if(inter) {
                if(same(inter, u1) || same(inter, v1) || same(inter, u2) || same(inter, v2))
                    continue;
                else
                    c += 1;
            }
        }
    }

    return c;
}

var eps = 0.0000001;
function between(a, b, c) {
    return a-eps <= b && b <= c+eps;
}
function close(a, b) {
    return (a-eps <= b && b <= a+eps) || (b-eps <= a && a <= b+eps);
}
function same (u, v) {
    return close(u.x, v.x) && close(u.y, v.y);
}
function segment_intersection(x1,y1,x2,y2, x3,y3,x4,y4) {
    var x=((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4)) /
            ((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4));
    var y=((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4)) /
            ((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4));
    if (isNaN(x)||isNaN(y)) {
        return false;
    } else {
        if (x1>=x2) {
            if (!between(x2, x, x1)) {return false;}
        } else {
            if (!between(x1, x, x2)) {return false;}
        }
        if (y1>=y2) {
            if (!between(y2, y, y1)) {return false;}
        } else {
            if (!between(y1, y, y2)) {return false;}
        }
        if (x3>=x4) {
            if (!between(x4, x, x3)) {return false;}
        } else {
            if (!between(x3, x, x4)) {return false;}
        }
        if (y3>=y4) {
            if (!between(y4, y, y3)) {return false;}
        } else {
            if (!between(y3, y, y4)) {return false;}
        }
    }
    return {x: x, y: y};
}
