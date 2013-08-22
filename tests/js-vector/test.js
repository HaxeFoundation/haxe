(function () { "use strict";
var $estr = function() { return js.Boot.__string_rec(this,''); };
var HxOverrides = function() { }
HxOverrides.cca = function(s,index) {
	var x = s.charCodeAt(index);
	if(x != x) return undefined;
	return x;
}
var Std = function() { }
Std.random = function(x) {
	if(x <= 0) return 0; else return Math.floor(Math.random() * x);
}
var Test = function() { }
Test.main = function() {
	var v = new Float64Array(10);
	var _g = 0;
	while(_g < 10) {
		var i = _g++;
		var val = Math.sqrt(i);
		v[i] = val;
	}
	console.log(v);
	var b = haxe.io.Bytes.ofString("Hello, world!");
	console.log(b.toString());
	var ov = new Int32Array(20);
	var _g = 0;
	while(_g < 20) {
		var i = _g++;
		ov[i] = i;
	}
	console.log(ov);
	var sv;
	var _g = [];
	var _g1 = 0;
	while(_g1 < 5) {
		var i = _g1++;
		_g.push(null);
	}
	sv = _g;
	var _g1 = 0;
	var _g = sv.length;
	while(_g1 < _g) {
		var i = _g1++;
		var val = { name : "???", value : Std.random(10)};
		sv[i] = val;
	}
	console.log(sv);
}
var haxe = {}
haxe.io = {}
haxe.io.Bytes = function(length,b) {
	this.length = length;
	this.b = b;
};
haxe.io.Bytes.ofString = function(s) {
	var nbytes = 0;
	var _g1 = 0;
	var _g = s.length;
	while(_g1 < _g) {
		var i = _g1++;
		var c = s.charCodeAt(i);
		nbytes += c <= 127?1:c <= 2047?2:c <= 65535?3:4;
	}
	var a = new Uint8Array(nbytes);
	var ci = 0;
	var _g1 = 0;
	var _g = s.length;
	while(_g1 < _g) {
		var i = _g1++;
		var c = s.charCodeAt(i);
		if(c <= 127) a[ci++] = c; else if(c <= 2047) {
			a[ci++] = 192 | c >> 6;
			a[ci++] = 128 | c & 63;
		} else if(c <= 65535) {
			a[ci++] = 224 | c >> 12;
			a[ci++] = 128 | c >> 6 & 63;
			a[ci++] = 128 | c & 63;
		} else {
			a[ci++] = 240 | c >> 18;
			a[ci++] = 128 | c >> 12 & 63;
			a[ci++] = 128 | c >> 6 & 63;
			a[ci++] = 128 | c & 63;
		}
	}
	return new haxe.io.Bytes(a.length,a);
}
haxe.io.Bytes.prototype = {
	toString: function() {
		return this.readString(0,this.length);
	}
	,readString: function(pos,len) {
		if(pos < 0 || len < 0 || pos + len > this.length) throw haxe.io.Error.OutsideBounds;
		var s = "";
		var b = this.b;
		var fcc = String.fromCharCode;
		var i = pos;
		var max = pos + len;
		while(i < max) {
			var c = b[i++];
			if(c < 128) {
				if(c == 0) break;
				s += fcc(c);
			} else if(c < 224) s += fcc((c & 63) << 6 | b[i++] & 127); else if(c < 240) {
				var c2 = b[i++];
				s += fcc((c & 31) << 12 | (c2 & 127) << 6 | b[i++] & 127);
			} else {
				var c2 = b[i++];
				var c3 = b[i++];
				s += fcc((c & 15) << 18 | (c2 & 127) << 12 | c3 << 6 & 127 | b[i++] & 127);
			}
		}
		return s;
	}
}
haxe.io.Error = { __constructs__ : ["Blocked","Overflow","OutsideBounds","Custom"] }
haxe.io.Error.Blocked = ["Blocked",0];
haxe.io.Error.Blocked.toString = $estr;
haxe.io.Error.Blocked.__enum__ = haxe.io.Error;
haxe.io.Error.Overflow = ["Overflow",1];
haxe.io.Error.Overflow.toString = $estr;
haxe.io.Error.Overflow.__enum__ = haxe.io.Error;
haxe.io.Error.OutsideBounds = ["OutsideBounds",2];
haxe.io.Error.OutsideBounds.toString = $estr;
haxe.io.Error.OutsideBounds.__enum__ = haxe.io.Error;
haxe.io.Error.Custom = function(e) { var $x = ["Custom",3,e]; $x.__enum__ = haxe.io.Error; $x.toString = $estr; return $x; }
Math.__name__ = ["Math"];
Math.NaN = Number.NaN;
Math.NEGATIVE_INFINITY = Number.NEGATIVE_INFINITY;
Math.POSITIVE_INFINITY = Number.POSITIVE_INFINITY;
Math.isFinite = function(i) {
	return isFinite(i);
};
Math.isNaN = function(i) {
	return isNaN(i);
};
Test.main();
})();
