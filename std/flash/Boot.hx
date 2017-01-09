/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package flash;

#if !as3
@:keep private class RealBoot extends Boot implements Dynamic {
	#if swc
	public function new() {
		super();
	}
	public static function initSwc(mc) {
		flash.Lib.current = mc;
		new RealBoot().init();
	}
	#else
	function new() {
		super();
		if( flash.Lib.current == null ) flash.Lib.current = this;
		start();
	}
	#end
}
#end

@:dox(hide)
@:keep
class Boot extends flash.display.MovieClip {

	static var tf : flash.text.TextField;
	static var lines : Array<String>;
	static var lastError : flash.errors.Error;

	public static var skip_constructor = false;

	function start() {
		#if dontWaitStage
			init();
		#else
			var c = flash.Lib.current;
			try {
				untyped if( c == this && c.stage != null && c.stage.align == "" )
					c.stage.align = "TOP_LEFT";
			} catch( e : Dynamic ) {
				// security error when loading from different domain
			}
			if( c.stage == null )
				c.addEventListener(flash.events.Event.ADDED_TO_STAGE, doInitDelay);
			else if( c.stage.stageWidth == 0 || c.stage.stageHeight == 0 )
				untyped __global__["flash.utils.setTimeout"](start,1);
			else
				init();
		#end
	}

	function doInitDelay(_) {
		flash.Lib.current.removeEventListener(flash.events.Event.ADDED_TO_STAGE, doInitDelay);
		start();
	}

	#if (swc && swf_protected) public #end function init() {
		throw "assert";
	}

	static var IN_E = 0;
	public static function enum_to_string( e : { tag : String, params : Array<Dynamic> } ) {
		if( e.params == null )
			return e.tag;
		var pstr = [];
		if( IN_E > 15 ) {
			pstr.push("...");
		} else {
			IN_E++;
			for( p in e.params )
				pstr.push(__string_rec(p, ""));
			IN_E--;
		}
		return e.tag+"("+pstr.join(",")+")";
	}

	public static function __instanceof( v : Dynamic, t : Dynamic ) {
		try {
			if( t == Dynamic )
				return true;
			return untyped __is__(v,t);
		} catch( e : Dynamic ) {
		}
		return false;
	}

	public static function __clear_trace() {
		if( tf == null )
			return;
		tf.parent.removeChild(tf);
		tf = null;
		lines = null;
	}

	public static function __set_trace_color(rgb) {
		var tf = getTrace();
		tf.textColor = rgb;
		tf.filters = [];
	}

	public static function getTrace() {
		var mc = flash.Lib.current;
		if( tf == null ) {
			tf = new flash.text.TextField();
			#if flash10_2
			var color = 0xFFFFFF, glow = 0;
			if( mc.stage != null ) {
				glow = mc.stage.color;
				color = 0xFFFFFF - glow;
			}
			tf.textColor = color;
			tf.filters = [new flash.filters.GlowFilter(glow, 1, 2, 2, 20)];
			#end
			var format = tf.getTextFormat();
			format.font = "_sans";
			tf.defaultTextFormat = format;
			tf.selectable = false;
			tf.width = if( mc.stage == null ) 800 else mc.stage.stageWidth;
			tf.autoSize = flash.text.TextFieldAutoSize.LEFT;
			tf.mouseEnabled = false;
		}
		if( mc.stage == null )
			mc.addChild(tf);
		else
			mc.stage.addChild(tf); // on top
		return tf;
	}

	public static function __trace( v : Dynamic, pos : haxe.PosInfos ) {
		var tf = getTrace();
		var pstr = if( pos == null ) "(null)" else pos.fileName+":"+pos.lineNumber;
		if( lines == null ) lines = [];
		var str = pstr +": "+__string_rec(v, "");
		if( pos != null && pos.customParams != null )
			for( v in pos.customParams )
				str += ","+__string_rec(v, "");
		lines = lines.concat(str.split("\n"));
		tf.text = lines.join("\n");
		var stage = flash.Lib.current.stage;
		if( stage == null )
			return;
		while( lines.length > 1 && tf.height > stage.stageHeight ) {
			lines.shift();
			tf.text = lines.join("\n");
		}
	}

	public static function __string_rec( v : Dynamic, str : String ) {
		var cname = untyped __global__["flash.utils.getQualifiedClassName"](v);
		switch( cname ) {
		case "Object":
			var k : Array<String> = untyped __keys__(v);
			var s = "{";
			var first = true;
			for( i in 0...k.length ) {
				var key = k[i];
				if( key == "toString" )
					try return v.toString() catch( e : Dynamic ) {}
				if( first )
					first = false;
				else
					s += ",";
				s += " "+key+" : "+__string_rec(v[untyped key],str);
			}
			if( !first )
				s += " ";
			s += "}";
			return s;
		case "Array":
			if( v == Array )
				return "#Array";
			var s = "[";
			var i;
			var first = true;
			var a : Array<Dynamic> = v;
			for( i in 0...a.length ) {
				if( first )
					first = false;
				else
					s += ",";
				s += __string_rec(a[i],str);
			}
			return s + "]";
		default:
			switch( untyped __typeof__(v) ) {
			case "function": return "<function>";
			case "undefined": return "null";
			}
		}
		return new String(v);
	}

	static function __unprotect__( s : String ) {
		return s;
	}

	static public function mapDynamic(d:Dynamic, f:Dynamic) {
		if (Std.is(d, Array)) {
			return untyped d["mapHX"](f);
		} else {
			return untyped d["map"](f);
		}
	}

	static public function filterDynamic(d:Dynamic, f:Dynamic) {
		if (Std.is(d, Array)) {
			return untyped d["filterHX"](f);
		} else {
			return untyped d["filter"](f);
		}
	}

	static function __init__() untyped {
		var aproto = Array.prototype;
		aproto.copy = function() {
			return __this__.slice();
		};
		aproto.insert = function(i,x) {
			__this__.splice(i,0,x);
		};
		aproto.remove = function(obj) {
			var idx = __this__.indexOf(obj);
			if( idx == -1 ) return false;
			#if flash19
			__this__.removeAt(idx);
			#else
			__this__.splice(idx,1);
			#end
			return true;
		}
		aproto.iterator = function() {
			var cur = 0;
			var arr : Array<Dynamic> = __this__;
			return {
				hasNext : function() {
					return cur < arr.length;
				},
				next : function() {
					return arr[cur++];
				}
			}
		};
		aproto.setPropertyIsEnumerable("copy", false);
		aproto.setPropertyIsEnumerable("insert", false);
		aproto.setPropertyIsEnumerable("remove", false);
		aproto.setPropertyIsEnumerable("iterator", false);
		#if (as3 || no_flash_override)
		aproto.filterHX = function(f) {
			var ret = [];
			var i = 0;
			var l = __this__.length;
			while ( i < l ) {
				if (f(__this__[i]))
					ret.push(__this__[i]);
				i++;
			}
			return ret;
		};
		aproto.mapHX = function(f) {
			var ret = [];
			var i = 0;
			var l = __this__.length;
			while( i < l ) {
				ret.push(f(__this__[i]));
				i++;
			}
			return ret;
		};
		aproto.setPropertyIsEnumerable("mapHX", false);
		aproto.setPropertyIsEnumerable("filterHX", false);
		String.prototype.charCodeAtHX = function(i) : Null<Int> {
		#else
		aproto["filter"] = function(f) {
			var ret = [];
			var i = 0;
			var l = __this__.length;
			while ( i < l ) {
				if (f(__this__[i]))
					ret.push(__this__[i]);
				i++;
			}
			return ret;
		};
		aproto["map"] = function(f) {
			var ret = [];
			var i = 0;
			var l = __this__.length;
			while( i < l ) {
				ret.push(f(__this__[i]));
				i++;
			}
			return ret;
		};
		aproto.setPropertyIsEnumerable("map", false);
		aproto.setPropertyIsEnumerable("filter", false);
		String.prototype.charCodeAt = function(i) : Null<Int> {
		#end
			var s : String = __this__;
			var x : Float = s.cca(i);
			if( __global__["isNaN"](x) )
				return null;
			return Std.int(x);
		};
	}

}
