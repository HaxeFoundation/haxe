/*
 * Copyright (C)2005-2012 Haxe Foundation
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

@:keep
class Boot {

	private static var def_color = 0;
	private static var exception = null;

	private static function __string_rec(o : Dynamic,s : String) {
		untyped {
			if( s.length >= 20 )
				return "<...>"; // too much deep recursion
			var t = __typeof__(o);
			if( t == "movieclip" )
				t = "object";
			else if( t == "function" && (o.__name__ != null || o.__ename__ != null) )
				t = "object";
			switch( t ) {
			case "object":
				if( __instanceof__(o,Array) ) {
					if( o.__enum__ != null ) {
						if( o["length"] == 2 )
							return o[0];
						var str = o[0]+"(";
						s += "    ";
						for( i in 2...o["length"] ) {
							if( i != 2 )
								str += "," + __string_rec(o[i],s);
							else
								str += __string_rec(o[i],s);
						}
						return str + ")";
					}
					var l = o["length"];
					var i;
					var str = "[";
					s += "    ";
					for( i in 0...l )
						str += (if (i > 0) "," else "")+__string_rec(o[i],s);
					str += "]";
					return str;
				}
				var s2 = o["toString"]();
				if( (__typeof__(s2) == "string" || __instanceof__(s2,String)) && s2 != "[object Object]" && s2 != "[type Function]" )
					return s2;
				var k;
				var str = "{\n";
				if( typeof(o) == "movieclip" )
					str = "MC("+o._name+") "+str;
				s += "    ";
				var keys : Array<String> = __keys__(o);
				for( k in keys.iterator() ) {
					if( k == "prototype" || k == "__class__" || k == "__super__" || k == "__interfaces__" )
						continue;
					if( str.length != 2 )
						str += ",\n";
					str += s + k + " : "+__string_rec(o[k],s);
				}
				s = s.substring(4);
				if( str.length != 2 )
					str += "\n";
				str += s + "}";
				return str;
			case "function":
				return "<function>";
			case "string":
				return o;
			default:
				return String(o);
			}
		}
	}

	private static function __closure(f,o) {
		untyped {
			var m = o[f];
			if( m == null )
				return null;
			var f2 = function() {
				var me = __arguments__["callee"];
				return me["f"]["apply"](me["o"],__arguments__);
			};
			f2["f"] = m;
			f2["o"] = o;
			return f2;
		}
	}

	#if flash6
	private static function __interfLoop(cc : Dynamic,cl : Dynamic) {
		if( cc == null )
			return false;
		var intf : Array<Dynamic> = cc.__interfaces__;
		for( i in 0...intf.length ) {
			var i = intf[i];
			if( i == cl || __interfLoop(i,cl) )
				return true;
		}
		return __interfLoop(cc.__super__,cl);
	}
	#end

	private static function __instanceof(o : Dynamic,cl) {
		untyped {
			if( !cl )
				return false;
			if( __instanceof__(o,cl) ) {
				if( cl == Array )
					return ( o[__unprotect__("__enum__")] == null );
				return true;
			}
			#if flash6
			if( __interfLoop(o[__unprotect__("__class__")],cl) )
				return true;
			#end
			switch( cast cl ) {
			case Int:
				return __typeof__(o) == "number" && __physeq__(Math.ceil(o),o%2147483648.0) && !(__physeq__(o,true) || __physeq__(o,false));
			case Float:
				return __typeof__(o) == "number";
			case Bool:
				return __physeq__(o,true) || __physeq__(o,false);
			case String:
				return __typeof__(o) == "string";
			case Dynamic:
				return true;
			default:
				return o[__unprotect__("__enum__")] == cl ||
					(cl == Class && o[__unprotect__("__name__")] != null) ||
					(cl == Enum && o[__unprotect__("__ename__")] != null);
			}
		}
	}

	private static function getTrace() : flash.TextField untyped {
		var root = flash.Lib.current;
		var tf : flash.TextField = root.__trace_txt;
		if( tf == null ) {
			var w = Stage.width, h = Stage.height;
			if( w == 0 ) w = 800;
			if( h == 0 ) h = 600;
			root.createTextField("__trace_txt",1048500,0,0,w,h+30);
			tf = root.__trace_txt;
			var format = tf.getTextFormat();
			format.font = "_sans";
			tf.setNewTextFormat(format);
			tf.selectable = false;
			tf.textColor = def_color;
			root.__trace_lines = new Array<String>();
		}
		return tf;
	}

	private static function __set_trace_color( rgb : Int ) {
		getTrace().textColor = rgb;
		def_color = rgb;
	}

	private static function __trace(v,inf : haxe.PosInfos) {
		untyped {
			var root = flash.Lib.current;
			var tf = getTrace();
			var s = inf.fileName+(if( inf.lineNumber == null ) "" else ":"+inf.lineNumber)+": "+__string_rec(v, "");
			if( inf != null && inf.customParams != null )
				for( v in inf.customParams )
					s += "," + __string_rec(v, "");
			var lines : Array<String> = root.__trace_lines["concat"](s.split("\n"));
			tf.text = lines.join("\n");
			while( lines.length > 1 && tf.textHeight > Stage.height ) {
				lines.shift();
				tf.text = lines.join("\n");
			}
			root.__trace_lines = lines;
		}
	}

	static function __exc(v) {
		var s = "";
		#if debug
		var a : Array<String> = untyped __eval__("$s");
		for( i in 0...a.length-1 )
			s += "\nCalled from "+a[i];
		var old = a.slice(0,a.length-1);
		a.splice(0,a.length);
		#end
		if( untyped Lib.onerror != null )
			untyped Lib.onerror(__string_rec(v,""),#if debug old #else [] #end);
		else
			__trace(__string_rec(v,"")+s,cast { fileName : "(uncaught exception)" });
	}

	private static function __clear_trace() {
		untyped {
			var root = flash.Lib.current;
			root.__trace_txt["removeTextField"]();
			root.__trace_lines = null;
		}
	}

	private static function __init(current : Dynamic) untyped {
		// only if not set yet
		var g : Dynamic = _global;
		if( !g.haxeInitDone ) {
			g.haxeInitDone = true;
			Array.prototype["copy"] = Array.prototype["slice"];
			Array.prototype["insert"] = function(i,x) {
				__this__["splice"](i,0,x);
			};
			Array.prototype["remove"] = function(obj) {
				var i = 0;
				var l = __this__["length"];
				while( i < l ) {
					if( __this__[i] == obj ) {
						__this__["splice"](i,1);
						return true;
					}
					i++;
				}
				return false;
			}
			Array.prototype["iterator"] = function() {
				return {
					cur : 0,
					arr : __this__,
					hasNext : function() {
						return __this__.cur < __this__.arr["length"];
					},
					next : function() {
						return __this__.arr[__this__.cur++];
					}
				}
			};
			Array.prototype["map"] = function(f) {
				var ret = [];
				var i = 0;
				var l = __this__["length"];
				while( i < l ) {
					ret.push(f(__this__[i]));
					i++;
				}
				return ret;
			};
			Array.prototype["filter"] = function(f) {
				var ret = [];
				var i = 0;
				var l = __this__["length"];
				while ( i < l ) {
					if (f(__this__[i]))
						ret.push(__this__[i]);
					i++;
				}
				return ret;
			};
			_global["ASSetPropFlags"](Array.prototype,null,7);
			var cca = String.prototype["charCodeAt"];
			String.prototype["cca"] = cca;
			String.prototype["charCodeAt"] = function(i) {
				var x = __this__["cca"](i);
				if( x <= 0 ) // fast NaN
					return null;
				return x;
			};
			// create flash package (in for FP7 mark support)
			if( _global["flash"] == null )
				_global["flash"] = {};
		}
		// set the Lib variables
		current.flash.Lib._global = _global;
		current.flash.Lib._root = _root;
		current.flash.Lib.current = current;
		// prevent closure creation by setting untyped
		current[__unprotect__("@instanceof")] = flash.Boot[__unprotect__("__instanceof")];
		current[__unprotect__("@closure")] = flash.Boot[__unprotect__("__closure")];
		// fix firefox default alignement
		if( _global["Stage"]["align"] == "" )
			_global["Stage"]["align"] = "LT";
		#if mt mt.flash.Init.check(); #end
	}

}
