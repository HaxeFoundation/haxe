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
package js;

class Boot {

	private static function __unhtml(s : String) {
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
	}

	private static function __trace(v,i : haxe.PosInfos) {
		untyped {
			var msg = if( i != null ) i.fileName+":"+i.lineNumber+": " else "";
			#if jsfl
			msg += __string_rec(v,"");
			fl.trace(msg);
			#else
			msg += __string_rec(v,"");
			var d;
			if( __js__("typeof")(document) != "undefined" && (d = document.getElementById("haxe:trace")) != null )
				d.innerHTML += __unhtml(msg)+"<br/>";
			else if( __js__("typeof")(console) != "undefined" && console.log != null )
				console.log(msg);
			#end
		}
	}

	private static function __clear_trace() {
		untyped {
			#if jsfl
			fl.outputPanel.clear();
			#else
			var d = document.getElementById("haxe:trace");
			if( d != null )
				d.innerHTML = "";
			#end
		}
	}

	static inline function isClass(o:Dynamic) : Bool {
		return untyped __define_feature__("js.Boot.isClass", o.__name__);
	}

	static inline function isEnum(e:Dynamic) : Bool {
		return untyped __define_feature__("js.Boot.isEnum", e.__ename__);
	}

	static inline function getClass(o:Dynamic) : Dynamic {
		return untyped __define_feature__("js.Boot.getClass", o.__class__);
	}

	@:ifFeature("has_enum")
	private static function __string_rec(o,s:String) {
		untyped {
			if( o == null )
			    return "null";
			if( s.length >= 5 )
				return "<...>"; // too much deep recursion
			var t = __js__("typeof(o)");
			if( t == "function" && (isClass(o) || isEnum(o)) )
				t = "object";
			switch( t ) {
			case "object":
				if( __js__("o instanceof Array") ) {
					if( o.__enum__ ) {
						if( o.length == 2 )
							return o[0];
						var str = o[0]+"(";
						s += "\t";
						for( i in 2...o.length ) {
							if( i != 2 )
								str += "," + __string_rec(o[i],s);
							else
								str += __string_rec(o[i],s);
						}
						return str + ")";
					}
					var l = o.length;
					var i;
					var str = "[";
					s += "\t";
					for( i in 0...l )
						str += (if (i > 0) "," else "")+__string_rec(o[i],s);
					str += "]";
					return str;
				}
				var tostr;
				try {
					tostr = untyped o.toString;
				} catch( e : Dynamic ) {
					// strange error on IE
					return "???";
				}
				if( tostr != null && tostr != __js__("Object.toString") ) {
					var s2 = o.toString();
					if( s2 != "[object Object]")
						return s2;
				}
				var k : String = null;
				var str = "{\n";
				s += "\t";
				var hasp = (o.hasOwnProperty != null);
				__js__("for( var k in o ) { ");
					if( hasp && !o.hasOwnProperty(k) )
						__js__("continue");
					if( k == "prototype" || k == "__class__" || k == "__super__" || k == "__interfaces__" || k == "__properties__" )
						__js__("continue");
					if( str.length != 2 )
						str += ", \n";
					str += s + k + " : "+__string_rec(o[k],s);
				__js__("}");
				s = s.substring(1);
				str += "\n" + s + "}";
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

	private static function __interfLoop(cc : Dynamic,cl : Dynamic) {
		if( cc == null )
			return false;
		if( cc == cl )
			return true;
		var intf : Dynamic = cc.__interfaces__;
		if( intf != null )
			for( i in 0...intf.length ) {
				var i : Dynamic = intf[i];
				if( i == cl || __interfLoop(i,cl) )
					return true;
			}
		return __interfLoop(cc.__super__,cl);
	}

	@:ifFeature("typed_catch") private static function __instanceof(o : Dynamic,cl) {
		untyped {
			try {
				if( __js__("o instanceof cl") ) {
					if( cl == Array )
						return (o.__enum__ == null);
					return true;
				}
				if( __interfLoop(js.Boot.getClass(o),cl) )
					return true;
			} catch( e : Dynamic ) {
				if( cl == null )
					return false;
			}
			switch( cl ) {
			case Int:
				return __js__("Math.ceil(o%2147483648.0) === o");
			case Float:
				return __js__("typeof(o)") == "number";
			case Bool:
				return __js__("o === true || o === false");
			case String:
				return __js__("typeof(o)") == "string";
			case Dynamic:
				return true;
			default:
				if( o == null )
					return false;
				// do not use isClass/isEnum here
				__feature__("Class.*",if( cl == Class && o.__name__ != null ) return true);
				__feature__("Enum.*",if( cl == Enum && o.__ename__ != null ) return true);
				return o.__enum__ == cl;
			}
		}
	}

	@:ifFeature("typed_cast") private static function __cast(o : Dynamic, t : Dynamic) {
		if (__instanceof(o, t)) return o;
		else throw "Cannot cast " +Std.string(o) + " to " +Std.string(t);
	}

}
