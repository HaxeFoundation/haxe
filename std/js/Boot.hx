/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package js;

class Boot {

	private static function __unhtml(s : String) {
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
	}

	private static function __trace(v,i) {
		untyped {
			var msg = if( i != null ) i.fileName+":"+i.lineNumber+": " else "";
			msg += __unhtml(__string_rec(v,""))+"<br/>";

			var d = document.getElementById("haxe:trace");
			if( d == null )
				alert("No haxe:trace element defined\n"+msg);
			else
				d.innerHTML += msg;
		}
	}

	private static function __clear_trace() {
		untyped {
			var d = document.getElementById("haxe:trace");
			if( d != null )
				d.innerHTML = "";
		}
	}

	private static function __closure(o,f) {
		untyped {
			var m = o[f];
			if( m == null )
				return null;
			return function() { return m.apply(o,arguments); };
		}
	}

	private static function __string_rec(o,s) {
		untyped {
			if( o == null )
			    return "null";
			if( s.length >= 5 )
				return "<...>"; // too much deep recursion
			var t = __js__("typeof(o)");
			if( t == "function" && (o.__name__ != null || o.__ename__ != null) )
				t = "object";
			switch( t ) {
			case "object":
				if( __js__("o instanceof Array") ) {
					if( o.__enum__ != null ) {
						if( o.length == 1 )
							return o[0];
						var str = o[0]+"(";
						s += "    ";
						for( i in 1...o.length ) {
							if( i != 1 )
								str += "," + __string_rec(o[i],s);
							else
								str += __string_rec(o[i],s);
						}
						return str + ")";
					}
					var l = o.length;
					var i;
					var str = "[";
					s += "    ";
					for( i in 0...l )
						str += (if (i > 0) "," else "")+__string_rec(o[i],s);
					s = s.substring(4);
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
				var k : String;
				var str = "{\n";
				s += "\t";
				var hasp = (o.hasOwnProperty != null);
				__js__("for( var k in o ) { ");
					if( hasp && !o.hasOwnProperty(k) )
						__js__("continue");
					if( k == "prototype" || k == "__class__" || k == "__super__" || k == "__interfaces__" )
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

	private static function __instanceof(o,cl) {
		untyped {
			try {
				if( __js__("o instanceof cl") ) {
					if( cl == Array )
						return (o.__enum__ == null);
					return true;
				}
				if( __interfLoop(o.__class__,cl) )
					return true;
			} catch( e : Dynamic ) {
				if( cl == null )
					return false;
			}
			switch( cl ) {
			case Int:
				return (Math.ceil(o) === o) && isFinite(o);
			case Float:
				return __js__("typeof(o)") == "number";
			case Bool:
				return (o === true || o === false);
			case String:
				return __js__("typeof(o)") == "string";
			case Dynamic:
				return true;
			default:
				if( o != null && o.__enum__ == cl )
					return true;
				return false;
			}
		}
	}

	private static function __init() {
		untyped {
			Lib.isIE = (document.all != null && window.opera == null );
			Lib.isOpera = (window.opera != null );
			Array.prototype.copy = Array.prototype.slice;
			Array.prototype.insert = function(i,x) {
				this.splice(i,0,x);
			};
			Array.prototype.remove = function(obj) {
				var i = 0;
				var l = this.length;
				while( i < l ) {
					if( this[i] == obj ) {
						this.splice(i,1);
						return true;
					}
					i++;
				}
				return false;
			}
			Array.prototype.iterator = function() {
				return {
					cur : 0,
					arr : this,
					hasNext : function() {
						return this.cur < this.arr.length;
					},
					next : function() {
						return this.arr[this.cur++];
					}
				}
			};
			String.prototype.__class__ = String;
			String.__name__ = ["String"];
			Array.prototype.__class__ = Array;
			Array.__name__ = ["Array"];
			var cca = String.prototype.charCodeAt;
			String.prototype.charCodeAt = function(i) {
				var x = cca.call(this,i);
				if( isNaN(x) )
					return null;
				return x;
			};
			var oldsub = String.prototype.substr;
			String.prototype.substr = function(pos,len){
				if( pos != null && pos != 0 && len < 0 ) return "";
				if( pos < 0 ){
					pos = this.length + pos;
					if( pos < 0 ) pos = 0;
				}else if( len < 0 ){
					len = this.length + len - pos;
				}
				return oldsub.apply(this,[pos,len]);
			};
			Int = __new__("Object");
			Dynamic = __new__("Object");
			Float = __js__("Number");
			Bool = __new__("Object");
			Bool["true"] = true;
			Bool["false"] = false;
			__js__("$closure = js.Boot.__closure");
		}
	}

}
