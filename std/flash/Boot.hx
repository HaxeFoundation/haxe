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
package flash;

class Boot {

	private static var exc : Array<Dynamic>;

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
						if( o["length"] == 1 )
							return o[0];
						var str = o[0]+"(";
						s += "    ";
						for( i in 1...o["length"] ) {
							if( i != 1 )
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
				for( k in (__keys__(o))["iterator"]() ) {
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
		var intf = cc.__interfaces__;
		for( i in 0...intf.length ) {
			var i = intf[i];
			if( i == cl || __interfLoop(i,cl) )
				return true;
		}
		return __interfLoop(cc.__super__,cl);
	}
	#end

	private static function __instanceof(o,cl) {
		untyped {
			if( __instanceof__(o,cl) ) {
				if( cl == Array )
					return ( o[__unprotect__("__enum__")] == null );
				return true;
			}
			#if flash6
			if( __interfLoop(o[__unprotect__("__class__")],cl) )
				return true;
			#end
			switch( cl ) {
			case Int:
				return (Math.ceil(o) === o) && isFinite(o) && (o !== true) && (o !== false);
			case Float:
				return __typeof__(o) == "number";
			case Bool:
				return (o === true || o === false);
			case String:
				return __typeof__(o) == "string";
			case Dynamic:
				return true;
			default:
				if( o.__enum__ == cl )
					return true;
				return false;
			}
		}
	}

	private static function __trace(v,inf : haxe.PosInfos) {
		untyped {
			var root = flash.Lib.current;
			var tf : flash.TextField = root.__trace_txt;
			if( tf == null ) {
				root.createTextField("__trace_txt",1048500,0,0,Stage.width,Stage.height+30);
				tf = root.__trace_txt;
				tf.selectable = false;
				root.__trace_lines = new Array<String>();
			}
			var s = inf.fileName+(if( inf.lineNumber == null ) "" else ":"+inf.lineNumber)+": "+__string_rec(v,"");
			var lines : Array<String> = root.__trace_lines["concat"](s.split("\n"));
			tf.text = lines.join("\n");
			while( tf.textHeight > Stage.height ) {
				lines.shift();
				tf.text = lines.join("\n");
			}
			root.__trace_lines = lines;
		}
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
			var obj = _global["Object"];
			g.haxeInitDone = true;
			g.Int = __new__(obj);
			g.Bool = __new__(obj);
			g.Dynamic = __new__(obj);
			g.Bool = __new__(obj);
			g.Bool[__unprotect__("true")] = true;
			g.Bool[__unprotect__("false")] = false;
			g.Float = _global["Number"];
			Array.prototype["copy"] = Array.prototype["slice"];
			Array.prototype["insert"] = function(i,x) {
				this["splice"](i,0,x);
			};
			Array.prototype["remove"] = function(obj) {
				var i = 0;
				var l = this["length"];
				while( i < l ) {
					if( this[i] == obj ) {
						this["splice"](i,1);
						return true;
					}
					i++;
				}
				return false;
			}
			Array.prototype["iterator"] = function() {
				return {
					cur : 0,
					arr : this,
					hasNext : function() {
						return this.cur < this.arr["length"];
					},
					next : function() {
						return this.arr[this.cur++];
					}
				}
			};
			Array.prototype[__unprotect__("__class__")] = Array;
			Array[__unprotect__("__name__")] = ["Array"];
			String.prototype[__unprotect__("__class__")] = String;
			String[__unprotect__("__name__")] = ["String"];
			var cca = String.prototype["charCodeAt"];
			String.prototype["charCodeAt"] = function(i) {
				var x = cca["call"](this,i);
				if( x <= 0 ) // fast NaN
					return null;
				return x;
			};
			// create flash package (in for FP7 mark support)
			if( _global["flash"] == null )
				_global["flash"] = __new__(obj);
		}
		// create the array stack
		if( exc == null )
			exc = new Array();
		// set the Lib variables
		current.flash.Lib._global = _global;
		current.flash.Lib._root = _root;
		current.flash.Lib.current = current;
		// prevent closure creation by setting untyped
		current[__unprotect__("@instanceof")] = untyped __instanceof;
		current[__unprotect__("@closure")] = untyped __closure;
		current[__unprotect__("@exc")] = exc;
		// fix firefox default alignement
		if( _global["Stage"]["align"] == "" )
			_global["Stage"]["align"] = "LT";
	}

}
