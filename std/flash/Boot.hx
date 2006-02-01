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

	private static function __string_rec(o,s) {
		untyped {
			if( s.length >= 20 )
				return "<...>"; // too much deep recursion
			var t = __typeof__(o);
			if( t == "movieclip" )
				t = "object";
			else if( t == "function" && o.__interfaces__ != null )
				t = "object";
			switch( t ) {
			case "object":
				if( __instanceof__(o,Array) ) {
					var l = o.length;
					var i;
					var str = "[";
					s += "    ";
					for i in 0...l
						str += (if (i > 0) "," else "")+__string_rec(o[i],s);
					s = s.substring(4);
					str += "]";
					return str;
				}
				var s2 = o.toString();
				if( (__typeof__(s2) == "string" || __instanceof__(s2,String)) && s2 != "[object Object]" && s2 != "[type Function]" )
					return s2;
				var k;
				var str = "{\n";
				if( typeof(o) == "movieclip" )
					str = "MC("+o._name+") "+str;
				s += "    ";
				for k in (__keys__(o)).iterator() {
					if( k == "__construct__" && __typeof__(o[k]) == "function" )
						str += s + k + " : <function>\n";
					else
						str += s + k + " : "+__string_rec(o[k],s)+"\n";
				}
				s = s.substring(4);
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
			var f2 = function() {
				var me = __arguments__.callee;
				return me.f.apply(me.o,__arguments__);
			};
			f2.f = o[f];
			f2.o = o;
			return f2;
		}
	}

	private static function __instanceof(o,cl) {
		untyped {
			if( __instanceof__(o,cl) )
				return true;
			switch( cl ) {
			case Int:
				return Math.ceil(o) == o; // error with NaN
			case Float:
				return __typeof__(o) == "number";
			case Bool:
				return (o == true || o == false);
			case String:
				return __typeof__(o) == "string";
			default:
				return false;
			}
		}
	}

	private static function __trace(v,inf) {
		untyped {
			var root = flash.Lib.current;
			var tf = root.__trace_txt;
			if( tf == null ) {
				root.createTextField("__trace_txt",1048500,0,0,Stage.width,Stage.height);
				tf = root.__trace_txt;
				tf.selectable = false;
				root.__trace_lines = new Array<String>();
			}
			var s = inf.fileName+":"+inf.lineNumber+": "+__string_rec(v,"");
			var lines = root.__trace_lines.concat(s.split("\n"));
			root.__trace_lines = lines;
			var nlines = Stage.height / 16;
			if( lines.length > nlines )
				lines.splice(0,lines.length-nlines);
			tf.text = lines.join("\n");
		}
	}

	private static function __clear_trace() {
		untyped {
			_root.__trace_txt.removeTextField();
			_root.__trace_lines = null;
		}
	}

	private static function __init(current) {
		untyped {
			var obj = _global["Object"];
			var flash = current["flash"];
			if( flash.text == null )
				flash.text = __new__(obj);
			flash.text.StyleSheet = TextField["StyleSheet"];
			flash.system = __new__(obj);
			flash.system.Capabilities = System.capabilities;
			flash.system.Security = System.security;
			Math.pi = Math["PI"];

			#if use_ime
			flash.system.IME = System["IME"];
			flash.system.IME._ALPHANUMERIC_FULL = System["IME"]["ALPHANUMERIC_FULL"];
			flash.system.IME._ALPHANUMERIC_HALF = System["IME"]["ALPHANUMERIC_HALF"];
			flash.system.IME._CHINESE = System["IME"]["CHINESE"];
			flash.system.IME._JAPANESE_HIRAGANA = System["IME"]["JAPANESE_HIRAGANA"];
			flash.system.IME._JAPANESE_KATAKANA_FULL = System["IME"]["JAPANESE_KATAKANA_FULL"];
			flash.system.IME._JAPANESE_KATAKANA_HALF = System["IME"]["JAPANESE_KATAKANA_HALF"];
			flash.system.IME._KOREAN = System["IME"]["KOREAN"];
			flash.system.IME._UNKNOWN = System["IME"]["UNKNOWN"];
			#end

			Node = _global["XMLNode"];
			Node.element_node = 1;
			Node.text_node = 3;
			Node.prototype.removeChild = Node.prototype.removeNode;
			Node.prototype.replaceChild = function(cnew,cold) {
				this.insertBefore(cnew,cold);
				this.removeChild(cold);
			};
			Node.prototype.nodes = function() {
				return untyped {
					p : 0,
					a : childNodes,
					next : function() {
						while true {
							var x = this.a[this.p];
							if( x == null )
								return null;
							this.p++;
							if( x.nodeType == 1 )
								return x;
						}
						return null;
					},
					hasNext : function() {
						var x = this.next();
						if( x != null ) {
							this.p--;
							return true;
						}
						return false;
					}
				};
			};
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
					max : this.length,
					arr : this,
					hasNext : function() {
						return this.cur < this.max;
					},
					next : function() {
						return this.arr[this.cur++];
					}
				}
			};
			Array.prototype.indexes = function() {
				return {
					cur : 0,
					max : this.length,
					hasNext : function() {
						return this.cur < this.max;
					},
					next : function() {
						return this.cur++;
					}
				}
			};

			// copy base classes from root to flash package
			// we can't make a loop since we need to assign short-type-ids
			flash.Accessibility = _global["Accessibility"];
			flash.Camera = _global["Camera"];
			flash.Color = _global["Color"];
			flash.Key = _global["Key"];
			flash.LoadVars = _global["LoadVars"];
			flash.LocalConnection = _global["LocalConnection"];
			flash.Microphone = _global["Microphone"];
			flash.Mouse = _global["Mouse"];
			flash.MovieClip = _global["MovieClip"];
			flash.MovieClipLoader = _global["MovieClipLoader"];
			flash.PrintJob = _global["PrintJob"];
			flash.Selection = _global["Selection"];
			flash.SharedObject = _global["SharedObject"];
			flash.Sound = _global["Sound"];
			flash.Stage = _global["Stage"];
			flash.System = _global["System"];
			flash.TextField = _global["TextField"];
			flash.TextFormat = _global["TextFormat"];
			flash.TextSnapshot = _global["TextSnapshot"];
			flash.Video = _global["Video"];

			Lib._global = _global;
			Lib._root = _root;
			Lib.current = current;
			Int = __new__(obj);
			Float = _global["Number"];
			// prevent closure creation by setting untyped
			current["@instanceof"] = untyped __instanceof;
			current["@closure"] = untyped __closure;
		}
	}

}