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

	private static function __keys(o) {
		return []; // TODO
	}

	private static function __unhtml(s : String) {
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
	}

	private static function __trace(v,i) {
		untyped {
			var msg = i.fileName+":"+i.lineNumber+": "+__unhtml(__string_rec(v,""))+"<br/>";
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

	private static function __string_rec(o,s) {
		return untyped __top__String(o); // TODO
	}

	private static function __instanceof(o,cl) {
		return false; // TODO
	}

	private static function __init() {
		untyped {
			Node.element_node = 1;
			Node.text_node = 3;
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
			Date.now = function() {
				return __new__(Date);
			};
			Date.prototype.toString = function() {
				var m = this.getMonth() + 1;
				var d = this.getDate();
				var h = this.getHours();
				var mi = this.getMinutes();
				var s = this.getSeconds();
				if( d < 10 )
					d = "0" + d;
				if( m < 10 )
					m = "0" + m;
				if( h < 10 )
					h = "0" + h;
				if( mi < 10 )
					mi = "0" + mi;
				if( s < 10 )
					s = "0" + s;
				return this.getFullYear()+"-"+m+"-"+d+" "+h+":"+mi+":"+s;
			};
		}
	}

}
