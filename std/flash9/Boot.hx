/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
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

class Boot extends flash.display.MovieClip, implements Dynamic {

	static var init : Void -> Void;
	static var tf : flash.text.TextField;
	static var lines : Array<String>;

	#if !as3gen
	static var __res : flash.utils.Dictionary;
	#end

	public static var skip_constructor = false;

	public function new(?mc:flash.display.MovieClip) {
		super();
		untyped {
			var aproto = Array.prototype;
			aproto.copy = function() {
				return this.slice();
			};
			aproto.insert = function(i,x) {
				this.splice(i,0,x);
			};
			aproto.remove = function(obj) {
				for( i in 0...this.length )
					if( this[i] == obj ) {
						this.splice(i,1);
						return true;
					}
				return false;
			}
			aproto.iterator = function() {
				var cur = 0;
				var arr : Array<Dynamic> = this;
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
			var cca = String.prototype.charCodeAt;
			String.prototype.charCodeAt = function(i) {
				var x = cca.call(this,i);
				if( __global__["isNaN"](x) )
					return null;
				return x;
			};
		}
		lines = new Array();
		var c = if( mc == null ) this else mc;
		flash.Lib.current = c;
		try {
			untyped if( c.stage != null && c.stage.align == "" )
				c.stage.align = "TOP_LEFT";
		} catch( e : Dynamic ) {
			// security error when loading from different domain
		}
		if( init != null )
			init();
	}

	public static function enum_to_string( e : { tag : String, params : Array<Dynamic> } ) {
		if( e.params == null )
			return e.tag;
		return e.tag+"("+e.params.join(",")+")";
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
		flash.Lib.current.removeChild(tf);
		tf = null;
		lines = new Array();
	}

	public static function __set_trace_color(rgb) {
		getTrace().textColor = rgb;
	}

	public static function getTrace() {
		var mc = flash.Lib.current;
		if( tf == null ) {
			tf = new flash.text.TextField();
			var format = tf.getTextFormat();
			format.font = "_sans";
			tf.defaultTextFormat = format;
			tf.selectable = false;
			tf.width = if( mc.stage == null ) 800 else mc.stage.stageWidth;
			tf.autoSize = flash.text.TextFieldAutoSize.LEFT;
			tf.mouseEnabled = false;
		}
		mc.addChild(tf); // on top
		return tf;
	}

	public static function __trace( v : Dynamic, pos : haxe.PosInfos ) {
		var tf = getTrace();
		var pstr = if( pos == null ) "(null)" else pos.fileName+":"+pos.lineNumber;
		lines = lines.concat((pstr +": "+__string_rec(v,"")).split("\n"));
		tf.text = lines.join("\n");
		var stage = flash.Lib.current.stage;
		if( stage == null )
			return;
		while( tf.height > stage.stageHeight ) {
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
			var s = "[";
			var i;
			var first = true;
			for( i in 0...v.length ) {
				if( first )
					first = false;
				else
					s += ",";
				s += __string_rec(v[i],str);
			}
			return s+"]";
		default:
			switch( untyped __typeof__(v) ) {
			case "function": return "<function>";
			}
		}
		return new String(v);
	}

	static function __unprotect__( s : String ) {
		return s;
	}

}
