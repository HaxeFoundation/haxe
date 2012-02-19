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
package haxe;

#if (flash_10_3 && !haxeJSON)
@:native('JSON') extern
#end
class JSON {

#if !(flash_10_3 && !haxeJSON)
	var buf : StringBuf;

	function new() {
	}

	function toString(v:Dynamic) {
		buf = new StringBuf();
		toStringRec(v);
		return buf.toString();
	}

	function objString( v : Dynamic ) {
		var first = true;
		buf.add('{');
		for( f in Reflect.fields(v) ) {
			var value = Reflect.field(v,f);
			if( Reflect.isFunction(value) ) continue;
			if( first ) first = false else buf.add(',');
			quote(f);
			buf.add(':');
			toStringRec(value);
		}
		buf.add('}');
	}

	function toStringRec(v:Dynamic) {
		switch( Type.typeof(v) ) {
		case TUnknown:
			buf.add('"???"');
		case TObject:
			objString(v);
		case TInt,TFloat:
			buf.add(v);
		case TFunction:
			buf.add('"<fun>"');
		case TClass(c):
			if( c == String )
				quote(v);
			else if( c == Array ) {
				var v : Array<Dynamic> = v;
				buf.add('[');
				var len = v.length;
				if( len > 0 ) {
					toStringRec(v[0]);
					var i = 1;
					while( i < len ) {
						buf.add(',');
						toStringRec(v[i++]);
					}
				}
				buf.add(']');
			} else if( c == Hash ) {
				var v : Hash<Dynamic> = v;
				var o = {};
				for( k in v.keys() )
					Reflect.setField(o,k,v.get(k));
				objString(o);
			} else if( v.iterator != null ) {
				var a = [];
				var it : Iterator<Dynamic> = v.iterator();
				for( v in it )
					a.push(v);
				toStringRec(a);
			} else
				objString(v);
		case TEnum(e):
			buf.add(Type.enumIndex(v));
		case TBool:
			buf.add(v ? 'true' : 'false');
		case TNull:
			buf.add('null');
		}
	}

	function quote( s : String ) {
		#if (neko || php || cpp)
		if( s.length != haxe.Utf8.length(s) ) {
			quoteUtf8(s);
			return;
		}
		#end
		buf.add('"');
		var i = 0;
		while( true ) {
			var c = StringTools.fastCodeAt(s,i++);
			if( StringTools.isEOF(c) ) break;
			switch( c ) {
			case '/'.code: buf.add('\\/');
			case '"'.code: buf.add('\\"');
			case '\\'.code: buf.add('\\\\');
			case '\n'.code: buf.add('\\n');
			case '\r'.code: buf.add('\\r');
			case '\t'.code: buf.add('\\t');
			case 8: buf.add('\\b');
			case 12: buf.add('\\f');
			default: buf.addChar(c);
			}
		}
		buf.add('"');
	}

	#if (neko || php || cpp)
	function quoteUtf8( s : String ) {
		var u = new haxe.Utf8();
		haxe.Utf8.iter(s,function(c) {
			switch( c ) {
			case '/'.code, '\\'.code, '"'.code: u.addChar('\\'.code); u.addChar(c);
			case '\n'.code: u.addChar('\\'.code); u.addChar('n'.code);
			case '\r'.code: u.addChar('\\'.code); u.addChar('r'.code);
			case '\t'.code: u.addChar('\\'.code); u.addChar('t'.code);
			case 8: u.addChar('\\'.code); u.addChar('b'.code);
			case 12: u.addChar('\\'.code); u.addChar('f'.code);
			default: u.addChar(c);
			}
		});
		buf.add("'");
		buf.add(u.toString());
		buf.add("'");
	}
	#end
#end

/*
	public static function parse( text : String ) {
		return new JSON().doParse(text);
	}
*/

	public static function stringify( value : Dynamic ) : String {
		return new JSON().toString(value);
	}

	#if !haxeJSON
		#if js
		static function __init__() untyped {
			if( __js__('typeof(JSON)') != 'undefined' )
				JSON = __js__('JSON');
		}
		#end
	#end

}
