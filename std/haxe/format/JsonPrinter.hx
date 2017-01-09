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
package haxe.format;

/**
	An implementation of JSON printer in Haxe.

	This class is used by `haxe.Json` when native JSON implementation
	is not available.

	@see https://haxe.org/manual/std-Json-encoding.html
**/
class JsonPrinter {

	/**
		Encodes `o`'s value and returns the resulting JSON string.

		If `replacer` is given and is not null, it is used to retrieve
		actual object to be encoded. The `replacer` function takes two parameters,
		the key and the value being encoded. Initial key value is an empty string.

		If `space` is given and is not null, the result will be pretty-printed.
		Successive levels will be indented by this string.
	**/
	static public function print(o:Dynamic, ?replacer:Dynamic -> Dynamic -> Dynamic, ?space:String) : String {
		var printer = new JsonPrinter(replacer, space);
		printer.write("", o);
		return printer.buf.toString();
	}

	var buf : #if flash flash.utils.ByteArray #else StringBuf #end;
	var replacer : Dynamic -> Dynamic -> Dynamic;
	var indent:String;
	var pretty:Bool;
	var nind:Int;

	function new(replacer:Dynamic -> Dynamic -> Dynamic, space:String) {
		this.replacer = replacer;
		this.indent = space;
		this.pretty = space != null;
		this.nind = 0;

		#if flash
		buf = new flash.utils.ByteArray();
		buf.endian = flash.utils.Endian.BIG_ENDIAN;
		buf.position = 0;
		#else
		buf = new StringBuf();
		#end
	}

	inline function ipad ():Void {
		if (pretty) add(StringTools.lpad('', indent, nind * indent.length));
	}

	inline function newl ():Void {
		if (pretty) addChar('\n'.code);
	}

	function write(k:Dynamic, v:Dynamic) {
		if (replacer != null) v = replacer(k, v);
		switch( Type.typeof(v) ) {
		case TUnknown:
			add('"???"');
		case TObject:
			objString(v);
		case TInt:
			add(#if as3 Std.string(v) #else v #end);
		case TFloat:
			add(Math.isFinite(v) ? v : 'null');
		case TFunction:
			add('"<fun>"');
		case TClass(c):
			if( c == String )
				quote(v);
			else if( c == Array ) {
				var v : Array<Dynamic> = v;
				addChar('['.code);

				var len = v.length;
				var last = len - 1;
				for (i in 0...len)
				{
					if (i > 0) addChar(','.code) else nind++;
					newl();
					ipad();
					write(i, v[i]);
					if (i == last)
					{
						nind--;
						newl();
						ipad();
					}
				}
				addChar(']'.code);
			} else if( c == haxe.ds.StringMap ) {
				var v : haxe.ds.StringMap<Dynamic> = v;
				var o = {};
				for( k in v.keys() )
					Reflect.setField(o,k,v.get(k));
				objString(o);
			} else if( c == Date ) {
				var v : Date = v;
				quote(v.toString());
			} else
				#if flash
				classString(v);
				#else
				objString(v);
				#end
		case TEnum(_):
			var i : Dynamic = Type.enumIndex(v);
			add(i);
		case TBool:
			add(#if (php || as3) (v ? 'true' : 'false') #else v #end);
		case TNull:
			add('null');
		}
	}

	@:extern inline function addChar(c:Int) {
		#if flash
		buf.writeByte(c);
		#else
		buf.addChar(c);
		#end
	}

	@:extern inline function add(v:String) {
		#if flash
		// argument is not always a string but will be automatically casted
		buf.writeUTFBytes(v);
		#else
		buf.add(v);
		#end
	}

	#if flash
	function classString ( v : Dynamic ) {
		fieldsString(v,Type.getInstanceFields(Type.getClass(v)));
	}
	#end

	inline function objString( v : Dynamic ) {
		fieldsString(v,Reflect.fields(v));
	}

	function fieldsString( v : Dynamic, fields : Array<String> ) {
		addChar('{'.code);
		var len = fields.length;
		var last = len - 1;
		var first = true;
		for( i in 0...len ) {
			var f = fields[i];
			var value = Reflect.field(v,f);
			if( Reflect.isFunction(value) ) continue;
			if( first ) { nind++; first = false; } else addChar(','.code);
			newl();
			ipad();
			quote(f);
			addChar(':'.code);
			if (pretty) addChar(' '.code);
			write(f, value);
			if (i == last)
			{
				nind--;
				newl();
				ipad();
			}
		}
		addChar('}'.code);
	}

	function quote( s : String ) {
		#if (neko || php || cpp)
		if( s.length != haxe.Utf8.length(s) ) {
			quoteUtf8(s);
			return;
		}
		#end
		addChar('"'.code);
		var i = 0;
		while( true ) {
			var c = StringTools.fastCodeAt(s, i++);
			if( StringTools.isEof(c) ) break;
			switch( c ) {
			case '"'.code: add('\\"');
			case '\\'.code: add('\\\\');
			case '\n'.code: add('\\n');
			case '\r'.code: add('\\r');
			case '\t'.code: add('\\t');
			case 8: add('\\b');
			case 12: add('\\f');
			default:
				#if flash
				if( c >= 128 ) add(String.fromCharCode(c)) else addChar(c);
				#else
				addChar(c);
				#end
			}
		}
		addChar('"'.code);
	}

	#if (neko || php || cpp)
	function quoteUtf8( s : String ) {
		var u = new haxe.Utf8();
		haxe.Utf8.iter(s,function(c) {
			switch( c ) {
			case '\\'.code, '"'.code: u.addChar('\\'.code); u.addChar(c);
			case '\n'.code: u.addChar('\\'.code); u.addChar('n'.code);
			case '\r'.code: u.addChar('\\'.code); u.addChar('r'.code);
			case '\t'.code: u.addChar('\\'.code); u.addChar('t'.code);
			case 8: u.addChar('\\'.code); u.addChar('b'.code);
			case 12: u.addChar('\\'.code); u.addChar('f'.code);
			default: u.addChar(c);
			}
		});
		buf.add('"');
		buf.add(u.toString());
		buf.add('"');
	}
	#end
}
