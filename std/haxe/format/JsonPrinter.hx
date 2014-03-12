package haxe.format;

class JsonPrinter {

	static public function print(o:Dynamic, ?replacer:Dynamic -> Dynamic -> Dynamic, ?space:String) : String {
		var printer = new JsonPrinter(replacer, space);
		printer.write("", o);
		return printer.buf.toString();
	}

	var buf : #if flash9 flash.utils.ByteArray #else StringBuf #end;
	var replacer : Dynamic -> Dynamic -> Dynamic;
	var indent:String;
	var nl:String;
	var pretty:Bool;
	var nind:Int;
	
	function new(replacer:Dynamic -> Dynamic -> Dynamic, space:String) {
		this.replacer = replacer;
		this.indent = space;
		this.nl = '\n';
		this.pretty = space != null;
		this.nind = 0;

		#if flash9
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
		if (pretty) add(nl);
	}

	function write(k:Dynamic, v:Dynamic) {
		if (replacer != null) v = replacer(k, v);
		switch( Type.typeof(v) ) {
		case TUnknown:
			add('"???"');
		case TObject:
			objString(v);
		case TInt:
			add(v);
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
				nind++;
				var len = v.length;
				if( len > 0 ) {
					newl();
					ipad();
					write(0, v[0]);
					var i = 1;
					while( i < len ) {
						addChar(','.code);
						newl();
						ipad();
						write(i, v[i++]);
					}
				}
				nind--;
				newl();
				ipad();
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
				#if flash9
				classString(v);
				#else
				objString(v);
				#end
		case TEnum(_):
			var i : Dynamic = Type.enumIndex(v);
			add(i);
		case TBool:
			add(#if php (v ? 'true' : 'false') #else v #end);
		case TNull:
			add('null');
		}
	}

	@:extern inline function addChar(c:Int) {
		#if flash9
		buf.writeByte(c);
		#else
		buf.addChar(c);
		#end
	}

	@:extern inline function add(v:String) {
		#if flash9
		// argument is not always a string but will be automatically casted
		buf.writeUTFBytes(v);
		#else
		buf.add(v);
		#end
	}

	#if flash9
	function classString ( v : Dynamic ) {
		fieldsString(v,Type.getInstanceFields(Type.getClass(v)));
	}
	#end

	inline function objString( v : Dynamic ) {
		fieldsString(v,Reflect.fields(v));
	}

	function fieldsString( v : Dynamic, fields : Array<String> ) {
		var first = true;
		addChar('{'.code);
		nind++;
		for( f in fields ) {
			var value = Reflect.field(v,f);
			if( Reflect.isFunction(value) ) continue;
			if( first ) first = false else addChar(','.code);
			newl();
			ipad();
			quote(f);
			addChar(':'.code);
			if (pretty) addChar(' '.code);
			write(f, value);
		}
		nind--;
		newl();
		ipad();
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
				#if flash9
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
