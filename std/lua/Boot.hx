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

package lua;

import haxe.Constraints.Function;


@:dox(hide)
class Boot {

	// Used temporarily for bind()
	static var _;
	static var _fid = 0;

	public static var platformBigEndian = NativeStringTools.byte(NativeStringTools.dump(function(){}),7) > 0;

	static var hiddenFields : Table<String,Bool> = untyped __lua__("{__id__=true, hx__closures=true, super=true, prototype=true, __fields__=true, __ifields__=true, __class__=true, __properties__=true}");



	static function __unhtml(s : String)
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");

	/*
	   Indicates if the given object is a class.
	*/
	static inline public function isClass(o:Dynamic) : Bool {
		if (Lua.type(o) != "table") return false;
		else return untyped __define_feature__("lua.Boot.isClass", o.__name__);
	}

	/*
	   Indicates if the given object is a enum.
	*/
	static inline public function isEnum(e:Dynamic) : Bool {
		if (Lua.type(e) != "table") return false;
		else return untyped __define_feature__("lua.Boot.isEnum", e.__ename__);
	}

	/*
	   Returns the class of a given object, and defines the getClass feature
	   for the given class.
	*/
	static inline public function getClass(o:Dynamic) : Class<Dynamic> {
		if (Std.is(o, Array)) return Array;
		else {
			var cl = untyped __define_feature__("lua.Boot.getClass", o.__class__);
			if (cl != null) return cl;
			else return null;
		}
	}

	/*
	   Indicates if the given object is an instance of the given Type
	*/
	@:ifFeature("typed_catch")
	private static function __instanceof(o : Dynamic, cl : Dynamic) {
		if( cl == null ) return false;

		switch( cl ) {
			case Int:
				return (Lua.type(o) == "number" &&  clamp(o) == o);
			case Float:
				return Lua.type(o) == "number";
			case Bool:
				return Lua.type(o) == "boolean";
			case String:
				return Lua.type(o) == "string";
			case Thread:
				return Lua.type(o) == "thread";
			case UserData:
				return Lua.type(o) == "userdata";
			case Array:
				return isArray(o);
			case Table:
				return Lua.type(o) == "table";
			case Dynamic:
				return true;
			default: {
				if ( o!= null &&  Lua.type(o)  == "table" && Lua.type(cl) == "table"){
					if (extendsOrImplements(getClass(o), cl)) return true;
					// We've exhausted standard inheritance checks.  Check for simple Class/Enum eqauality
					// Also, do not use isClass/isEnum here, perform raw checks
					untyped __feature__("Class.*",if( cl == Class && o.__name__ != null ) return true);
					untyped __feature__("Enum.*",if( cl == Enum && o.__ename__ != null ) return true);
					// last chance, is it an enum instance?
					return o.__enum__ == cl;
				} else {
					return false;
				}
			}
		}
	}
	static function isArray(o:Dynamic) : Bool {
		return Lua.type(o) == "table"
			&& untyped o.__enum__ == null
			&& Lua.getmetatable(o) != null
			&& Lua.getmetatable(o).__index == untyped Array.prototype;
	}

	/*
	   Indicates if the given object inherits from the given class
	*/
	static function inheritsFrom(o:Dynamic, cl:Class<Dynamic>) : Bool {
		while (Lua.getmetatable(o) != null && Lua.getmetatable(o).__index != null){
			if (Lua.getmetatable(o).__index == untyped cl.prototype) return true;
			o = Lua.getmetatable(o).__index;
		}
		return false;
	}

	@:ifFeature("typed_cast")
	private static function __cast(o : Dynamic, t : Dynamic) {
		if (__instanceof(o, t)) return o;
		else throw "Cannot cast " +Std.string(o) + " to " +Std.string(t);
	}

	/*
	   Helper method to generate a string representation of an enum
	*/
	static function printEnum(o:Array<Dynamic>, s : String){
		if (o.length == 2){
			return o[0];
		} else {
			// parameterized enums are arrays
			var str = o[0] + "(";
			s += "\t";
			for (i in 2...o.length){
				if( i != 2 )
					str += "," + __string_rec(o[i],s);
				else
					str += __string_rec(o[i],s);
			}
			return str + ")";
		}
	}

	/*
	   Helper method to generate a string representation of a class
	*/
	static inline function printClass(c:Table<String,Dynamic>, s : String) : String {
		return '{${printClassRec(c,'',s)}}';
	}

	/*
	   Helper method to generate a string representation of a class
	*/
	static function printClassRec(c:Table<String,Dynamic>, result='', s : String) : String {
		var f = Boot.__string_rec;
		untyped __lua__("for k,v in pairs(c) do if result ~= '' then result = result .. ', ' end result = result .. k .. ':' .. f(v, s.. '\t') end");
		return result;
	}

	/*
	   Generate a string representation for arbitrary object.
	*/
	@:ifFeature("has_enum")
	static function __string_rec(o : Dynamic, s:String = "") {
		return switch(untyped __type__(o)){
			case "nil": "null";
			case "number" : {
				if (o == std.Math.POSITIVE_INFINITY) "Infinity";
				else if (o == std.Math.NEGATIVE_INFINITY) "-Infinity";
				else if (o != o) "NaN";
				else untyped tostring(o);
			}
			case "boolean" : untyped tostring(o);
			case "string"  : o;
			case "userdata": "<userdata>";
			case "function": "<function>";
			case "thread"  : "<thread>";
			case "table": {
			    if (o.__enum__ != null) printEnum(o,s);
				else if (o.toString != null && !isArray(o)) o.toString();
				else if (isArray(o)) {
					var o2 : Array<Dynamic> = untyped o;
					if (s.length > 5) "[...]"
					else '[${[for (i in o2) __string_rec(i,s+1)].join(",")}]';
				}
				else if (o.__class__ != null) printClass(o,s+"\t");
				else {
					var fields = fieldIterator(o);
					var buffer:Table<Int,String> = Table.create();
					var first = true;
					Table.insert(buffer,"{ ");
					for (f in fields){
						if (first) first = false;
						else Table.insert(buffer,", ");
						Table.insert(buffer,'${Std.string(f)} : ${untyped Std.string(o[f])}');
					}
					Table.insert(buffer, " }");
					Table.concat(buffer, "");
				}
			};
			default : {
				throw "Unknown Lua type";
				null;
			}
		}

	}

	/*
	   Define an array from the given table
	*/
	public inline static function defArray<T>(tab: Table<Int,T>, ?length : Int) : Array<T> {
		if (length == null) length = TableTools.maxn(tab) + 1; // maxn doesn't count 0 index
		return untyped _hx_tab_array(tab, length);
	}

	/*
	   Create a Haxe object from the given table structure
	*/
	public inline static function tableToObject<T>(t:Table<String,T>) : Dynamic<T> {
		return untyped _hx_o(t);
	}

	/*
	   Get Date object as string representation
	*/
	public static function dateStr( date : std.Date ) : String {
		var m = date.getMonth() + 1;
		var d = date.getDate();
		var h = date.getHours();
		var mi = date.getMinutes();
		var s = date.getSeconds();
		return date.getFullYear()
			+"-"+(if( m < 10 ) "0"+m else ""+m)
			+"-"+(if( d < 10 ) "0"+d else ""+d)
			+" "+(if( h < 10 ) "0"+h else ""+h)
			+":"+(if( mi < 10 ) "0"+mi else ""+mi)
			+":"+(if( s < 10 ) "0"+s else ""+s);
	}

	/*
	   A 32 bit clamp function for numbers
	*/
	public inline static function clamp(x:Float){
		return untyped __define_feature__("lua.Boot.clamp", _hx_bit_clamp(x));
	}

	/*
	   Create a standard date object from a lua string representation
	*/
	public static function strDate( s : String ) : std.Date {
		switch( s.length ) {
		case 8: // hh:mm:ss
			var k = s.split(":");
			var t = lua.Os.time({
				year  : 0,
				month : 1,
				day   : 1,
				hour  : Lua.tonumber(k[0]),
				min   : Lua.tonumber(k[1]),
				sec   : Lua.tonumber(k[2])
			});
			return std.Date.fromTime(t);
		case 10: // YYYY-MM-DD
			var k = s.split("-");
			return new std.Date(Lua.tonumber(k[0]), Lua.tonumber(k[1]) - 1, Lua.tonumber(k[2]),0,0,0);
		case 19: // YYYY-MM-DD hh:mm:ss
			var k = s.split(" ");
			var y = k[0].split("-");
			var t = k[1].split(":");
			return new std.Date(cast y[0],Lua.tonumber(y[1]) - 1, Lua.tonumber(y[2]),Lua.tonumber(t[0]),Lua.tonumber(t[1]),Lua.tonumber(t[2]));
		default:
			throw "Invalid date format : " + s;
		}
	}

	/*
	  Helper method to determine if class cl1 extends, implements, or otherwise equals cl2
	*/
	public static function extendsOrImplements(cl1 : Class<Dynamic>, cl2 : Class<Dynamic>) : Bool {
		if (cl1 == null || cl2 == null) return false;
		else if (cl1 == cl2) return true;
		else if (untyped cl1.__interfaces__ != null) {
			var intf = untyped cl1.__interfaces__;
			for (i in 1...( TableTools.maxn(intf) + 1)){
				// check each interface, including extended interfaces
				if (extendsOrImplements(intf[i], cl2)) return true;
			}
		}
		// check standard inheritance
		return extendsOrImplements(untyped cl1.__super__, cl2);
	}


	/*
		Returns a shell escaped version of "cmd" along with any args
	*/
	public static function shellEscapeCmd(cmd : String, ?args : Array<String>){
		if (args != null) {
			switch (Sys.systemName()) {
				case "Windows":
					cmd = [
						for (a in [StringTools.replace(cmd, "/", "\\")].concat(args))
						StringTools.quoteWinArg(a, true)
					].join(" ");
				case _:
					cmd = [cmd].concat(args).map(StringTools.quoteUnixArg).join(" ");
			}
		}
		return cmd;
	}

	/*
	   Returns a temp file path that can be used for reading and writing
	*/
	public static function tempFile() : String {
		switch (Sys.systemName()){
			case "Windows" : return haxe.io.Path.join([Os.getenv("TMP"), Os.tmpname()]);
			default : return Os.tmpname();
		}
	}

	public static function fieldIterator( o : Table<String,Dynamic>) : Iterator<String> {
		var tbl : Table<String,String> =  cast (untyped o.__fields__ != null) ?  o.__fields__ : o;
		var cur = Lua.pairs(tbl).next;
		var next_valid = function(tbl, val){
			while (hiddenFields[untyped val] != null){
				val = cur(tbl, val).index;
			}
			return val;
		}
		var cur_val = next_valid(tbl, cur(tbl, null).index);
		return {
			next : function(){
				var ret = cur_val;
				cur_val = next_valid(tbl, cur(tbl, cur_val).index);
				return ret;
			},
			hasNext : function() return cur_val !=  null
		}
	}

	static var os_patterns = [
		'Windows' => ['windows','^mingw','^cygwin'],
		'Linux'   => ['linux'],
		'Mac'     => ['mac','darwin'],
		'BSD'     => ['bsd$'],
		'Solaris' => ['SunOS']
	];

	public static function systemName() : String {
		var os : String = null;
		if (untyped jit != null && untyped jit.os != null ){
			os = untyped jit.os;
			os = os.toLowerCase();
		} else {
			var popen_status : Bool = false;
			var popen_result : lua.FileHandle = null;
			untyped __lua__("popen_status, popen_result = pcall(_G.io.popen, '')");
			if (popen_status) {
				popen_result.close;
				os = lua.Io.popen('uname -s','r').read('*l').toLowerCase();
			} else {
				os = lua.Os.getenv('OS').toLowerCase();
			}

		}

		for (k in os_patterns.keys()){
			for (p in os_patterns.get(k)) {
				if (lua.NativeStringTools.match(os,p) != null){
					return k;
				}
			}
		}

		return null;
	}
}
