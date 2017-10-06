package unit.issues;

using haxe.Int64Helper;
using haxe.Int64;

private typedef Repr = {
	var value:String;
	var complement:String;
}

class Issue6640 extends Test {
	static inline var MIN_INT64_STR = "-9223372036854775808";
	static inline var MAX_INT64_STR = "9223372036854775807";
	static inline var ABS_MIN_INT64_STR = "9223372036854775808";
	static inline var ABS_MAX_INT64_STR = "9223372036854775807";

	function test() {

		t(Int64Helper.parseString(MIN_INT64_STR).toStr() == MIN_INT64_STR);
		t(Int64Helper.parseString(MAX_INT64_STR).toStr() == MAX_INT64_STR);
		
		t(Int64Helper.parseString("-0") == 0);
		//t(numLexiCompare("-0", "0") == 0); // won't work with numLexiCompare()
		
		t(numLexiCompare("0000", "0") == 0);
		t(numLexiCompare("-0001", "-1") == 0);
		t(numLexiCompare("1", "0001") == 0);
		
		var validInt64Strings = [
			"-0317551087311700367",
			"008854131894243499964",
			"-0087507667970265160",
			"-0242362870617229368",
			"0000" + ABS_MAX_INT64_STR,
			"-0" + ABS_MIN_INT64_STR,
		];
		
		for (s in validInt64Strings) {
			try {
				var i64:Int64 = Int64Helper.parseString(s);
				t(numLexiCompare(i64.toStr(), s) == 0);
			} catch (err:Dynamic) {
				trace("this should be valid, but:	" + s + " isInInt64Range " + isInInt64Range(s));
				f(true);
			}
		}
		
		var invalidInt64Strings = [
			"-9223372036854775809", // MIN_INT64 - 1
			"9223372036854775808", // MAX_INT64 + 1
			"-09223372036854775809", // MIN_INT64 - 1
			"09223372036854775808", // MAX_INT64 + 1
			
			// directly from issue6640
			"-5518926151087227722779",
			"18484280858095180538",
			"-425386078818862309697",
			"-576944185342198403634548",
			"74817639777751565309",
			
			// more
			"-00159545534313305560850",
			"01597247592514562010911",
			"-025236287061722936894",
			"-99777412066094807232797",
			"88851476464433781269008",
		];
		
		for (s in invalidInt64Strings) {
			try {
				var i64:Int64 = Int64Helper.parseString(s);
				trace("this should NOT be valid, but: isInInt64Range(" + s + "):" + isInInt64Range(s) + " vs i64:" + i64.toStr());
				f(true);
			} catch (err:Dynamic) {
				t(true);
			}
		}
	}
	
	static inline function isNegativeStr(s:String):Bool {
		return s.charAt(0) == '-';
	}
	
	static inline function leftZeroPad(s:String, len:Int):String {
		return StringTools.lpad(s, "0", len);
	}
	
	static function createRepr(s:String, len:Int):Repr {
		inline function makeComplement(s:String) {
			var buf = new StringBuf();
			for (i in 0...s.length) {
				var c = s.charCodeAt(i);
				buf.addChar('0'.code + '9'.code - c);
			}
			return buf.toString();
		}
		
		var isNegative = isNegativeStr(s);
		var abs = isNegative ? s.substr(1) : s;
		var zeroPadded = leftZeroPad(abs, len);
		var complement = isNegative ? makeComplement(zeroPadded) : zeroPadded;
		return { value:s, complement:complement };
	}
	
	static inline function cmp(a:Repr, b:Repr):Int {
		return (a.complement < b.complement) ? 1 : (a.complement > b.complement) ? -1 : 0;
	}
	
	static function numLexiCompare(a:String, b:String):Int {
		var len = a.length > b.length ? a.length : b.length;
		
		var first = createRepr(a, len);
		var second = createRepr(b, len);
		
		var compare = cmp(first, second);
		//trace(compare, first, second);
		
		return compare;
	}
	
	static inline function isInInt64Range(a:String):Bool {
		return (numLexiCompare(a, MIN_INT64_STR) >= 0 && numLexiCompare(a, MAX_INT64_STR) <= 0); 
	}
}
