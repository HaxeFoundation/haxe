package unit.issues;

using haxe.Int64Helper;
using haxe.Int64;

private typedef Repr = {
	var value:String;
	var stripped:String;
	var complement:String;
}

class Issue6640 extends Test {
	static inline var MIN_INT64_STR = "-9223372036854775808";
	static inline var MAX_INT64_STR = "9223372036854775807";
	static inline var ABS_MIN_INT64_STR = "9223372036854775808";
	static inline var ABS_MAX_INT64_STR = "9223372036854775807";
	
	public function test() {

		t(Int64Helper.parseString(MIN_INT64_STR).toStr() == MIN_INT64_STR);
		t(Int64Helper.parseString(MAX_INT64_STR).toStr() == MAX_INT64_STR);
		
		t(Int64Helper.parseString("-0") == 0);
		
		t(numLexiCompare("-0", "0") == 0);
		t(numLexiCompare("0000", "0") == 0);
		t(numLexiCompare("0000", "-0000") == 0);
		t(numLexiCompare("-0001", "-1") == 0);
		t(numLexiCompare("1", "0001") == 0);
		t(numLexiCompare("-1", "0001") == -1);
		t(numLexiCompare("01", "-1") == 1);
		t(numLexiCompare("1", "-1") == 1);
		t(numLexiCompare("-1", "1") == -1);
	
		t(isInInt64Range("-7834911985406430862"));
		t(isInInt64Range(MIN_INT64_STR));
		t(isInInt64Range(MAX_INT64_STR));
		f(isInInt64Range(ABS_MIN_INT64_STR));
		
		var validInt64Strings = [
			"9560",
			"-9109",
			"-0317551087311700367",
			"008854131894243499964",
			"-0087507667970265160",
			"-0242362870617229368",
			"0000" + ABS_MAX_INT64_STR,
			"-0" + ABS_MIN_INT64_STR,
			"-7834911985406430862",
		];
		
		for (str in validInt64Strings) checkDecString(str);
		
		
		var invalidInt64Strings = [
			"-9223372036854775809", // MIN_INT64 - 1
			"9223372036854775808", // MAX_INT64 + 1
			"-09223372036854775809", // MIN_INT64 - 1
			"09223372036854775808", // MAX_INT64 + 1
			
			// from https://github.com/HaxeFoundation/haxe/issues/6640
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
		
		for (str in invalidInt64Strings) checkDecString(str);
		
		// random strings
		var N = 50;
		for (i in 0...N) {
			var str = getRandDecString();
			checkDecString(str);
		}
	}
	
	function checkDecString(str:String) {
		var inRange = isInInt64Range(str);
		
		if (inRange) {
			try {
				var i64:Int64 = Int64Helper.parseString(str);
				eq(i64.toStr(), stripLeadingZeros(str));
			} catch (err:Dynamic){
				trace("this should be valid, but `" + str + "` threw an exception while isInInt64Range returned " + inRange);
				f(true);
			}
		} else {
			try {
				var i64:Int64 = Int64Helper.parseString(str);
				trace("this should NOT be valid, but `" + str + "` returned i64:" + i64.toStr() + " while isInInt64Range returned " + inRange);
				eq(i64.toStr(), stripLeadingZeros(str));
			} catch (err:Dynamic){
				t(true);
			}
		}
	}
	
	static function getRandDecString(minLen:Int = 1, maxLen:Int = 25):String {
		var chars = "000123456789".split(""); // make string containing zeros more probable
		var sign = Math.random() > .5 ? "-" : "";
		var length = minLen + Std.random(maxLen - minLen);
		var randString = sign + [for (i in 0...length) chars[Std.random(chars.length)]].join("");
		return randString;
	}
	
	static inline function isNegativeStr(s:String):Bool {
		return s.charAt(0) == '-';
	}
	
	static inline function leftPad(s:String, with:String, len:Int):String {
		return StringTools.lpad(s, with, len);
	}
	
	static inline function isNullOrEmpty(s:String):Bool {
		return s == null || s == "";
	}
	
	static function stripLeadingZeros(s:String):String {
		var regex:EReg = ~/^([-])?([0]+)(.*)/g;
		if (regex.match(s)) {
			var sign = isNullOrEmpty(regex.matched(1)) ? "" : regex.matched(1);
			var rest = isNullOrEmpty(regex.matched(3)) ? null : regex.matched(3);
			if (rest == null) return "0";
			return sign + rest;
		} else {
			return s;
		}
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
		
		var noZeros = stripLeadingZeros(s);
		var isNegative = isNegativeStr(noZeros);
		var repr:Repr = { value:s, stripped:noZeros, complement:noZeros };
		if (isNegative) {
			var abs = noZeros.substr(1);
			var padded = leftPad(abs, "0", len);
			repr.complement = "0" + makeComplement(padded);
		} else {
			var abs = noZeros;
			var padded = leftPad(abs, "0", len);
			repr.complement = "9" + padded;
		}
		
		return repr;
	}
	
	static inline function cmp(a:Repr, b:Repr):Int {
		return (a.complement < b.complement) ? -1 : (a.complement > b.complement) ? 1 : 0;
	}
	
	static function numLexiCompare(a:String, b:String):Int {
		var len = a.length >= b.length ? a.length : b.length;
		
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