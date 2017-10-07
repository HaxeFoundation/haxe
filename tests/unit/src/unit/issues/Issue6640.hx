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
  
	static var MIN_INT64_REPR = createRepr(MIN_INT64_STR);
	static var MAX_INT64_REPR = createRepr(MAX_INT64_STR);

	public function test() {

		t(Int64Helper.parseString(MIN_INT64_STR).toStr() == MIN_INT64_STR);
		t(Int64Helper.parseString(MAX_INT64_STR).toStr() == MAX_INT64_STR);
		
		t(Int64Helper.parseString("-0") == 0);
		
    t(numLexiCompare("-0", "0") == 0);
		t(numLexiCompare("0000", "0") == 0);
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
			"-0317551087311700367",
			"008854131894243499964",
			"-0087507667970265160", // lua-only issue
			"-0242362870617229368",
			"0000" + ABS_MAX_INT64_STR,
			"-0" + ABS_MIN_INT64_STR,
			"-7834911985406430862",
		];
		
		for (s in validInt64Strings) {
			try {
				var i64:Int64 = Int64Helper.parseString(s);
				t(isInInt64Range(s));
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
		
		for (s in invalidInt64Strings) {
			try {
				var i64:Int64 = Int64Helper.parseString(s);
				trace("this should NOT be valid, but: isInInt64Range(" + s + "):" + isInInt64Range(s) + " vs i64:" + i64.toStr());
				f(isInInt64Range(s));
				f(true);
			} catch (err:Dynamic) {
				t(true);
			}
		}
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
  
	static function createRepr(s:String):Repr {
		inline function makeComplement(s:String) {
			var buf = new StringBuf();
			for (i in 0...s.length) {
				var c = s.charCodeAt(i);
				buf.addChar('0'.code + '9'.code - c);
			}
			return buf.toString();
		}
		
		var isNegative = isNegativeStr(s);
    var noZeros = stripLeadingZeros(s);
		var abs = isNegative ? noZeros.substr(1) : noZeros;
    var padded = isNegative ? leftPad(abs, "9", noZeros.length) : noZeros;
		var complement = isNegative ? makeComplement(padded) : padded;
		return { value:s, stripped:noZeros, complement:complement };
	}
	
	static inline function cmp(a:Repr, b:Repr):Int {
		return (a.complement < b.complement) ? -1 : (a.complement > b.complement) ? 1 : 0;
	}
	
	static function numLexiCompare(a:String, b:String):Int {
		var first = createRepr(a);
		var second = createRepr(b);
		
		var compare = cmp(first, second);
		//trace(compare, first, second);
		
		return compare;
	}
	
	static inline function isInInt64Range(a:String):Bool {
		return (cmp(createRepr(a), MIN_INT64_REPR) >= 0 && cmp(createRepr(a), MAX_INT64_REPR) <= 0);
	}
}