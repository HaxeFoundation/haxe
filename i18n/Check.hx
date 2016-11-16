import haxe.io.Bytes;
import haxe.i18n.Utf8;
import haxe.i18n.Utf16;
import haxe.i18n.Ucs2;
class Check {

	static function main () {


		haxe.Log.trace;

		


		//var x = new Utf8("foofoofoobarbar");
		//trace(x.lastIndexOf(new Utf8("r")));
		//trace(x.indexOf(new Utf8("r")));

		//trace(x.lastIndexOf(new Utf8("bar")));


		//var r = [1,2];
		//trace(r);
		//trace(r.length);
		//r.push(1);
		//trace(r);
		//trace(r.length);
//
		//showCode(0x1D11E, "\u{1D11E}"); // 𝄞
		//showCode(0x4E00, "\u{4E00}");//一

		
	}

	static function showCode (code:Int, nativeString:String) {

		

		var codes = [];
		for (i in 0...nativeString.length) {
			var code = nativeString.charCodeAt(i);
			codes.push(code);
		}

		trace("---------------------------------------");
		trace("length:     " + nativeString.length);
		trace("codes:      " + codes);
		trace("hex-codes:  " + codes.map(function (x) return StringTools.hex(x) ));
		
		
		trace("code hex:   " + StringTools.hex(code));
		trace("native str: " + nativeString);
		trace("native hex: " + Bytes.ofString(nativeString).toHex());
		trace("utf-8:      " + Utf8.fromCharCode(code).toBytes().toHex());
		trace("utf-16:     " + Utf16.fromCharCode(code).toBytes().toHex());
		if (code <= 0xFFFF) {
		trace("ucs-2:      " + Ucs2.fromCharCode(code).toBytes().toHex());
		} else {
		trace("ucs-2:      no representation");
		}
	}
}