import haxe.io.Bytes;
import haxe.i18n.Utf8;
import haxe.i18n.Utf16;
import haxe.i18n.Ucs2;
import haxe.i18n.Utf32;
import haxe.i18n.ByteAccess;
import haxe.i18n.ByteAccessBuffer;
import haxe.i18n.Tools;

class Check {

	static function main () {
		showCode( "𝌆".code, "𝌆");

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

		trace("ucs-2:      " + Ucs2.fromCharCode(code).toBytes().toHex());

	}
}