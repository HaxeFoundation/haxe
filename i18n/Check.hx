import haxe.io.Bytes;
import haxe.i18n.Utf8;
import haxe.i18n.Utf16;
import haxe.i18n.Ucs2;
import haxe.i18n.ByteAccess;
import haxe.i18n.ByteAccessBuffer;
import haxe.i18n.BytesBufferTools;
import haxe.i18n.Encoding;
import haxe.i18n.NativeStringTools;

class Check {

	static function main () {

		var eq1 = function (a:haxe.i18n.Utf8, b:haxe.i18n.Utf8, ?pos:haxe.PosInfos) {
			$type(pos);
			pos.customParams = [a.toCodeArray(), b.toCodeArray()];
			haxe.Log.trace(a == b, pos);
		}

		haxe.Log.trace;
		var wrap = function (s) return new haxe.i18n.Utf8(s);
		wrap("ऽ𝄞Éa").length == 4;

		wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").length == 15; 

		wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").indexOf(wrap("É𝄞ÉÉ")) == 5;

		wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").indexOf(wrap("É𝄞ÉÉ")) == 5;

		wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").lastIndexOf(wrap("É𝄞ÉÉ")) == 11;
		wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").lastIndexOf(wrap("É𝄞ÉÉ")) == 11;
		eq1(wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").substr(0, -1), wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞É"));
		var s = wrap("ऽ𝄞Éoxfooxxbarxbarxx");
		eq1(s.substring(0, 0), wrap(""));
		eq1(s.substring(0, 1), wrap("ऽ"));
		eq1(s.substring(1, 0), wrap("ऽ"));
		eq1(s.substring(0, 2), wrap("ऽ𝄞"));
		eq1(s.substring(2, 0), wrap("ऽ𝄞"));
		trace(s.substring(-1, 0));
		eq1(s.substring(-1, 0), wrap(""));
		
		
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