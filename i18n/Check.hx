import haxe.io.Bytes;
import haxe.i18n.Utf8;
import haxe.i18n.Utf16;
import haxe.i18n.Ucs2;
class Check {

	static function main () {
		haxe.Log.trace;
		var vioCode = 0x4E00;
		var violine = "\u{4E00}";

		trace(violine.length);

		var codes = [];
		for (i in 0...violine.length) {
			var code = violine.charCodeAt(i);
			codes.push(code);
		}

		trace(codes);
		trace(codes.map(function (x) return StringTools.hex(x) ));
		
		trace(violine);
		

		trace("native");
		trace(Bytes.ofString(violine).toHex());
		trace(StringTools.hex(vioCode));
		trace("utf-8");
		trace(Utf8.fromCharCode(vioCode).toBytes().toHex());
		trace("utf-16");
		trace(Utf8.fromCharCode(vioCode).toUtf16().toBytes().toHex());
		trace(Utf16.fromCharCode(vioCode).toBytes().toHex());
		trace("ucs2-16");
		trace(Ucs2.fromCharCode(vioCode).toBytes().toHex());
	}
}