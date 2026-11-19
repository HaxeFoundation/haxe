package unit.teststd.haxe.crypto;

class TestBase64 extends unit.Test {
	public function test() {
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), true), "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==");
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), false), "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), true), "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), false), "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw");

		eq(haxe.crypto.Base64.decode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==", true).toString(), "The quick brown fox jumps over the lazy dog");
		eq(haxe.crypto.Base64.decode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw", false).toString(), "The quick brown fox jumps over the lazy dog");
		eq(haxe.crypto.Base64.urlDecode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==", true).toString(), "The quick brown fox jumps over the lazy dog");
		eq(haxe.crypto.Base64.urlDecode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw", false).toString(), "The quick brown fox jumps over the lazy dog");

		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("a"), true), "YQ==");
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab"), true), "YWI=");
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab?"), true), "YWI/");
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab~c"), true), "YWJ+Yw==");
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("a"), false), "YQ");
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab"), false), "YWI");
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab?"), false), "YWI/");
		eq(haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab~c"), false), "YWJ+Yw");

		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("a"), true), "YQ==");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab"), true), "YWI=");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab?"), true), "YWI_");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab~c"), true), "YWJ-Yw==");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("a"), false), "YQ");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab"), false), "YWI");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab?"), false), "YWI_");
		eq(haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab~c"), false), "YWJ-Yw");

		eq(haxe.crypto.Base64.decode("YQ==", true).toString(), "a");
		eq(haxe.crypto.Base64.decode("YWI=", true).toString(), "ab");
		eq(haxe.crypto.Base64.decode("YWI/", true).toString(), "ab?");
		eq(haxe.crypto.Base64.decode("YWJ+Yw==", true).toString(), "ab~c");
		eq(haxe.crypto.Base64.decode("YQ", false).toString(), "a");
		eq(haxe.crypto.Base64.decode("YWI", false).toString(), "ab");
		eq(haxe.crypto.Base64.decode("YWI/", false).toString(), "ab?");
		eq(haxe.crypto.Base64.decode("YWJ+Yw", false).toString(), "ab~c");

		eq(haxe.crypto.Base64.urlDecode("YQ==", true).toString(), "a");
		eq(haxe.crypto.Base64.urlDecode("YWI=", true).toString(), "ab");
		eq(haxe.crypto.Base64.urlDecode("YWI_", true).toString(), "ab?");
		eq(haxe.crypto.Base64.urlDecode("YWJ-Yw==", true).toString(), "ab~c");
		eq(haxe.crypto.Base64.urlDecode("YQ", false).toString(), "a");
		eq(haxe.crypto.Base64.urlDecode("YWI", false).toString(), "ab");
		eq(haxe.crypto.Base64.urlDecode("YWI_", false).toString(), "ab?");
		eq(haxe.crypto.Base64.urlDecode("YWJ-Yw", false).toString(), "ab~c");

		// every byte value, both alphabets, with and without padding
		var all = haxe.io.Bytes.alloc(256 * 3 + 1);
		for (i in 0...all.length)
			all.set(i, i & 255);
		var encoded = haxe.crypto.Base64.encode(all);
		eq(encoded.length, 1028);
		eq(encoded.substr(0, 24), "AAECAwQFBgcICQoLDA0ODxAR");
		eq(encoded.substr(-8), "/f7/AA==");
		eq(haxe.crypto.Base64.decode(encoded).compare(all), 0);
		eq(haxe.crypto.Base64.decode(haxe.crypto.Base64.encode(all, false), false).compare(all), 0);
		var urlEncoded = haxe.crypto.Base64.urlEncode(all);
		eq(urlEncoded, StringTools.replace(StringTools.replace(haxe.crypto.Base64.encode(all, false), "+", "-"), "/", "_"));
		eq(haxe.crypto.Base64.urlDecode(urlEncoded).compare(all), 0);
	}
}
