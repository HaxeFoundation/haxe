haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), true) == "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), false) == "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), true) == "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog"), false) == "VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw";

haxe.crypto.Base64.decode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==", true).toString() == "The quick brown fox jumps over the lazy dog";
haxe.crypto.Base64.decode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw", false).toString() == "The quick brown fox jumps over the lazy dog";
haxe.crypto.Base64.urlDecode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw==", true).toString() == "The quick brown fox jumps over the lazy dog";
haxe.crypto.Base64.urlDecode("VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZw", false).toString() == "The quick brown fox jumps over the lazy dog";

haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("a"), true) == "YQ==";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab"), true) == "YWI=";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab?"), true) == "YWI/";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab~c"), true) == "YWJ+Yw==";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("a"), false) == "YQ";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab"), false) == "YWI";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab?"), false) == "YWI/";
haxe.crypto.Base64.encode(haxe.io.Bytes.ofString("ab~c"), false) == "YWJ+Yw";

haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("a"), true) == "YQ==";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab"), true) == "YWI=";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab?"), true) == "YWI_";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab~c"), true) == "YWJ-Yw==";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("a"), false) == "YQ";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab"), false) == "YWI";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab?"), false) == "YWI_";
haxe.crypto.Base64.urlEncode(haxe.io.Bytes.ofString("ab~c"), false) == "YWJ-Yw";

haxe.crypto.Base64.decode("YQ==", true).toString() == "a";
haxe.crypto.Base64.decode("YWI=", true).toString() == "ab";
haxe.crypto.Base64.decode("YWI/", true).toString() == "ab?";
haxe.crypto.Base64.decode("YWJ+Yw==", true).toString() == "ab~c";
haxe.crypto.Base64.decode("YQ", false).toString() == "a";
haxe.crypto.Base64.decode("YWI", false).toString() == "ab";
haxe.crypto.Base64.decode("YWI/", false).toString() == "ab?";
haxe.crypto.Base64.decode("YWJ+Yw", false).toString() == "ab~c";

haxe.crypto.Base64.urlDecode("YQ==", true).toString() == "a";
haxe.crypto.Base64.urlDecode("YWI=", true).toString() == "ab";
haxe.crypto.Base64.urlDecode("YWI_", true).toString() == "ab?";
haxe.crypto.Base64.urlDecode("YWJ-Yw==", true).toString() == "ab~c";
haxe.crypto.Base64.urlDecode("YQ", false).toString() == "a";
haxe.crypto.Base64.urlDecode("YWI", false).toString() == "ab";
haxe.crypto.Base64.urlDecode("YWI_", false).toString() == "ab?";
haxe.crypto.Base64.urlDecode("YWJ-Yw", false).toString() == "ab~c";
