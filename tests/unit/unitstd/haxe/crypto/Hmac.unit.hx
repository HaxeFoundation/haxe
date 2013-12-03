var hmacMd5 = new haxe.crypto.Hmac(MD5);
var hmacSha1 = new haxe.crypto.Hmac(SHA1);
var hmacSha256 = new haxe.crypto.Hmac(SHA256);

hmacMd5.make(haxe.io.Bytes.ofString(""), haxe.io.Bytes.ofString("")).toHex() == "74e6f7298a9c2d168935f58c001bad88";
hmacSha1.make(haxe.io.Bytes.ofString(""), haxe.io.Bytes.ofString("")).toHex() == "fbdb1d1b18aa6c08324b7d64b71fb76370690e1d";
hmacSha256.make(haxe.io.Bytes.ofString(""), haxe.io.Bytes.ofString("")).toHex() == "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad";

hmacMd5.make(haxe.io.Bytes.ofString("key"), haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog")).toHex() == "80070713463e7749b90c2dc24911e275";
hmacSha1.make(haxe.io.Bytes.ofString("key"), haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog")).toHex() == "de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9";
hmacSha256.make(haxe.io.Bytes.ofString("key"), haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog")).toHex() == "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8";