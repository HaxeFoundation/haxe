package cs.system.security.cryptography;

@:native("System.Security.Cryptography.AesGcm")
extern class AesGcm {
	static var NonceByteSizes(default, never):cs.system.security.cryptography.KeySizes;
	static var TagByteSizes(default, never):cs.system.security.cryptography.KeySizes;
	@:overload(function(key:cs.NativeArray<cs.UInt8>):Void {})
	function new(key:cs.system.ReadOnlySpan<cs.UInt8>):Void;
	@:overload(function(nonce:cs.NativeArray<cs.UInt8>, ciphertext:cs.NativeArray<cs.UInt8>, tag:cs.NativeArray<cs.UInt8>, plaintext:cs.NativeArray<cs.UInt8>, ?associatedData:cs.NativeArray<cs.UInt8>):Void {})
	function Decrypt(nonce:cs.system.ReadOnlySpan<cs.UInt8>, ciphertext:cs.system.ReadOnlySpan<cs.UInt8>, tag:cs.system.ReadOnlySpan<cs.UInt8>, plaintext:cs.system.Span<cs.UInt8>, ?associatedData:cs.system.ReadOnlySpan<cs.UInt8>):Void;
	function Dispose():Void;
	@:overload(function(nonce:cs.NativeArray<cs.UInt8>, plaintext:cs.NativeArray<cs.UInt8>, ciphertext:cs.NativeArray<cs.UInt8>, tag:cs.NativeArray<cs.UInt8>, ?associatedData:cs.NativeArray<cs.UInt8>):Void {})
	function Encrypt(nonce:cs.system.ReadOnlySpan<cs.UInt8>, plaintext:cs.system.ReadOnlySpan<cs.UInt8>, ciphertext:cs.system.Span<cs.UInt8>, tag:cs.system.Span<cs.UInt8>, ?associatedData:cs.system.ReadOnlySpan<cs.UInt8>):Void;
}
