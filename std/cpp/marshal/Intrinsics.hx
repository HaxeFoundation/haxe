package cpp.marshal;

extern class Intrinsics {
	@:pure public static function sizeof<T>(obj:Class<T>):cpp.SizeT;
	@:pure public static function alignof<T>(obj:Class<T>):cpp.SizeT;
}
