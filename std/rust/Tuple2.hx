package rust;

extern class Tuple2<A, B> {
	public var a(default, null):A;
	public var b(default, null):B;
	public function new(a:A, b:B):Void;
}