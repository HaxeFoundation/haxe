package rust;

extern class Tuple3<A, B, C> {
	public var a(default, null):A;
	public var b(default, null):B;
	public var c(default, null):C;
	public function new(a:A, b:B, c:C):Void;
}