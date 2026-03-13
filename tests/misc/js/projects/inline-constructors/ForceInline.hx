class Inl {
	public var i : Int = 0;
	public function new() {}
}

class ForceInline {
	public static function main() {
		var a = inline new Inl();
		trace(a);
	}
}