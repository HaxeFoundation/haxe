package rust;

class Lib {
	@:functionCode('
		return match(v) {
			Some(o) -> o,
			None -> fail!("Value is null!")
		};
	')
	public static function unwrap<T>(v:Null<T>):T {
		return v;
	}
	@:functionCode('
		return Some(v);
	')
	public static function wrap<T>(v:T):Null<T> {
		return null;
	}
}