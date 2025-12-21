class Main {
	static function main() {
		var a = new TShaderConstant(true);
		trace(a.bool);
	}
}

class TShaderConstant {
	public var bool: Bool;

	public function new(boolValue: Bool) {
		this.bool = boolValue;
	}
}
