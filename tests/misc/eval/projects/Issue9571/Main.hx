class Main {
	static function main() {}

	static function f<TA:A>(v:TA) {
		v.defined(); // works
		v.forwarded(); // doesn't work before 4.2
	}
}

@:forward(forwarded)
abstract A({function forwarded():Void;}) {
	public function defined() {}
}
