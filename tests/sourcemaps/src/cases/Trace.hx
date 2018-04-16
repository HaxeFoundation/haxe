package cases;

class Trace {
	static public function main() {
		expect(trace(''), [Js => "console.log", Php => "(Log::$trace)"]);
	}
}