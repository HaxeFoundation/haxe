package cases;

class FailExample {
	static public function main() {
		expect(trace(''), [Js => 'js_fail_example', Php => 'php_fail_example']);
	}
}