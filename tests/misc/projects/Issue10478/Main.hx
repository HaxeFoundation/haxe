class Main {
	static function main() {
		var a = 1;
		var b = 2;
		a = a ?? b;
		a ??= b;

		var a = 1;
		var b:Null<Int> = 2;
		a = a ?? b;
		a ??= b;

		var a:Null<Int> = 1;
		var b = 2;
		a = a ?? b;
		a ??= b;

		var a = 1;
		var b = 2;
		var c = 2;
		a = a ?? b ?? c;

		var a = null;
		a = 1;
		a ??= b;
	}
}
