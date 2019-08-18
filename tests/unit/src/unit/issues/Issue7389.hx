package unit.issues;

private typedef Throwable = Dynamic;

class Issue7389 extends unit.Test {
	static function main () {
		var x = try {
			1;
		} catch (e:Throwable) {
			2;
		};
	}
}