package tests.unit.src.unit.issues;

private typedef Throwable = Dynamic;

class Issue7389 {
	static function main () {
		var x = try {
			1;
		} catch (e:Throwable) {
			2;
		};
	}
}