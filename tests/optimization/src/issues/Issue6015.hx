package issues;

class Issue6015 {
	@:js('
		var a = null;
		var tmp = a.a();
		String.fromCharCode.apply(null,tmp);
	')
	static public function main() {
		var a:Dynamic = null;
		untyped String.fromCharCode.apply(null, a.a());
	}
}