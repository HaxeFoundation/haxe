package issues;

class Issue6015 {
	@:js('
		var a = null;
		var tmp = a.a();
		_$String_String_$Impl_$.fromCharCode.apply(null,tmp);
	')
	static public function main() {
		var a:Dynamic = null;
		untyped String.fromCharCode.apply(null, a.a());
	}
}