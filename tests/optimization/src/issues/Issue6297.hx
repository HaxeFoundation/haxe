package issues;

class Issue6297 {
	@:js("
		issues_Issue6297.use(a.test == null);
		issues_Issue6297.use(a.test != null);
		issues_Issue6297.use(null == a.test);
		issues_Issue6297.use(null != a.test);
	")
	@:analyzer(no_local_dce)
	static function f(a:{function test():Void;}) {
		use(a.test == null);
		use(a.test != null);
		use(null == a.test);
		use(null != a.test);
	}

	@:pure(false)
	static function use(e:Dynamic) {}
}