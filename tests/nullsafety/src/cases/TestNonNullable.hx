package cases;

import Validator.shouldWarn;

typedef Data = {
	var foo:String;
}

@:nullSafety(Loose, [WarnNonNullable])
class Main {
	static function main() {
		final foo = 0;
		if (shouldWarn(foo) == null) {}

		final dyn:Dynamic = null;
		if (dyn == null) {}

		final dyn:Any = 1;
		if (shouldWarn(dyn) == null) {}

		var data:Data = haxe.Json.parse("{}");
		data.foo.length;

		switch shouldWarn(data.foo) {
			case null if (shouldWarn(data.foo) == null):
				final v = shouldWarn(data.foo) == null;
		}

		final v = shouldWarn(data.foo) == null;
		shouldWarn(data.foo) != null && true;
		true && shouldWarn(data.foo) != null;
		data.foo != null || true;
		true || shouldWarn(data.foo) != null;

		throw shouldWarn(data.foo) == null;

		function foo():Bool {
			return shouldWarn(data.foo) == null;
		}

		while (shouldWarn(data.foo) == null) {}

		shouldWarn(data.foo) ??= "";
		final foo = shouldWarn(data.foo ?? "");
		if (null == shouldWarn(data.foo)) {
			trace(1);
		}
		if (shouldWarn(data.foo) == null) {
			data.foo = "default";
		}
	}
}
