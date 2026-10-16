package unit.issues;

#if (cpp && !cppia)
@:unreflective
private class Unreflective {
	public static var lastInstance:String;
	public static var lastStatic:String;

	public function new() {}

	public function onInstance(value:String):Void {
		lastInstance = value;
	}

	public static function onStatic(value:String):Void {
		lastStatic = value;
	}
}

private class Dispatcher {
	public static function call(listener:String->Void, value:String):Void {
		listener(value);
	}
}
#end

class Issue12437 extends Test {
	#if (cpp && !cppia)
	function test() {
		Dispatcher.call(Unreflective.onStatic, "static");
		eq("static", Unreflective.lastStatic);

		var u = new Unreflective();
		Dispatcher.call(u.onInstance, "instance");
		eq("instance", Unreflective.lastInstance);
	}
	#end
}
