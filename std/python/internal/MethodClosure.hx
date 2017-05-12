package python.internal;


class MethodClosure {
	@:allow(Reflect) var obj:Dynamic;
	@:allow(Reflect) var func:haxe.Constraints.Function;
	public function new (obj:Dynamic, func:haxe.Constraints.Function) {
		this.obj = obj;
		this.func = func;
	}
	@:keep public function __call__ (args:VarArgs<Dynamic>) {
		return this.func(this.obj, args);
	}
}