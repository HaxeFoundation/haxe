using jvm.NativeTools.NativeClassTools;

@:annotation("RUNTIME")
interface Mixin extends java.lang.annotation.Annotation {
	function value():java.NativeArray<Class<Dynamic>>;
	function targets():java.NativeArray<String>;
	function priority():Int;
	function remap():Bool;
}

class B {}

@:strict(Mixin({
	value: [String, B],
	targets: ["here"],
	priority: 9001,
	remap: true
}))
class C {}

class MainMixin {
	static function main() {
		var annot = C.native().getAnnotation(Mixin.native());
		for (v in annot.value()) {
			trace(v);
		}
		for (v in annot.targets()) {
			trace(v);
		}
		trace(annot.priority());
		trace(annot.remap());
	}
}