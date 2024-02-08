package unit.issues;

import utest.Assert.*;

#if jvm
@:meta(java.lang.annotation.Documented())
private class NotMain {
	static public function gather() {
		final main = new NotMain();

		final cls = java.Lib.getNativeType(main);

		final classAnnotations = [];
		final methods = new Map();
		final fieldAnnotations = [];

		for (an in cls.getAnnotations())
			classAnnotations.push(an.toString());

		for (f in cls.getMethods()) {
			final methodAnnotations = [];
			final methodArgAnnotations = new Map();
			for (an in f.getAnnotations())
				methodAnnotations.push(an.toString());

			for (arg in f.getParameters()) {
				var annots = [];
				for (an in arg.getAnnotations()) {
					annots.push(an.toString());
				}
				methodArgAnnotations[arg.getName()] = annots;
			}
			methods[f.getName()] = {methodAnnotations: methodAnnotations, methodArgAnnotations: methodArgAnnotations};
		}

		for (f in cls.getConstructors()) {
			final methodAnnotations = [];
			final methodArgAnnotations = new Map();
			for (an in f.getAnnotations())
				methodAnnotations.push(an.toString());

			for (arg in f.getParameters()) {
				var annots = [];
				for (an in arg.getAnnotations()) {
					annots.push(an.toString());
				}
				methodArgAnnotations[arg.getName()] = annots;
			}
			methods[f.toString()] = {methodAnnotations: methodAnnotations, methodArgAnnotations: methodArgAnnotations};
		}

		for (f in cls.getFields())
			if (f.getName() == 'member') {
				for (an in f.getAnnotations())
					fieldAnnotations.push(an.toString());
			}

		return {
			classAnnotations: classAnnotations,
			methods: methods,
			fieldAnnotations: fieldAnnotations
		}
	}

	@:meta(java.lang.annotation.Documented())
	@:keep
	public overload function new(@:meta(java.lang.annotation.Documented()) arg0:String, @:meta(java.lang.annotation.Documented()) arg1:String) {}

	overload function new() {}

	@:meta(java.lang.annotation.Documented())
	@:keep var member:String;

	@:meta(java.lang.annotation.Documented())
	@:keep
	public function on0(@:meta(java.lang.annotation.Documented()) arg0:String):String {
		return 'foo';
	}

	@:keep
	public function on1(arg0:String, @:meta(java.lang.annotation.Documented()) arg1:String):String {
		return 'foo';
	}

	@:meta(java.lang.annotation.Documented())
	@:keep
	public function onBoth(@:meta(java.lang.annotation.Documented()) arg0:String, @:meta(java.lang.annotation.Documented()) arg1:String):String {
		return 'foo';
	}

	@:meta(java.lang.annotation.Documented())
	@:keep
	public function on0Static(@:meta(java.lang.annotation.Documented()) arg0:String):String {
		return 'foo';
	}

	@:keep
	public function on1Static(arg0:String, @:meta(java.lang.annotation.Documented()) arg1:String):String {
		return 'foo';
	}

	@:meta(java.lang.annotation.Documented())
	@:keep
	public function onBothStatic(@:meta(java.lang.annotation.Documented()) arg0:String, @:meta(java.lang.annotation.Documented()) arg1:String):String {
		return 'foo';
	}
}
#end

class Issue10397 extends unit.Test {
	#if jvm
	function test() {
		var annots = NotMain.gather();
		same([
			"@java.lang.annotation.Documented()",
			"@haxe.jvm.annotation.ClassReflectionInformation(hasSuperClass=false)"
		], annots.classAnnotations);
		same({
			methodAnnotations: ["@java.lang.annotation.Documented()"],
			methodArgAnnotations: ["arg0" => ["@java.lang.annotation.Documented()"]],
		}, annots.methods["on0"]);
		same({
			methodAnnotations: [],
			methodArgAnnotations: ["arg0" => [], "arg1" => ["@java.lang.annotation.Documented()"]],
		}, annots.methods["on1"]);
		same({
			methodAnnotations: ["@java.lang.annotation.Documented()"],
			methodArgAnnotations: [
				"arg0" => ["@java.lang.annotation.Documented()"],
				"arg1" => ["@java.lang.annotation.Documented()"]
			],
		}, annots.methods["onBoth"]);
		same({
			methodAnnotations: ["@java.lang.annotation.Documented()"],
			methodArgAnnotations: ["arg0" => ["@java.lang.annotation.Documented()"]],
		}, annots.methods["on0Static"]);
		same({
			methodAnnotations: [],
			methodArgAnnotations: ["arg0" => [], "arg1" => ["@java.lang.annotation.Documented()"]],
		}, annots.methods["on1Static"]);
		same({
			methodAnnotations: ["@java.lang.annotation.Documented()"],
			methodArgAnnotations: [
				"arg0" => ["@java.lang.annotation.Documented()"],
				"arg1" => ["@java.lang.annotation.Documented()"]
			],
		}, annots.methods["onBothStatic"]);
		same({
			methodAnnotations: ["@java.lang.annotation.Documented()"],
			methodArgAnnotations: [
				"arg0" => ["@java.lang.annotation.Documented()"],
				"arg1" => ["@java.lang.annotation.Documented()"]
			],
		},
			annots.methods["public unit.issues.Issue10397$NotMain(java.lang.String,java.lang.String)"]);
		same(["@java.lang.annotation.Documented()"], annots.fieldAnnotations);
	}
	#end
}
