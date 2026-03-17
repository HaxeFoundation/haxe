package unit.teststd.haxe.rtti;

class TestRtti extends unit.Test {
	public function test() {
		f(haxe.rtti.Rtti.hasRtti(NonRttiClass));
		t(haxe.rtti.Rtti.hasRtti(RttiClass1));
		t(haxe.rtti.Rtti.hasRtti(RttiClass2));

		var cl = haxe.rtti.Rtti.getRtti(RttiClass1);
		f(cl.isExtern);
		f(cl.isInterface);
		eq(cl.params.length, 0);
		eq(cl.fields.length, 1);
		eq(cl.superClass, null);
		eq(cl.interfaces.length, 0);
		eq(cl.fields.length, 1);
		eq(cl.statics.length, 1);
		eq(cl.tdynamic, null);

		var cf = cl.statics.shift();
		eq(cf.name, "v");
		eq(haxe.rtti.CType.CTypeTools.toString(cf.type), "String");
		f(cf.isPublic);
		f(cf.isOverride);
		eq(cf.doc, null);
		eq(cf.get, RNormal);
		eq(cf.set, RNormal);
		eq(cf.params.length, 0);
		eq(cf.platforms.length, 0);
		eq(cf.meta.length, 0);
		eq(cf.line, null);
		eq(cf.overloads, null);

		var cf = cl.fields.shift();
		eq(cf.name, "f");
		eq(haxe.rtti.CType.CTypeTools.toString(cf.type), "Void -> Float");
		t(cf.isPublic);
		f(cf.isOverride);
		eq(cf.doc, null);
		eq(cf.get, RNormal);
		eq(cf.set, RMethod);
		eq(cf.params.length, 0);
		eq(cf.platforms.length, 0);
		eq(cf.meta.length, 0);
		//cf.line == null;
		eq(cf.overloads, null);

		var cl = haxe.rtti.Rtti.getRtti(RttiClass2);
		f(cl.isExtern);
		f(cl.isInterface);
		eq(cl.params.length, 0);
		eq(cl.fields.length, 0);
		eq(cl.superClass.path, "unit.teststd.haxe.rtti._TestRtti.RttiClass1");
		eq(cl.superClass.params.length, 0);
		eq(cl.interfaces.length, 0);
		eq(cl.fields.length, 0);
		eq(cl.statics.length, 0);
		eq(cl.tdynamic, null);

		var cl = haxe.rtti.Rtti.getRtti(RttiClass3);
		f(cl.isExtern);
		f(cl.isInterface);
		eq(cl.params.length, 0);
		eq(cl.fields.length, 1);
		eq(cl.superClass.path, "unit.teststd.haxe.rtti._TestRtti.RttiClass1");
		eq(cl.superClass.params.length, 0);
		eq(cl.interfaces.length, 0);
		eq(cl.fields.length, 1);
		eq(cl.statics.length, 0);
		eq(cl.tdynamic, null);

		var cf = cl.fields.shift();
		eq(cf.name, "f");
		eq(haxe.rtti.CType.CTypeTools.toString(cf.type), "Void -> Int");
		t(cf.isPublic);
		t(cf.isOverride);
		eq(cf.doc, null);
		eq(cf.get, RNormal);
		eq(cf.set, RMethod);
		eq(cf.params.length, 0);
		eq(cf.platforms.length, 0);
		eq(cf.meta.length, 0);
		//cf.line == null;
		eq(cf.overloads, null);
	}
}

private class NonRttiClass {}

@:rtti
@:keepSub
private class RttiClass1 {
	static var v:String;

	public function f() {
		return 33.0;
	}
}

private class RttiClass2 extends RttiClass1 {}

private class RttiClass3 extends RttiClass1 {
	override function f():Int {
		return 33;
	}
}
