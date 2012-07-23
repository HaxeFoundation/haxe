package unit;

class DCEClass {
	// used statics
	static function staticUsed() { }
	@:keep static function staticKeep() { }
	static var staticVarUsed = "foo";
	static var staticPropUsed(get_staticPropUsed, set_staticPropUsed):Int = 1;
	static function get_staticPropUsed() return staticPropUsed
	static function set_staticPropUsed(i:Int) return 0
	
	// used members
	function memberUsed() { }
	@:keep function memberKeep() { }
	var memberVarUsed = 0;
	var memberPropUsed(get_memberPropUsed, set_memberPropUsed):Int = 1;
	function get_memberPropUsed() return memberPropUsed
	function set_memberPropUsed(i:Int) return 0
	
	// unused statics
	static function staticUnused() { }
	static var staticVarUnused = "bar";
	static var staticPropUnused(get_staticPropUnused, set_staticPropUnused):Int = 1;
	static function get_staticPropUnused() return 0
	static function set_staticPropUnused(i:Int) return 0
	
	// unused members
	function memberUnused() { }
	var memberVarUnused = 1;
	var memberPropUnused(get_memberPropUnused, set_memberPropUnused):Int = 1;
	function get_memberPropUnused() return 0
	function set_memberPropUnused(i:Int) return 0	
	
	static var c :Array<Dynamic> = [null, unit.UsedReferenced2];
	
	public function new() {
		staticUsed();
		staticVarUsed;
		staticPropUsed = 1;
		staticPropUsed;
		
		memberUsed();
		memberVarUsed;
		memberPropUsed = 2;
		memberPropUsed;
		
		new UsedConstructed();
		
		try cast (null, UsedReferenced) catch(e:Dynamic) { }
				
		new UsedAsBaseChild();
		c.length;
	}
}

class TestDCE extends Test {
	
	public function testFields() {
		var dce = new DCEClass();
		var c = Type.getClass(dce);
		hf(c, "memberKeep");
		hf(c, "memberUsed");
		hf(c, "memberVarUsed");
		hf(c, "memberPropUsed");
		hf(c, "get_memberPropUsed");
		hf(c, "set_memberPropUsed");
		
		hsf(c, "staticKeep");
		hsf(c, "staticUsed");
		hsf(c, "staticVarUsed");
		hsf(c, "staticPropUsed");
		hsf(c, "get_staticPropUsed");
		hsf(c, "set_staticPropUsed");
		
		nhf(c, "memberUnused");
		nhf(c, "memberVarUnused");
		nhf(c, "memberPropUnused");
		nhf(c, "get_memberPropUnused");
		nhf(c, "set_memberPropUnused");
		
		nhsf(c, "staticUnused");
		nhsf(c, "staticVarUnused");
		nhsf(c, "staticPropUnused");
		nhsf(c, "get_staticPropUnused");
		nhsf(c, "set_staticPropUnused");	
	}
	
	public function testInterface() {
		var l:UsedInterface = new UsedThroughInterface();
		var l2:UsedInterface = new InterfaceMethodFromBaseClassChild();
		var ic = Type.resolveClass("unit.UsedInterface");
		var c = Type.getClass(l);
		var bc = Type.resolveClass("unit.InterfaceMethodFromBaseClass");
		
		l.usedInterfaceFunc();
		hf(ic, "usedInterfaceFunc");
		hf(c, "usedInterfaceFunc");
		hf(bc, "usedInterfaceFunc");
		nhf(ic, "unusedInterfaceFunc");
		nhf(c, "unusedInterfaceFunc");
		nhf(bc, "unusedInterfaceFunc");	
	}
	
	public function testProperty() {
		var l:PropertyInterface = new PropertyAccessorsFromBaseClassChild();
		var ic = Type.resolveClass("unit.PropertyInterface");
		var c = Type.getClass(l);
		var bc = Type.resolveClass("unit.PropertyAccessorsFromBaseClass");
		
		l.x = "bar";
		hf(c, "set_x");
		hf(bc, "set_x");
		nhf(ic, "set_x");
		nhf(ic, "get_x");
		nhf(c, "get_x");
		nhf(bc, "get_x");
	}
	
	public function testClasses() {
		t(Type.resolveClass("unit.UsedConstructed") != null);
		t(Type.resolveClass("unit.UsedReferenced") != null);
		t(Type.resolveClass("unit.UsedReferenced2") != null);
		t(Type.resolveClass("unit.UsedInterface") != null);
		t(Type.resolveClass("unit.UsedThroughInterface") != null);
		t(Type.resolveClass("unit.UsedAsBase") != null);
		t(Type.resolveClass("unit.UsedAsBaseChild") != null);
		
		t(Type.resolveClass("unit.Unused") == null);
		t(Type.resolveClass("unit.UnusedChild") == null);		
		t(Type.resolveClass("unit.UnusedImplements") == null);
		t(Type.resolveClass("unit.UsedConstructedChild") == null);
		t(Type.resolveClass("unit.UsedReferencedChild") == null);	
	}
	
	function hf(c:Class<Dynamic>, n:String, ?pos:haxe.PosInfos) {
		Test.count++;
		if (!Lambda.has(Type.getInstanceFields(c), n))
			Test.report(Type.getClassName(c) + " should have member field " +n, pos);
	}
	
	function nhf(c:Class<Dynamic>, n:String, ?pos:haxe.PosInfos) {
		Test.count++;
		if (Lambda.has(Type.getInstanceFields(c), n))
			Test.report(Type.getClassName(c) + " should not have member field " +n, pos);
	}
	
	function hsf(c:Class<Dynamic> , n:String, ?pos:haxe.PosInfos) {
		Test.count++;
		if (!Lambda.has(Type.getClassFields(c), n))
			Test.report(Type.getClassName(c) + " should have static field " +n, pos);
	}	
	
	function nhsf(c:Class<Dynamic> , n:String, ?pos:haxe.PosInfos) {
		Test.count++;
		if (Lambda.has(Type.getClassFields(c), n))
			Test.report(Type.getClassName(c) + " should not have static field " +n, pos);
	}
}

class UsedConstructed {
	public function new() { }
}

class UsedReferenced { }
class UsedReferenced2 { }

class UsedConstructedChild extends UsedConstructed {
	
}

class UsedReferencedChild extends UsedReferenced {
	
}

interface UsedInterface {
	public function usedInterfaceFunc():Void;
	public function unusedInterfaceFunc():Void;
}

class UsedThroughInterface implements UsedInterface {
	public function new() { }
	public function usedInterfaceFunc():Void { }
	public function unusedInterfaceFunc():Void { }
	public function otherFunc() { }
}

class UsedAsBase { }
class UsedAsBaseChild extends UsedAsBase {
	public function new() { }
}

class Unused {
	
}

class UnusedChild extends Unused { }

class UnusedImplements implements UsedInterface {
	public function usedInterfaceFunc():Void { }
	public function unusedInterfaceFunc():Void { }
}

interface PropertyInterface {
	public var x(get_x, set_x):String;
}

class PropertyAccessorsFromBaseClass {
	public function get_x() return throw "must not set"
	public function set_x(x:String) return "ok"
}

class PropertyAccessorsFromBaseClassChild extends PropertyAccessorsFromBaseClass, implements PropertyInterface {
	public var x(get_x, set_x):String;
	public function new() { }
}

class InterfaceMethodFromBaseClass {
	public function usedInterfaceFunc():Void { }
	public function unusedInterfaceFunc():Void { }
}

class InterfaceMethodFromBaseClassChild extends InterfaceMethodFromBaseClass, implements UsedInterface {
	public function new() { }
}