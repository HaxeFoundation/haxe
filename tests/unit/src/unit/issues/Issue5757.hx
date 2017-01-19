package unit.issues;

#if sys
import sys.db.Types;
import sys.db.Object;

class SpodRtti extends Object {
	public var id : SId;
}
#end

class Issue5757 extends unit.Test {
#if sys
	function test() {
		var rtti = haxe.rtti.Meta.getType( SpodRtti ).rtti;
		t(rtti != null);
	}
#end
}