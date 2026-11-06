extern class Ext {
	function new():Void;
	function meth():Int;
	static function func():Void;
	static inline function inlineFunc():Int return 1;
	static var field:Int;
}

#if super_call
class Sub extends Ext {
	public var x = 42;

	public function new() {
		super();
	}
}
#end

class Main {
	static function main() {
		#if inline_closure
		var f = Ext.inlineFunc;
		Sys.println(f());
		#elseif instance
		var e:Ext = null;
		Sys.println(e.meth());
		#elseif super_call
		Sys.println(new Sub().x);
		#elseif static_var
		trace(Ext.field);
		#else
		Ext.func();
		#end
	}
}
