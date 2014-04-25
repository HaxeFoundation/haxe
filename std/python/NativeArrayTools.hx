package python;

class NativeArrayTools {

	public static inline function extend <T>(a:Array<T>, x:Array<T>):Void {
		python.Syntax.field(a, "extend")(x);
	}

	public static inline function append <T>(a:Array<T>, x:T):Void {
		python.Syntax.field(a, "append")(x);
	}

	public static inline function contains <T>(a:Array<T>, x:T):Bool {
		return python.Syntax.isIn(x,a);
	}

}