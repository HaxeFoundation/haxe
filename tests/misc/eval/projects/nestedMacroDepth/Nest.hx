import haxe.macro.Context;

class Nest {
	// a recursion of depth n, then the typing of the class next (its build macro runs nested in this one)
	static function rec( n : Int, next : String ) : Int {
		if( n == 0 ) {
			// the fields of next : its build macro runs now, nested in this one
			if( next != null ) switch( Context.getType(next) ) { case TInst(c, _): c.get().fields.get(); default: }
			return 0;
		}
		return 1 + rec(n - 1, next);
	}

	public static function build( depth : Int, ?next : String ) : Array<haxe.macro.Expr.Field> {
		rec(depth, next);
		return null;
	}
}
