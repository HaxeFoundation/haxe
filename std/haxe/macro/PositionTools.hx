package haxe.macro;

import haxe.macro.Expr;

class PositionTools {

	/**
		Returns the `Position` where the caller of `here` is.
	**/
	macro public static function here():ExprOf<Position> {
		var positionExpr = Context.makeExpr(Context.getPosInfos(Context.currentPos()), Context.currentPos());
		if (Context.defined("macro")) {
			return macro Context.makePosition($positionExpr);
		} else {
			return positionExpr;
		}
	}

	/**
		Like `Context.getPosInfos`, except this method is available on all platforms.
	**/
	public static function getInfos( p : Position ) : { min : Int, max : Int, file : String } {
		#if macro
		return Context.getPosInfos(p);
		#else
		return p;
		#end
	}

	/**
		Like `Context.makePosition`, except this method is available on all platforms.
	**/
	public static function make( inf : { min : Int, max : Int, file : String } ) : Position {
		#if macro
		return Context.makePosition(inf);
		#else
		return inf;
		#end
	}

}
