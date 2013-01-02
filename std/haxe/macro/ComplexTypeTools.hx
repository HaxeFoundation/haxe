package haxe.macro;

import haxe.macro.Expr;

/**
	This class provides some utility methods to work with AST-level types. It is
	best used through 'using haxe.macro.ComplexType' syntax and then provides
	additional methods on haxe.macro.Expr.ComplexType instances.
**/
class ComplexTypeTools {
	
	/**
		Converts type [c] to a human-readable String representation.
		
		The result is guaranteed to be valid haxe code, but there may be
		differences from the original lexical syntax.
	**/	
	static public function toString( c : ComplexType ) : String
		return new Printer().printComplexType(c)
		
	#if macro
	
	/**
		Returns a type corresponding to [c].
		
		If [c] is null, the result is null.
	**/
	static public function toType( c : ComplexType ) : Null<Type>
		return c == null ? null : haxe.macro.Context.typeof( { expr: ECheckType(macro null, c), pos: Context.currentPos() } )
		
	#end
}