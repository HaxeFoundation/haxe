package haxe.macro;
#if macro
import haxe.macro.Expr;
#end

/**
	Some macro utility methods that can be used on different platforms
**/
#if !macro extern #end
class Tools {

	#if (js || macro)
	/**
		Embed an on-disk javascript file (can be called into an __init__ method)
	**/
	@:macro public static function includeFile( fileName : Expr ) {
		var str = switch( fileName.expr ) {
		case EConst(c):
			switch( c ) {
			case CString(str): str;
			default: null;
			}
		default: null;
		}
		if( str == null ) Context.error("Should be a constant string", fileName.pos);
		var f = try sys.io.File.getContent(Context.resolvePath(str)) catch( e : Dynamic ) Context.error(Std.string(e), fileName.pos);
		var p = Context.currentPos();
		return { expr : EUntyped( { expr : ECall( { expr : EConst(CIdent("__js__")), pos : p }, [ { expr : EConst(CString(f)), pos : p } ]), pos : p } ), pos : p };
	}
	#end

}