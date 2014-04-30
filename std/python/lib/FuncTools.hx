
package python.lib;


@:import("functools")
extern class FuncTools {

	public static function cmp_to_key<A>(f:A->A->Int):Dynamic;

}