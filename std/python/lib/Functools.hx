
package python.lib;


@:pythonImport("functools")
extern class Functools {

	public static function cmp_to_key<A>(f:A->A->Int):Dynamic;

}