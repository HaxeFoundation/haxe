class Neko {

	public static function load( lib : String, prim : String, nargs : Int ) : Dynamic {
		return untyped __dollar__loader.loadprim((lib+"@"+prim).__s,nargs);
	}

}