package lua;
@:enum
abstract LocaleCategory(String){
	var All      = "all";
	var Collate  = "collate";
	var Ctype    = "ctype";
	var Monetary = "monetary";
	var Numeric  = "numeric";
	var Time     = "time";
}
