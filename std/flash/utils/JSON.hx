package flash.utils;

@:native("JSON") @:require(flash11) extern class JSON {
	static function parse(text : String, ?reviver : Dynamic) : Dynamic;
	static function stringify(value : Dynamic, ?replacer : Dynamic, ?space : Dynamic) : String;
}
