package sys.ssl;

@:noDoc @:keep
class Lib {
	static function __init__() : Void{
		ssl_init();
	}
	
	@:hlNative("ssl","ssl_init") static function ssl_init(){};
}
