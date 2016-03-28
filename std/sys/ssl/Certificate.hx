package sys.ssl;

extern class Certificate {
	
	public static function loadFile( file : String ) : Certificate;
	
	public static function loadPath( path : String ) : Certificate;
	
	public static function loadDefaults() : Certificate;

	public var commonName(get,null) : Null<String>;

	public var altNames(get,null) : Array<String>;
	
	public var notBefore(get,null) : Date;
	
	public var notAfter(get,null) : Date;
	
	public function subject( field : String ) : Null<String>;
	
	public function issuer( field : String ) : Null<String>;
	
	public function next() : Null<Certificate>;

}
