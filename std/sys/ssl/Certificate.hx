package sys.ssl;

extern class Certificate {
	
	public static function loadFile( file : String ) : Certificate;
	
	public static function loadPath( path : String ) : Certificate;

	public static function fromString( str : String ) : Certificate;
	
	public static function loadDefaults() : Certificate;

	public var commonName(get,null) : Null<String>;

	public var altNames(get,null) : Array<String>;
	
	public var notBefore(get,null) : Date;
	
	public var notAfter(get,null) : Date;
	
	public function subject( field : String ) : Null<String>;
	
	public function issuer( field : String ) : Null<String>;
	
	public function next() : Null<Certificate>;

	public function add( pem : String ) : Void;

	public function addDER( der : haxe.io.Bytes ) : Void;

	private function get_commonName() : Null<String>;

	private function get_altNames() : Array<String>;

	private function get_notBefore() : Date;

	private function get_notAfter() : Date;

}
