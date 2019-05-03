package flash.system;

extern class SecurityDomain {
	@:require(flash11_3) var domainID(get,never) : String;
	private function get_domainID() : String;
	static var currentDomain(get,never) : SecurityDomain;
	private static function get_currentDomain() : SecurityDomain;
}
