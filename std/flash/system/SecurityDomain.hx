package flash.system;

extern class SecurityDomain {
	@:require(flash11_3) var domainID(default,null) : String;
	static var currentDomain(default,null) : SecurityDomain;
}
