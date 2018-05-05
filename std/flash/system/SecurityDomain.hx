package flash.system;

extern class SecurityDomain {
	@:require(flash11_3) var domainID(default,never) : String;
	static var currentDomain(default,never) : SecurityDomain;
}
