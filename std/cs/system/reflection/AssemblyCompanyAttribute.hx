package cs.system.reflection;

/** Defines a company name custom attribute for an assembly manifest. */
@:native("System.Reflection.AssemblyCompanyAttribute")
extern class AssemblyCompanyAttribute extends cs.system.Attribute {
	/**
	 * Gets company name information.
	 * @return A string containing the company name.
	 */
	var Company(default, never):String;
	function new(company:String):Void;
}
