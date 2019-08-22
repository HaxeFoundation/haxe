package flash.automation;

@:require(flash10_1) extern class Configuration {
	@:flash.property static var deviceConfiguration(get,set) : String;
	@:flash.property static var testAutomationConfiguration(get,never) : String;
	private static function get_deviceConfiguration() : String;
	private static function get_testAutomationConfiguration() : String;
	private static function set_deviceConfiguration(value : String) : String;
}
