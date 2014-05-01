
package python.lib;

abstract TermiosSettings(Dynamic) {}

@:pythonImport("termios", ignoreError=true)
extern class Termios {

	public static var TCSADRAIN : Int;
	public static var ECHO : Int;

	public static function tcgetattr (fileNo:Int):TermiosSettings;

	public static function tcsetattr (fileNo:Int, when:Int, settings:TermiosSettings):Void;

}