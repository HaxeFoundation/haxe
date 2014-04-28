
package python.lib.json;

extern class JSONEncoder {

	static function __init__ ():Void {
		python.Syntax.importFromAs("json", "JSONEncoder", "python.lib.json.JSONEncoder");
	}
}