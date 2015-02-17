
package python.lib.datetime;


@:pythonImport("datetime", "datetime")
extern class Datetime {

	public function new (year:Int, month:Int, day:Int, hour:Int=0, minute:Int=0, second:Int=0, microsecond:Int=0, tzinfo:Tzinfo=null);

	public static var min : Datetime;
	public static var max : Datetime;
	public static var resolution : Timedelta;

	public var year : Int;
	public var month : Int;
	public var day : Int;
	public var hour : Int;
	public var minute : Int;
	public var second : Int;
	public var microsecond : Int;
	public var tzinfo : Tzinfo;



	public static function today ():Datetime;
	public static function now (?tzinfo:Tzinfo):Datetime;
	public static function utcnow ():Datetime;
	public static function fromtimestamp (timestamp:Float, tzInfo:Tzinfo=null):Datetime;
	public static function utcfromtimestamp (timestamp:Int):Datetime;
	public static function fromordinal (ordinal:Int):Datetime;

	public function strftime (format:String):String;
	public function replace (?year:Int = 1970, ?month:Int = 1, ?day:Int = 1, ?hour:Int = 0, ?minute:Int = 0, ?second:Int, ?microsecond:Int, ?tzinfo:Tzinfo):Datetime;
	/* 0-6 */
	public function weekday():Int;
	/* 1-7 */
	public function isoweekday():Int;

	// python 3.3
	public function timestamp ():Float;
}