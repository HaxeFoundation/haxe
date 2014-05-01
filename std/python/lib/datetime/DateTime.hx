
package python.lib.datetime;


@:pythonImport("datetime", "datetime")
extern class DateTime {

	public function new (year:Int, month:Int, day:Int, hour:Int=0, minute:Int=0, second:Int=0, microsecond:Int=0, tzinfo:TzInfo=null);

	public static var min : DateTime;
	public static var max : DateTime;
	public static var resolution : TimeDelta;

	public var year : Int;
	public var month : Int;
	public var day : Int;
	public var hour : Int;
	public var minute : Int;
	public var second : Int;
	public var microsecond : Int;
	public var tzinfo : TzInfo;



	public static function today ():DateTime;
	public static function now (?tzinfo:TzInfo):DateTime;
	public static function utcnow ():DateTime;
	public static function fromtimestamp (timestamp:Float, tzInfo:TzInfo=null):DateTime;
	public static function utcfromtimestamp (timestamp:Int):DateTime;
	public static function fromordinal (ordinal:Int):DateTime;

	public function strftime (format:String):String;
	public function replace (?year:Int = 1970, ?month:Int = 1, ?day:Int = 1, ?hour:Int = 0, ?minute:Int = 0, ?second:Int, ?microsecond:Int, ?tzinfo:TzInfo):DateTime;
	/* 0-6 */
	public function weekday():Int;
	/* 1-7 */
	public function isoweekday():Int;

	// python 3.3
	public function timestamp ():Float;
}