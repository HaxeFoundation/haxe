package;
import cs.system.DateTime;
import haxe.Int64;

@:coreApi class Date
{
	private var date:DateTime;

	/**
		Creates a new date object.
	**/
	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void
	{
		if (day <= 0) day = 1;
		if (year <= 0) year = 1;
		date = new DateTime(year, month + 1, day, hour, min, sec);
	}

	/**
		Returns the timestamp of the date. It's the number of milliseconds
		elapsed since 1st January 1970. It might only have a per-second precision
		depending on the platforms.
	**/
	public inline function getTime() : Float
	{
		return (cast(date.Ticks, Float) / TimeSpan.TicksPerMillisecond);
	}

	/**
		Returns the hours value of the date (0-23 range).
	**/
	public inline function getHours() : Int
	{
		return date.Hour;
	}

	/**
		Returns the minutes value of the date (0-59 range).
	**/
	public inline function getMinutes() : Int
	{
		return date.Minute;
	}

	/**
		Returns the seconds of the date (0-59 range).
	**/
	public inline function getSeconds() : Int
	{
		return date.Second;
	}

	/**
		Returns the full year of the date.
	**/
	public inline function getFullYear() : Int
	{
		return date.Year;
	}

	/**
		Returns the month of the date (0-11 range).
	**/
	public inline function getMonth() : Int
	{
		return date.Month - 1;
	}

	/**
		Returns the day of the date (1-31 range).
	**/
	public inline function getDate() : Int
	{
		return date.Day;
	}

	/**
		Returns the week day of the date (0-6 range).
	**/
	public function getDay() : Int
	{
		var ret = cast(date.DayOfWeek, Int) - 1;
		if (ret == -1)
			ret = 6;
		return ret;
	}

	/**
		Returns a string representation for the Date, by using the
		standard format [YYYY-MM-DD HH:MM:SS]. See [DateTools.format] for
		other formating rules.
	**/
	public function toString():String
	{
		var m = getMonth() + 1;
		var d = getDate();
		var h = getHours();
		var mi = getMinutes();
		var s = getSeconds();
		return (getFullYear())
			+"-"+(if( m < 10 ) "0"+m else ""+m)
			+"-"+(if( d < 10 ) "0"+d else ""+d)
			+" "+(if( h < 10 ) "0"+h else ""+h)
			+":"+(if( mi < 10 ) "0"+mi else ""+mi)
			+":"+(if( s < 10 ) "0"+s else ""+s);
	}

	/**
		Returns a Date representing the current local time.
	**/
	static public function now() : Date
	{
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = DateTime.Now;
		return d;
	}

	/**
		Returns a Date from a timestamp [t] which is the number of
		milliseconds elapsed since 1st January 1970.
	**/
	static public function fromTime( t : Float ) : Date
	{
		var d = new Date(0, 0, 0, 0, 0, 0);
		d.date = new DateTime(cast(t, Int64));
		return d;
	}


	/**
		Returns a Date from a formated string of one of the following formats :
		[YYYY-MM-DD hh:mm:ss] or [YYYY-MM-DD] or [hh:mm:ss]. The first two formats
		are expressed in local time, the third in UTC Epoch.
	**/
	static public function fromString( s : String ) : Date
	{
		switch( s.length )
		{
			case 8: // hh:mm:ss
				var k = s.split(":");
				var d : Date = new Date(1, 1, 1, Std.parseInt(k[0]), Std.parseInt(k[1]), Std.parseInt(k[2]));
				return d;
			case 10: // YYYY-MM-DD
				var k = s.split("-");
				return new Date(Std.parseInt(k[0]),Std.parseInt(k[1]) - 1,Std.parseInt(k[2]),0,0,0);
			case 19: // YYYY-MM-DD hh:mm:ss
				var k = s.split(" ");
				var y = k[0].split("-");
				var t = k[1].split(":");
				return new Date(Std.parseInt(y[0]),Std.parseInt(y[1]) - 1,Std.parseInt(y[2]),Std.parseInt(t[0]),Std.parseInt(t[1]),Std.parseInt(t[2]));
			default:
				throw "Invalid date format : " + s;
		}
	}

	private static function fromNative( d : cs.system.DateTime ) : Date
	{
		var date = new Date(0, 0, 0, 0, 0, 0);
		date.date = d;
		return date;
	}
}