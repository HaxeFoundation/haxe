package mtwin;

class DateFormat {
	static var REG_STD = ~/([0-9]{4})-([0-9]{2})-([0-9]{2})/;
	static var REG_A = ~/(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) ([0-9]{1,2}), ([0-9]{2,4})/;
	static var REG_B = ~/([0-9]{1,2}) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) ([0-9]{2,4})/;
	static var REG_A2 = ~/(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) ([0-9]{1,2}) .*? ([0-9]{4})$/;
	static var REG_B2 = ~/([0-9]{1,2}) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) .*? ([0-9]{4})$/;
	static var REG_HOUR = ~/([0-9]{2}):([0-9]{2}):([0-9]{2})/;
	static var REG_TZ = ~/(ADT|BST|NZT|NZST|IDLE|GST|EAST|JST|CCT|WAST|BT|EET|SWT|MEWT|MET|FWT|CET|IDLW|NT|HST|CAT|AHST|YST|AST|AT|WAT|WET|UTC|UT|GMT|EST|EDT|CST|CDT|MST|MDT|PST|PDT|YDT|HDT|MEST|MESZ|SST|FST|WADT|EADT|NZDT|[+-][0-9]{3,4}|[A-Z])$/;
	static var REG_SIMPLE_TZ = ~/[+-][0-9]{3,4}/;
	static var MONTH = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
	static var DAY = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];

	static function month( s : String ) : Int{
		for( i in 0...12 ){
			if( MONTH[i].toLowerCase() == s.toLowerCase() ) return i+1;
		}
		throw "Unknown month: "+s;
	}

	static function timezone( str : String ) : {h: Int,m: Int}{
		if( REG_SIMPLE_TZ.match(str) ){
			var h = Std.parseInt(str.substr(0,-2));
			var m = Std.parseInt(str.substr(-2,2));
			return {h: h,m:m};
		}

		return switch( str ){	
			case "GMT","UT","UTC","WET","Z","BST": {h:0,m:0};
			case "A","WAT": {h:-1,m:0};
			case "B","AT": {h:-2,m:0};
			case "C","ADT": {h:-3,m:0};
			case "D","AST","EDT": {h:-4,m:0};
			case "E","EST","CDT": {h:5,m:0};
			case "F","CST","MDT":  {h:-6,m:0};
			case "G","MST","PDT":  {h:-7,m:0};
			case "H","PST","YDT":  {h:-8,m:0};
			case "I","YST","HDT":  {h:-9,m:0};
			case "K","AHST","CAT","HST":  {h:-10,m:0};
			case "L","NT":  {h:-11,m:0};
			case "M","IDLW":  {h:-12,m:0};
			case "N","CET","FWT","MET","MEWT","SWT":  {h:1,m:0};
			case "O","EET","ZP2","MEST","MESZ","SST","FST":  {h:2,m:0};
			case "P","BT","ZP3":  {h:3,m:0};
			case "Q","ZP4":  {h:4,m:0};
			case "R","ZP5":  {h:5,m:0};
			case "S","ZP6":  {h:6,m:0};
			case "T","WAST","ZP7":  {h:7,m:0};
			case "U","CCT","ZP8":  {h:8,m:0};
			case "V","JST","ZP9":  {h:9,m:0};
			case "W","EAST","GST":  {h:10,m:0}; 
			case "X":  {h:11,m:0};
			case "Y","IDLE","NZST","NST":  {h:12,m:0};	
			default: {h:0,m:0};
		}
	}

	public static function timezoneConvert( d : Date, tz : String, ?ltz: String ) : Date {
		if( ltz == null )
			ltz = DateTools.format(Date.now(),"%z");

		var localtz = timezone( ltz );
		var otz = timezone( tz );

		var diff = (localtz.h-otz.h)*3600 + (localtz.m-otz.m)*60;
		return DateTools.delta(d,diff*1000.0);
	}
	
	public static function parse( str : String ) : Date {
		var h = 0;
		var i = 0;
		var s = 0;

		var y = 1970;
		var m = 1;
		var d = 1;

		if( REG_STD.match(str) ){
			y = Std.parseInt(REG_STD.matched(1));
			m = Std.parseInt(REG_STD.matched(2));
			d = Std.parseInt(REG_STD.matched(3));
		}else if( REG_A.match(str) ){
			y = Std.parseInt(REG_A.matched(3));
			d = Std.parseInt(REG_A.matched(2));
			m = month(REG_A.matched(1));
		}else if( REG_B.match(str) ){
			y = Std.parseInt(REG_B.matched(3));
			d = Std.parseInt(REG_B.matched(1));
			m = month(REG_B.matched(2));
		}else if( REG_A2.match(str) ){
			y = Std.parseInt(REG_A2.matched(3));
			d = Std.parseInt(REG_A2.matched(2));
			m = month(REG_A2.matched(1));
		}else if( REG_B2.match(str) ){
			y = Std.parseInt(REG_B2.matched(3));
			d = Std.parseInt(REG_B2.matched(1));
			m = month(REG_B2.matched(2));
		}else{
			return null;
		}

		if( y < 50 )
			y += 2000;
		else if( y < 100 )
			y += 1900;

		if( REG_HOUR.match(str) ){
			h = Std.parseInt(REG_HOUR.matched(1));
			i = Std.parseInt(REG_HOUR.matched(2));
			s = Std.parseInt(REG_HOUR.matched(3));
		}

		var ret = new Date(y,m-1,d,h,i,s);
		if( REG_TZ.match(str) )
			ret = timezoneConvert(ret,REG_TZ.matched(1));
		return ret;
	}

	public static function formatRfc822( d : Date ) : String {
		var month = MONTH[d.getMonth()];
		var day = DAY[d.getDay()];
		return DateTools.format(d,day+", %d "+month+" %Y %H:%M:%S %z");
	}
}
