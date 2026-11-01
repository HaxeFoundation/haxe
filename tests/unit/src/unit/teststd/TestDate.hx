package unit.teststd;

class TestDate extends unit.Test {
	public function test() {
		// we only specify basic things here, most invalid input is platform-dependent
		var date = new Date(1982,10,10,14,2,20);
		eq(date.getHours(), 14);
		eq(date.getMinutes(), 2);
		eq(date.getSeconds(), 20);
		eq(date.getFullYear(), 1982);
		eq(date.getMonth(), 10);
		eq(date.getDate(), 10);
		eq(date.getDay(), 3);
		eq(date.toString(), "1982-11-10 14:02:20");

		var date = Date.fromTime(date.getTime());
		eq(date.getHours(), 14);
		eq(date.getMinutes(), 2);
		eq(date.getSeconds(), 20);
		eq(date.getFullYear(), 1982);
		eq(date.getMonth(), 10);
		eq(date.getDate(), 10);
		eq(date.getDay(), 3);
		eq(date.toString(), "1982-11-10 14:02:20");

		var date = Date.fromTime(405781340000);
		eq(date.getTime(), 405781340000);
		eq(date.getUTCHours(), 13);
		eq(date.getUTCMinutes(), 2);
		eq(date.getUTCSeconds(), 20);
		eq(date.getUTCFullYear(), 1982);
		eq(date.getUTCMonth(), 10);
		eq(date.getUTCDate(), 10);
		eq(date.getUTCDay(), 3);

		// timezone issues
		var date1 = Date.fromTime(1455555555 * 1000.); // 15 Feb 2016 16:59:15 GMT
		var date2 = new Date(2016, 1, 15, 16, 59, 15);
		#if github
		date1.getTime() == date2.getTime(); // depends on GitHub timezone setting!
		#end

		var referenceDate = new Date(1970, 0, 12, 2, 0, 0);
		eq(referenceDate.toString(), "1970-01-12 02:00:00");
		#if github
		referenceDate.getTime() == 957600000.; // depends on GitHub timezone setting!
		#end

		var date = new Date(1970, 0, 12, 1, 59, 59);
		t(date.getTime() < referenceDate.getTime());

		// < 1970 (negative timestamp)
		// neko, cpp, and python only fail on Windows
		// disabled - see #8600
		#if false // !(hl || eval || neko || cpp || python)
		var date = new Date(1904, 11, 12, 1, 4, 1);
		eq(date.getHours(), 1);
		eq(date.getMinutes(), 4);
		eq(date.getSeconds(), 1);
		eq(date.getFullYear(), 1904);
		eq(date.getMonth(), 11);
		eq(date.getDate(), 12);
		eq(date.getDay(), 1);
		t(date.getTime() < referenceDate.getTime());
		#end

		// < 1902 (negative timestamp, outside of signed 32-bit integer range)
		// lua only fails on Mac
		// python only fails on Windows
		// disabled - see #8600
		#if false // !(hl || neko || eval || cpp || lua || python)
		var date = new Date(1888, 0, 1, 15, 4, 2);
		eq(date.getHours(), 15);
		eq(date.getMinutes(), 4);
		eq(date.getSeconds(), 2);
		eq(date.getFullYear(), 1888);
		eq(date.getMonth(), 0);
		eq(date.getDate(), 1);
		eq(date.getDay(), 0);
		t(date.getTime() < referenceDate.getTime());
		#end


		// Y2038 (outside of signed 32-bit integer range)
		// disabled - see #8600
		#if false // !neko
		var date = new Date(2039, 0, 1, 1, 59, 59);
		eq(date.getHours(), 1);
		eq(date.getMinutes(), 59);
		eq(date.getSeconds(), 59);
		eq(date.getFullYear(), 2039);
		eq(date.getMonth(), 0);
		eq(date.getDate(), 1);
		eq(date.getDay(), 6);
		t(date.getTime() > referenceDate.getTime());
		#end

		// Y2112 (outside of unsigned 32-bit integer range)
		// disabled - see #8600
		#if false // !(hl || neko)
		var date = new Date(2112, 0, 1, 1, 59, 59);
		eq(date.getHours(), 1);
		eq(date.getMinutes(), 59);
		eq(date.getSeconds(), 59);
		eq(date.getFullYear(), 2112);
		eq(date.getMonth(), 0);
		eq(date.getDate(), 1);
		eq(date.getDay(), 5);
		t(date.getTime() > referenceDate.getTime());
		#end

		/*
		// fromTime outside the 1970...2038 range (not supported)
		var date = Date.fromTime(-2052910800.0);
		eq(date.getFullYear(), 1904);
		eq(date.getMonth(), 11);
		date.getDate() == 12; // could fail on very large UTC offsets
		var date = Date.fromTime(-2587294800.0);
		eq(date.getFullYear(), 1888);
		eq(date.getMonth(), 0);
		date.getDate() == 5; // could fail on very large UTC offsets
		var date = Date.fromTime(2177838000.0);
		eq(date.getFullYear(), 2039);
		eq(date.getMonth(), 0);
		date.getDate() == 5; // could fail on very large UTC offsets
		var date = Date.fromTime(4481434800.0);
		eq(date.getFullYear(), 2039);
		eq(date.getMonth(), 0);
		date.getDate() == 5; // could fail on very large UTC offsets
		*/

		// weekdays
		eq((new Date(2019, 6, 1, 12, 0, 0)).getDay(), 1);
		eq((new Date(2019, 6, 2, 12, 0, 0)).getDay(), 2);
		eq((new Date(2019, 6, 3, 12, 0, 0)).getDay(), 3);
		eq((new Date(2019, 6, 4, 12, 0, 0)).getDay(), 4);
		eq((new Date(2019, 6, 5, 12, 0, 0)).getDay(), 5);
		eq((new Date(2019, 6, 6, 12, 0, 0)).getDay(), 6);
		eq((new Date(2019, 6, 7, 12, 0, 0)).getDay(), 0);

		// fromString
		var date = Date.fromString("2019-07-08 12:22:00");
		eq(date.getHours(), 12);
		eq(date.getMinutes(), 22);
		eq(date.getSeconds(), 0);
		eq(date.getFullYear(), 2019);
		eq(date.getMonth(), 6);
		eq(date.getDate(), 8);
		eq(date.getDay(), 1);

		var date = Date.fromString("2019-03-02");
		eq(date.getHours(), 0);
		eq(date.getMinutes(), 0);
		eq(date.getSeconds(), 0);
		eq(date.getFullYear(), 2019);
		eq(date.getMonth(), 2);
		eq(date.getDate(), 2);
		eq(date.getDay(), 6);

		// fromString HH:MM:SS should interpret the time as UTC
		#if python
		// disabled on Windows due to https://bugs.python.org/issue37527
		if (Sys.systemName() != "Windows") {
		#end
		var date = Date.fromString("04:05:06");
		t(date.getUTCHours() == 4);
		t(date.getUTCMinutes() == 5);
		t(date.getUTCSeconds() == 6);
		t(date.getUTCFullYear() == 1970);
		t(date.getUTCMonth() == 0);
		t(date.getUTCDate() == 1);
		t(date.getUTCDay() == 4);
		t(date.getTime() == 14706000.);
		#if python
		}
		#end

		// timezone offset
		// see https://en.wikipedia.org/wiki/UTC_offset
		eq(Date.fromString("2015-01-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-02-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-03-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-04-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-05-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-06-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-07-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-08-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-09-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-10-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-11-08 12:22:00").getTimezoneOffset() % 15, 0);
		eq(Date.fromString("2015-12-08 12:22:00").getTimezoneOffset() % 15, 0);

		eq(Date.fromString("2015-06-15 10:00:00").getTimezoneOffset(), Date.fromString("2016-06-15 10:00:00").getTimezoneOffset());

	}
}
