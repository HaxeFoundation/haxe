// leap year
var d = new Date(2012, 1, 17, 1, 2, 3);
DateTools.getMonthDays(d) == 29;

DateTools.format(d, "%d") == "17";      // day
DateTools.format(d, "%a") == "Fri";      // abbreviated day name
DateTools.format(d, "%w") == "5";        // weekday
DateTools.format(d, "%b") == "Feb";      // abbreviated month name
DateTools.format(d, "%B") == "February"; // full month name
DateTools.format(d, "%y") == "12";      // year without century
DateTools.format(d, "%Y") == "2012";    // year
DateTools.format(d, "%-j") == "48";      // day of the year (no padding)

// seconds/delta
var diff = DateTools.seconds(59);
var d2 = DateTools.delta(d, diff);
d2.toString() == "2012-02-17 01:03:02";
DateTools.format(d2, "%F %T") == "2012-02-17 01:03:02";

d = new Date(2004, 4, 3, 21, 50, 39);

DateTools.format(d, "%-d") == "3";     // day (no padding)
DateTools.format(d, "%A") == "Monday"; // day name
DateTools.format(d, "%w") == "1";      // weekday
DateTools.format(d, "%m") == "05";     // month
DateTools.format(d, "%-m") == "5";     // month (no padding)
DateTools.format(d, "%-y") == "4";     // year without century (no padding)
DateTools.format(d, "%Y") == "2004";   // year
DateTools.format(d, "%j") == "124";    // day of the year

//UTC based timestamp generation
#if (js || flash || php || cpp || python)
DateTools.makeUtc(1982, 10, 10, 14, 2, 20) == 405784940000.;
#end