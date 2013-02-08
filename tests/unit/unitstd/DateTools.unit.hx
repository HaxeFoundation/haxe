// leap year
var d = new Date(2012, 01, 17, 01, 02, 03);
DateTools.getMonthDays(d) == 29;

// seconds/delta
var diff = DateTools.seconds(59);
var d2 = DateTools.delta(d, diff);
d2.toString() == "2012-02-17 01:03:02";