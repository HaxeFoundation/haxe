// we only specify basic things here, most invalid input is platform-dependent
var date = new Date(1982,10,10,14,2,20);
date.getHours() == 14;
date.getMinutes() == 2;
date.getSeconds() == 20;
date.getFullYear() == 1982;
date.getMonth() == 10;
date.getDate() == 10;
date.getDay() == 3;
//date.getTime() == 405781340000.;
date.toString() == "1982-11-10 14:02:20";

var date = Date.fromTime(date.getTime());
date.getHours() == 14;
date.getMinutes() == 2;
date.getSeconds() == 20;
date.getFullYear() == 1982;
date.getMonth() == 10;
date.getDate() == 10;
date.getDay() == 3;
//date.getTime() == 405781340000.;
date.toString() == "1982-11-10 14:02:20";

var date = Date.fromTime(405781340000);
date.getTime() == 405781340000;

// timezone issues
var date1 = Date.fromTime(1455551955 * 1000.); // 15 Feb 2016 16:59:15 GMT
var date2 = new Date(2016, 1, 15, 16, 59, 15);
date1.getTime() == date2.getTime();

var referenceDate = new Date(1970, 0, 1, 2, 0, 0);
referenceDate.toString() == "1970-01-01 02:00:00";
referenceDate.getTime() == 3600000.;

var date = new Date(1970, 0, 1, 1, 59, 59);
date.getTime() < referenceDate.getTime();

// < 1970 problems
var date = new Date(1904, 11, 12, 1, 4, 1);
date.getFullYear() == 1904;
date.getHours() == 1;

// Y2038 problems
var date = new Date(2039, 0, 1, 1, 59, 59);
date.getFullYear() == 2039;
date.getTime() > referenceDate.getTime();
