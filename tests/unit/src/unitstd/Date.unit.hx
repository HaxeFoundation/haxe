// we only specify basic things here, most invalid input is platform-dependent
var date = new Date(1982,10,10,14,2,20);
date.getHours() == 14;
date.getMinutes() == 2;
date.getSeconds() == 20;
date.getFullYear() == 1982;
date.getMonth() == 10;
date.getDate() == 10;
date.getDay() == 3;
date.toString() == "1982-11-10 14:02:20";

var date = Date.fromTime(date.getTime());
date.getHours() == 14;
date.getMinutes() == 2;
date.getSeconds() == 20;
date.getFullYear() == 1982;
date.getMonth() == 10;
date.getDate() == 10;
date.getDay() == 3;
date.toString() == "1982-11-10 14:02:20";

var date = Date.fromTime(405781340000);
date.getTime() == 405781340000;

// timezone issues
var date1 = Date.fromTime(1455555555 * 1000.); // 15 Feb 2016 16:59:15 GMT
var date2 = new Date(2016, 1, 15, 16, 59, 15);
#if azure
date1.getTime() == date2.getTime(); // depends on Azure timezone setting!
#end

var referenceDate = new Date(1970, 0, 1, 2, 0, 0);
referenceDate.toString() == "1970-01-01 02:00:00";
#if azure
referenceDate.getTime() == 7200000.; // depends on Azure timezone setting!
#end

var date = new Date(1970, 0, 1, 1, 59, 59);
date.getTime() < referenceDate.getTime();

// < 1970 (negative timestamp)
#if !(hl || eval)
var date = new Date(1904, 11, 12, 1, 4, 1);
date.getHours() == 1;
date.getMinutes() == 4;
date.getSeconds() == 1;
date.getFullYear() == 1904;
date.getMonth() == 11;
date.getDate() == 12;
date.getDay() == 1;
date.getTime() < referenceDate.getTime();
#end

// < 1902 (negative timestamp, outside of signed 32-bit integer range)
#if !(hl || neko || eval || cpp)
var date = new Date(1888, 0, 1, 15, 4, 2);
date.getHours() == 15;
date.getMinutes() == 4;
date.getSeconds() == 2;
date.getFullYear() == 1888;
date.getMonth() == 0;
date.getDate() == 1;
date.getDay() == 0;
date.getTime() < referenceDate.getTime();
#end

// Y2038 (outside of signed 32-bit integer range)
#if !neko
var date = new Date(2039, 0, 1, 1, 59, 59);
date.getHours() == 1;
date.getMinutes() == 59;
date.getSeconds() == 59;
date.getFullYear() == 2039;
date.getMonth() == 0;
date.getDate() == 1;
date.getDay() == 6;
date.getTime() > referenceDate.getTime();
#end

// Y2112 (outside of unsigned 32-bit integer range)
#if !(hl || neko)
var date = new Date(2112, 0, 1, 1, 59, 59);
date.getHours() == 1;
date.getMinutes() == 59;
date.getSeconds() == 59;
date.getFullYear() == 2112;
date.getMonth() == 0;
date.getDate() == 1;
date.getDay() == 5;
date.getTime() > referenceDate.getTime();
#end

/*
// fromTime outside the 1970...2038 range (not supported)
var date = Date.fromTime(-2052910800.0);
date.getFullYear() == 1904;
date.getMonth() == 11;
date.getDate() == 12; // could fail on very large UTC offsets
var date = Date.fromTime(-2587294800.0);
date.getFullYear() == 1888;
date.getMonth() == 0;
date.getDate() == 5; // could fail on very large UTC offsets
var date = Date.fromTime(2177838000.0);
date.getFullYear() == 2039;
date.getMonth() == 0;
date.getDate() == 5; // could fail on very large UTC offsets
var date = Date.fromTime(4481434800.0);
date.getFullYear() == 2039;
date.getMonth() == 0;
date.getDate() == 5; // could fail on very large UTC offsets
*/

// weekdays
(new Date(2019, 6, 1, 12, 0, 0)).getDay() == 1;
(new Date(2019, 6, 2, 12, 0, 0)).getDay() == 2;
(new Date(2019, 6, 3, 12, 0, 0)).getDay() == 3;
(new Date(2019, 6, 4, 12, 0, 0)).getDay() == 4;
(new Date(2019, 6, 5, 12, 0, 0)).getDay() == 5;
(new Date(2019, 6, 6, 12, 0, 0)).getDay() == 6;
(new Date(2019, 6, 7, 12, 0, 0)).getDay() == 0;
