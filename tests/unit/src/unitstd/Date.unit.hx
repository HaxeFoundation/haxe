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
date1.getTime() == date2.getTime(); // depends on Azure timezone setting!

var referenceDate = new Date(1970, 0, 1, 2, 0, 0);
referenceDate.toString() == "1970-01-01 02:00:00";
referenceDate.getTime() == 7200000.; // depends on Azure timezone setting!

var date = new Date(1970, 0, 1, 1, 59, 59);
date.getTime() < referenceDate.getTime();

// < 1970
var date = new Date(1904, 11, 12, 1, 4, 1);
date.getHours() == 1;
date.getMinutes() == 4;
date.getSeconds() == 1;
date.getFullYear() == 1904;
date.getMonth() == 11;
date.getDate() == 12;
date.getDay() == 1;
date.getTime() < referenceDate.getTime();

// < 1902
var date = new Date(1888, 0, 1, 15, 4, 2);
date.getHours() == 15;
date.getMinutes() == 4;
date.getSeconds() == 2;
date.getFullYear() == 1888;
date.getMonth() == 0;
date.getDate() == 1;
date.getDay() == 0;
date.getTime() < referenceDate.getTime();

// Y2038
var date = new Date(2039, 0, 1, 1, 59, 59);
date.getHours() == 1;
date.getMinutes() == 59;
date.getSeconds() == 59;
date.getFullYear() == 2039;
date.getMonth() == 0;
date.getDate() == 1;
date.getDay() == 6;
date.getTime() > referenceDate.getTime();

// weekdays
(new Date(2019, 6, 1, 12, 0, 0)).getDay() == 1;
(new Date(2019, 6, 2, 12, 0, 0)).getDay() == 2;
(new Date(2019, 6, 3, 12, 0, 0)).getDay() == 3;
(new Date(2019, 6, 4, 12, 0, 0)).getDay() == 4;
(new Date(2019, 6, 5, 12, 0, 0)).getDay() == 5;
(new Date(2019, 6, 6, 12, 0, 0)).getDay() == 6;
(new Date(2019, 6, 7, 12, 0, 0)).getDay() == 0;
