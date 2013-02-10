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