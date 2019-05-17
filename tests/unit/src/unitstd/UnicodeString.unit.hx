var s = new UnicodeString("𠜎zя");
var codes = [132878, 122, 1103];

// length
s.length == codes.length;

// charAt
s.charAt(0) == "𠜎";
s.charAt(1) == "z";
s.charAt(2) == "я";
s.charAt(3) == "";
s.charAt( -1) == "";
("":UnicodeString).charAt(0) == "";
("":UnicodeString).charAt(1) == "";
("":UnicodeString).charAt( -1) == "";

// charCodeAt
s.charCodeAt(0) == codes[0];
s.charCodeAt(1) == codes[1];
s.charCodeAt(2) == codes[2];
s.charCodeAt(3) == null;
s.charCodeAt(-1) == null;

// @:op(UnicodeString)
var s2 = new UnicodeString("𠜎z");
s != s2;
!(s == s2);
s > s2;
s >= s2;
s2 < s;
s2 <= s;
(s + s2).length == s.length + s2.length;
var s3 = s;
(s3 += s2).length == s.length + s2.length;

// @:op(String)
var s2 = "abя";
s != s2;
!(s == s2);
s > s2;
s >= s2;
s2 < s;
s2 <= s;
(s + s2).length == s.length + (s2:UnicodeString).length;
var s3 = s;
(s3 += s2).length == s.length + (s2:UnicodeString).length;

// iterator
aeq(codes, [for(c in s) c]);

// keyValueIterator
var keys = [for(i in 0...codes.length) i];
var actualKeyCodes = [for(i => c in s) [i, c]];
aeq(keys, actualKeyCodes.map(a -> a[0]));
aeq(codes, actualKeyCodes.map(a -> a[1]));