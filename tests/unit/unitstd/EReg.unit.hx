#if !flash8
var r = ~/a/;
var rg = ~/a/g;
var rg2 = ~/aa/g;
r.match("") == false;
r.match("b") == false;
r.match("a") == true;
r.matched(0) == "a";
r.matchedLeft() == "";
r.matchedRight() == "";
var pos = r.matchedPos();
pos.pos == 0;
pos.len == 1;

r.match("aa") == true;
r.matched(0) == "a";
r.matchedLeft() == "";
r.matchedRight() == "a";
var pos = r.matchedPos();
pos.pos == 0;
pos.len == 1;

rg.match("aa") == true;
rg.matched(0) == "a";
rg.matchedLeft() == "";
rg.matchedRight() == "a";
var pos = rg.matchedPos();
pos.pos == 0;
pos.len == 1;

rg2.match("aa") == true;
rg2.matched(0) == "aa";
rg2.matchedLeft() == "";
rg2.matchedRight() == "";
var pos = rg2.matchedPos();
pos.pos == 0;
pos.len == 2;

rg2.match("AaaBaaC") == true;
rg2.matched(0) == "aa";
rg2.matchedLeft() == "A";
rg2.matchedRight() == "BaaC";
var pos = rg2.matchedPos();
pos.pos == 1;
pos.len == 2;

// split
~/a/.split("") == [""];
~/a/.split("a") == ["",""];
~/a/.split("aa") == ["","a"];
~/a/.split("b") == ["b"];
~/a/.split("ab") == ["","b"];
~/a/.split("ba") == ["b",""];
~/a/.split("aba") == ["","ba"];
~/a/.split("bab") == ["b","b"];
~/a/.split("baba") == ["b","ba"];

// split + g
~/a/g.split("") == [""];
~/a/g.split("a") == ["",""];
~/a/g.split("aa") == ["","",""];
~/a/g.split("b") == ["b"];
~/a/g.split("ab") == ["","b"];
~/a/g.split("ba") == ["b",""];
~/a/g.split("aba") == ["","b",""];
~/a/g.split("bab") == ["b","b"];
~/a/g.split("baba") == ["b","b",""];

// replace
~/a/.replace("", "z") == "";
~/a/.replace("a", "z") == "z";
~/a/.replace("aa", "z") == "za";
~/a/.replace("b", "z") == "b";
~/a/.replace("ab", "z") == "zb";
~/a/.replace("ba", "z") == "bz";
~/a/.replace("aba", "z") == "zba";
~/a/.replace("bab", "z") == "bzb";
~/a/.replace("baba", "z") == "bzba";

// replace + g
~/a/g.replace("", "z") == "";
~/a/g.replace("a", "z") == "z";
~/a/g.replace("aa", "z") == "zz";
~/a/g.replace("b", "z") == "b";
~/a/g.replace("ab", "z") == "zb";
~/a/g.replace("ba", "z") == "bz";
~/a/g.replace("aba", "z") == "zbz";
~/a/g.replace("bab", "z") == "bzb";
~/a/g.replace("baba", "z") == "bzbz";

// replace + $
~/href="(.*?)"/.replace('lead href="foo" trail',"$1") == "lead foo trail";
~/href="(.*?)"/.replace('lead href="foo" trail',"$2") == "lead $2 trail";
~/href="(.*?)"/.replace('href="foo"',"$1") == "foo";
~/href="(.*?)"/.replace('href="foo"',"$2") == "$2";
~/href="(.*?)"/g.replace('lead href="foo" href="bar" trail',"$1") == "lead foo bar trail";
~/href="(.*?)"/g.replace('lead href="foo" href="bar" trail',"$$$1$$") == "lead $foo$ $bar$ trail";
~/href="(.*?)"/g.replace('lead href="foo" href="bar" trail',"$$$2$$") == "lead $$2$ $$2$ trail";
#end