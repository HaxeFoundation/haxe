<xml></xml> == "<xml></xml>";
<xml ></xml> == "<xml ></xml>";
<xml > </xml> == "<xml > </xml>";

// nested
<xml><xml></xml></xml> == "<xml><xml></xml></xml>";
<xml><yml></xml> == "<xml><yml></xml>";

// self-closing
<xml/> == "<xml/>";
<xml abc /> == "<xml abc />";

// self-closing nested
<xml><xml /></xml> == "<xml><xml /></xml>";

// No check for string literal balancing
<xml a=" </xml> == "<xml a=\" </xml>";
<xml a=' </xml> == "<xml a=' </xml>";

// comments are fine
<xml a=// </xml> == "<xml a=// </xml>";
<xml a=/* </xml> == "<xml a=/* </xml>";

// regex too
<xml a=~/ </xml> == "<xml a=~/ </xml>";

// format
var count = 33;
<xml>$count + $count = ${count*2}</xml> == <xml>33 + 33 = 66</xml>;
<xml>$count + <xml>$count</xml> = ${count*2}</xml> == <xml>33 + <xml>33</xml> = 66</xml>;