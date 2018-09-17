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