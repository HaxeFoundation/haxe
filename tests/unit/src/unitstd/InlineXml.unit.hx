unit.HelperMacros.pipeMarkupLiteral(<xml></xml>) == "<xml></xml>";
unit.HelperMacros.pipeMarkupLiteral(<xml ></xml>) == "<xml ></xml>";
unit.HelperMacros.pipeMarkupLiteral(<xml > </xml>) == "<xml > </xml>";

// nested
unit.HelperMacros.pipeMarkupLiteral(<xml><xml></xml></xml>) == "<xml><xml></xml></xml>";
unit.HelperMacros.pipeMarkupLiteral(<xml><yml></xml>) == "<xml><yml></xml>";

// self-closing
unit.HelperMacros.pipeMarkupLiteral(<xml/>) == "<xml/>";
unit.HelperMacros.pipeMarkupLiteral(<xml abc />) == "<xml abc />";

// self-closing nested
unit.HelperMacros.pipeMarkupLiteral(<xml><xml /></xml>) == "<xml><xml /></xml>";

// No check for string literal balancing
unit.HelperMacros.pipeMarkupLiteral(<xml a=" </xml>) == "<xml a=\" </xml>";
unit.HelperMacros.pipeMarkupLiteral(<xml a=' </xml>) == "<xml a=' </xml>";

// comments are fine
unit.HelperMacros.pipeMarkupLiteral(<xml a=// </xml>) == "<xml a=// </xml>";
unit.HelperMacros.pipeMarkupLiteral(<xml a=/* </xml>) == "<xml a=/* </xml>";

// regex too
unit.HelperMacros.pipeMarkupLiteral(<xml a=~/ </xml>) == "<xml a=~/ </xml>";

// format
var count = 33;
unit.HelperMacros.pipeMarkupLiteral(<xml>$count + $count = ${count*2}</xml>) == unit.HelperMacros.pipeMarkupLiteral(<xml>33 + 33 = 66</xml>);
unit.HelperMacros.pipeMarkupLiteral(<xml>$count + <xml>$count</xml> = ${count*2}</xml>) == unit.HelperMacros.pipeMarkupLiteral(<xml>33 + <xml>33</xml> = 66</xml>);