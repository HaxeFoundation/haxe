package unit.teststd;

class TestInlineXml extends unit.Test {
	public function test() {
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml></xml>), "<xml></xml>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml ></xml>), "<xml ></xml>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml > </xml>), "<xml > </xml>");

		// nested
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml><xml></xml></xml>), "<xml><xml></xml></xml>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml><yml></xml>), "<xml><yml></xml>");

		// self-closing
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml/>), "<xml/>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml abc />), "<xml abc />");

		// self-closing nested
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml><xml /></xml>), "<xml><xml /></xml>");

		// special chars
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml-xml></xml-xml>), "<xml-xml></xml-xml>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<:xml></:xml>), "<:xml></:xml>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml:xml></xml:xml>), "<xml:xml></xml:xml>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<foo.Bar_barf3-gnieh:blargh></foo.Bar_barf3-gnieh:blargh>), "<foo.Bar_barf3-gnieh:blargh></foo.Bar_barf3-gnieh:blargh>");

		// fragments
		eq(unit.HelperMacros.pipeMarkupLiteral(<></>), "<></>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<>abc</>), "<>abc</>");

		// No check for string literal balancing
		unit.HelperMacros.pipeMarkupLiteral(<xml a=" </xml>) == "<xml a=\" </xml>";
		unit.HelperMacros.pipeMarkupLiteral(<xml a=' </xml>) == "<xml a=' </xml>";

		// comments are fine
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml a=// </xml>), "<xml a=// </xml>");
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml a=/* </xml>), "<xml a=/* </xml>");

		// regex too
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml a=~/ </xml>), "<xml a=~/ </xml>");

		// format
		var count = 33;
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml>$count + $count = ${count*2}</xml>), unit.HelperMacros.pipeMarkupLiteral(<xml>33 + 33 = 66</xml>));
		eq(unit.HelperMacros.pipeMarkupLiteral(<xml>$count + <xml>$count</xml> = ${count*2}</xml>), unit.HelperMacros.pipeMarkupLiteral(<xml>33 + <xml>33</xml> = 66</xml>));

		// dollar
		eq(unit.HelperMacros.pipeMarkupLiteralUnprocessed(<$xml></$xml>), "<$xml></$xml>");

		// uppercase
		eq(unit.HelperMacros.pipeMarkupLiteral(<Xml></Xml>), "<Xml></Xml>");

		// no semicolon at block-level
		eq(unit.HelperMacros.pipeMarkupLiteral({ <Xml></Xml><yml /> }), "<Xml></Xml><yml />");

	}
}
