package unit.issues;

class Issue4084 extends Test {
	function test() {
        var x = Xml.parse("<div>Hello!</div>");
        var v = switch x.nodeType {
            case Xml.ProcessingInstruction: 0;
            case Xml.DocType: 1;
            case Xml.Document: 2;
            case Xml.CData: 3;
            case Xml.PCData: 4;
            case Xml.Element: 5;
            case Xml.Comment: 6;
        }
		eq(2, v);
	}
}