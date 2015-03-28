package unit.issues;
import unit.Test;

class Issue4094 extends Test {
    function test() {
        var div = Xml.parse("<div></div>").firstElement();
        var nodes = Xml.parse("Text1<!--Comment--><span>Span</span><div>Div</div>Text2");

        // Test insertChild() moves the nodes correctly.
        var text1 = nodes.firstChild();
        var span = nodes.firstElement();

        div.insertChild( text1, 0 );
        div.insertChild( span, 1 );

        eq( div.toString(), '<div>Text1<span>Span</span></div>' );
        eq( nodes.toString(), '<!--Comment--><div>Div</div>Text2' );
        eq( text1.parent, div );
        eq( span.parent, div );

        // Test removeChild() removes the nodes correctly
        var divText = nodes.firstElement().firstChild();

        nodes.firstElement().removeChild( divText );
        eq( divText.parent, null );
        eq( nodes.toString(), '<!--Comment--><div/>Text2' );

        // Test addChild() moves the nodes correctly.
        var comment = nodes.firstChild();
        var innerDiv = nodes.firstElement();

        div.addChild( comment );
        div.addChild( innerDiv );
        div.addChild( divText );

        eq( div.toString(), '<div>Text1<span>Span</span><!--Comment--><div/>Div</div>' );
        eq( nodes.toString(), 'Text2' );
        eq( comment.parent, div );
        eq( innerDiv.parent, div );
        eq( divText.parent, div );

        // Test addChild() moves a current child to the end.

        div.addChild( span );
        eq( div.toString(), '<div>Text1<!--Comment--><div/>Div<span>Span</span></div>' );
    }
}
