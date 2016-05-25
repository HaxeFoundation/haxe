package unit.issues;

/**
 * Tests if String's indexOf/lastIndexOf functions behave ECMAScript compliant
 */
class Issue5271 extends unit.Test {

    public function testIndexOf() {
        assertEquals( 0, ""      .indexOf(""));
        assertEquals( 0, ""      .indexOf("", 0));
        assertEquals( 0, ""      .indexOf("", 1));
        assertEquals( 0, ""      .indexOf("", -1));

        assertEquals( 0, " "     .indexOf(""));
        assertEquals( 0, " "     .indexOf("", 0));
        assertEquals( 1, " "     .indexOf("", 1));
        assertEquals( 1, " "     .indexOf("", 2));
        assertEquals( 0, " "     .indexOf("", -1));
        
        assertEquals( 0, "dog"   .indexOf(""));
        assertEquals( 0, "dog"   .indexOf("", 0));
        assertEquals( 1, "dog"   .indexOf("", 1));
        assertEquals( 2, "dog"   .indexOf("", 2));
        assertEquals( 3, "dog"   .indexOf("", 3));
        assertEquals( 3, "dog"   .indexOf("", 4));
        assertEquals( 3, "dog"   .indexOf("", 10));
        assertEquals( 0, "dog"   .indexOf("", -1));
        
        assertEquals(-1, "dogdog".indexOf("cat"));
        assertEquals( 3, "dogcat".indexOf("cat"));
        assertEquals( 3, "dogcat".indexOf("cat", 0));
        assertEquals( 3, "dogcat".indexOf("cat", 1));
        assertEquals( 3, "catcat".indexOf("cat", 3));
        assertEquals(-1, "catcat".indexOf("cat", 4));
    }

    public function testLastIndexOf() {
        assertEquals( 0, ""      .lastIndexOf(""));
        assertEquals( 0, ""      .lastIndexOf("", 0));
        assertEquals( 0, ""      .lastIndexOf("", 1));
        assertEquals( 0, ""      .lastIndexOf("", -1));
        
        assertEquals( 1, " "     .lastIndexOf(""));
        assertEquals( 0, " "     .lastIndexOf("", 0));
        assertEquals( 1, " "     .lastIndexOf("", 1));
        assertEquals( 1, " "     .lastIndexOf("", 2));
        assertEquals( 0, " "     .lastIndexOf("", -1));
        
        assertEquals( 3, "dog"   .lastIndexOf(""));
        assertEquals( 0, "dog"   .lastIndexOf("", 0));
        assertEquals( 1, "dog"   .lastIndexOf("", 1));
        assertEquals( 2, "dog"   .lastIndexOf("", 2));
        assertEquals( 3, "dog"   .lastIndexOf("", 3));
        assertEquals( 3, "dog"   .lastIndexOf("", 4));
        assertEquals( 3, "dog"   .lastIndexOf("", 10));
        assertEquals( 0, "dog"   .lastIndexOf("", -1));
        
        assertEquals(-1, "dogdog".lastIndexOf("cat"));
        assertEquals( 3, "dogcat".lastIndexOf("cat"));
        assertEquals(-1, "dogcat".lastIndexOf("cat", 0));
        assertEquals(-1, "dogcat".lastIndexOf("cat", 1));
        assertEquals( 3, "catcat".lastIndexOf("cat", 3));
        assertEquals( 3, "catcat".lastIndexOf("cat", 4));
    }
}
