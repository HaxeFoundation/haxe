package unit.hxcpp_issues;


class Issue189 extends Test {
   dynamic function dynamicWidthDefaults(first=true, second=6, third=10.5) : Float {
      return first ? second : third;
   }
	function test() {
		eq(dynamicWidthDefaults(),6);
		eq(dynamicWidthDefaults(false),10.5);
	}
}

