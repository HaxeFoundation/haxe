package unit.issues;
import unit.Test;

class Issue2271 extends Test
{
	function test()
	{
		var changed = false;
		do
		{
			changed = true;
		} while(false);
		t(changed);
	}
}
