package haxe.test;
import java.lang.annotation.*;

public interface MyDefaultInterface
{
	default public String test(String a)
	{
		return a.toUpperCase();
	}
}

class MyDefaultClass implements MyDefaultInterface {}