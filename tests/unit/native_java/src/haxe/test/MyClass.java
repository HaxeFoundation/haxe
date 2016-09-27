package haxe.test;
import java.lang.annotation.*;

public class MyClass
{
	public void normalOverload(String a)
	{

	}

	public void normalOverload(int a)
	{

	}

	public void normalOverload(boolean a)
	{

	}

	public void normalOverload(Object a)
	{

	}

	public int boolTest1(Boolean value)
	{
		if (value == null)
			return 100;
		return value.booleanValue() ? 1 : 0;
	}

	public boolean boolTest1(boolean i)
	{
		return i;
	}

	public int boolTest2(Boolean value)
	{
		if (value == null)
			return 100;
		return value.booleanValue() ? 1 : 0;
	}

	public int intTest2(Integer value)
	{
		if (value == null)
			return 100;
		return value.intValue();
	}

	public int intTest1(Integer value)
	{
		if (value == null)
			return 100;
		return value.intValue();
	}

	public long intTest1(Long value)
	{
		if (value == null)
			return -100L;
		return -value.longValue();
	}

	public long longTest(Long value)
	{
		if (value == null)
			return 100L;
		return value.longValue();
	}

	public void normalOverload(long a)
	{

	}

	public void normalOverload(float a)
	{

	}

	public void normalOverload(double a)
	{

	}

	public void normalOverload(Base a)
	{
		
	}

	static public class InnerName1$
	{
		static public String test()
		{
			return "InnerName1$";
		}
	}

	static public class $InnerName2
	{
		static public String test()
		{
			return "$InnerName2";
		}
	}

	static public class Inner$Name3
	{
		static public String test()
		{
			return "Inner$Name3";
		}
	}

	static public class InnerName4$$
	{
		static public String test()
		{
			return "InnerName4$$";
		}
	}

	static public class $$InnerName5
	{
		static public String test()
		{
			return "$$InnerName5";
		}
	}

	static public class Inner$$Name6
	{
		static public String test()
		{
			return "Inner$$Name6";
		}
	}

	static public class $$Inner$$Name7$$
	{
		static public String test()
		{
			return "$$Inner$$Name7$$";
		}

		static public class $$Inner$$Name8
		{
			static public String test()
			{
				return "$$Inner$$Name8";
			}
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	public @interface MyAnnotation {
		String author();
		int currentRevision() default 1;
		String lastModified() default "N/A";
		TEnum someEnum() default TEnum.TC;
	}

	@Retention(RetentionPolicy.RUNTIME)
	public @interface ParameterLessAnnotation {
	}
}

