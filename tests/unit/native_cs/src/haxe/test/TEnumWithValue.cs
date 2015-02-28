namespace haxe.test
{

public enum TEnumWithValue
{
	TVA = 0x100,TVB = 0x1,TVD = 0x10, TVC = 0x20
}

public enum TEnumWithBigValue : ulong
{
	TBA = 0x1000000000L, 
	TBD = 0x200000000000L, 
	TBB = 0x100000000L, 
	TBC = 0x3000000000000L, 
}

}

