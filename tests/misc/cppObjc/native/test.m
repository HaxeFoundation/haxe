#import "include/test.h"

@implementation TestClass

@synthesize something = _something;

+ (int)aStatic
{
	return 42;
}

+ (BOOL)isNull:(TestClass *)t
{
	return t == nil;
}

- (void)setOtherThing:(int) value
{
	self->otherThing = value;
}

- (int)getOtherThing
{
	return self->otherThing;
}

- (char)getOtherThingChar
{
	return (char) self->otherThing;
}

- (NSString *)addHello:(NSString *)str
{
	return [@"Hello, " stringByAppendingString: str];
}

- (NSString *)addHello:(NSString *)str andString:(NSString *) str2
{
	return [[@"Hello, " stringByAppendingString: str] stringByAppendingString: str2];
}

- (NSString *)addSomething:(NSString *)str
{
	return [str stringByAppendingString: self->_something];
}

- (BOOL)isBiggerThan10:(NSNumber *)value
{
	return [value doubleValue] > 10;
}

- (NSNumber *)isBiggerThan10Num:(NSNumber *)value
{
	return [NSNumber numberWithBool:[value doubleValue] > 10];
}

- (BOOL)isBiggerThan10Int:(int)integer
{
	return integer > 10;
}

- (TestClass *)getSelf
{
	return self;
}

- (NSString *)someOptionalMethod
{
	return @"someOptionalMethod!";
}

@end

int some_c_call(TestClass *t)
{
	return [t getOtherThing] + 10;
}

BOOL is_bigger_than_10(TestClass *t, int val)
{
	return [t isBiggerThan10Int: val];
}
