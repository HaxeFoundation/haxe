#import <Foundation/Foundation.h>

@interface TestClass : NSObject {
	@public int otherThing;
}

@property (retain) NSString *something;

+ (int)aStatic;

- (void)setOtherThing:(int) value;

- (int)getOtherThing;

- (char)getOtherThingChar;

- (NSString *)addHello:(NSString *)str;

- (NSString *)addHello:(NSString *)str andString:(NSString *) str2;

- (NSString *)addSomething:(NSString *)str;

- (BOOL)isBiggerThan10:(NSNumber *)value;

- (NSNumber *)isBiggerThan10Num:(NSNumber *)value;

- (BOOL)isBiggerThan10Int:(int)integer;


@end

int some_c_call(TestClass *t);

BOOL is_bigger_than_10(TestClass *t, int val);
