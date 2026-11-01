#ifndef TEST_H_INCLUDED
#define TEST_H_INCLUDED
#import <Foundation/Foundation.h>

@protocol TestInterface

- (id <TestInterface>)getSelf;

- (int)getOtherThing;

- (char)getOtherThingChar;

@optional

- (NSString *)someOptionalMethod;

- (NSString *)unimplementedOptional;

@end

@interface TestClass : NSObject <TestInterface> {
	@public int otherThing;
}

@property (retain) NSString *something;

+ (int)aStatic;

+ (BOOL)isNull:(TestClass *)t;

- (void)setOtherThing:(int) value;

- (int)getOtherThing;

- (char)getOtherThingChar;

- (NSString *)addHello:(NSString *)str;

- (NSString *)addHello:(NSString *)str andString:(NSString *) str2;

- (NSString *)addSomething:(NSString *)str;

- (BOOL)isBiggerThan10:(NSNumber *)value;

- (NSNumber *)isBiggerThan10Num:(NSNumber *)value;

- (BOOL)isBiggerThan10Int:(int)integer;

- (TestClass *)getSelf;

- (NSString *)someOptionalMethod;


@end

int some_c_call(TestClass *t);

BOOL is_bigger_than_10(TestClass *t, int val);
#endif
