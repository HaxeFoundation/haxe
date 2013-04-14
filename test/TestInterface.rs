mod TestInterface;
pub struct TestInterface<T>;
pub trait<T> TestInterface<T> {
	fn get() : Option<@T>;
}
impl HxObject for TestInterface<T> {
}
