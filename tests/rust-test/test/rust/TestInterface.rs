mod Lib;
mod HxObject;
pub struct TestInterface<T> {
	value: Option<@T>
}
pub trait<T> TestInterface<T> {
	fn get() : Option<@T>;
}
impl HxObject for TestInterface<T> {
}
