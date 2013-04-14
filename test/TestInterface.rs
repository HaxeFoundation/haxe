mod HxObject;
mod HxEnum;
pub struct TestInterface<T>;
pub trait<T> TestInterface<T> {
	fn get() : Option<@T>;
}
impl HxObject for Option<@TestInterface> {
}
