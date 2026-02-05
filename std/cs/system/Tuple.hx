package cs.system;

/** Provides static methods for creating tuple objects. */
@:native("System.Tuple")
extern class Tuple {
	@:overload(function<T1>(item1:T1):cs.system.Tuple_1<T1> {})
	@:overload(function<T1, T2>(item1:T1, item2:T2):cs.system.Tuple_2<T1, T2> {})
	@:overload(function<T1, T2, T3>(item1:T1, item2:T2, item3:T3):cs.system.Tuple_3<T1, T2, T3> {})
	@:overload(function<T1, T2, T3, T4>(item1:T1, item2:T2, item3:T3, item4:T4):cs.system.Tuple_4<T1, T2, T3, T4> {})
	@:overload(function<T1, T2, T3, T4, T5>(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5):cs.system.Tuple_5<T1, T2, T3, T4, T5> {})
	@:overload(function<T1, T2, T3, T4, T5, T6>(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5, item6:T6):cs.system.Tuple_6<T1, T2, T3, T4, T5, T6> {})
	@:overload(function<T1, T2, T3, T4, T5, T6, T7>(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5, item6:T6, item7:T7):cs.system.Tuple_7<T1, T2, T3, T4, T5, T6, T7> {})
	/**
	 * Creates a new 1-tuple, or singleton.
	 * @param T1 The type of the only component of the tuple.
	 * @param item1 The value of the only component of the tuple.
	 * @return A tuple whose value is ().
	 */
	static function Create<T1, T2, T3, T4, T5, T6, T7, T8>(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5, item6:T6, item7:T7, item8:T8):cs.system.Tuple_8<T1, T2, T3, T4, T5, T6, T7, cs.system.Tuple_1<T8>>;
}
