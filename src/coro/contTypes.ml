open Type

type continuation_api = {
	completion : tclass_field;
	context : tclass_field;
	state : tclass_field;
	result : tclass_field;
	error : tclass_field;
	recursing : tclass_field;
}

let create_continuation_api completion context state result error recursing = {
	completion;
	context;
	state;
	result;
	error;
	recursing;
}