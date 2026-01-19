open Type

type continuation_api = {
	base_continuation_class : tclass;
	immediate_suspension_result_class : tclass;
	suspension_state : Type.t;
	suspension_result : t -> t;
	suspension_result_class : tclass;
	continuation : Type.t;
	state : tclass_field;
	result : tclass_field;
	error : tclass_field;
	completion : tclass_field;
	context : tclass_field;
	goto_label : tclass_field;
	recursing : tclass_field;
	immediate_result : texpr -> texpr;
	immediate_error : texpr -> Type.t -> texpr;
	suspended : tclass_field;
}

let create_continuation_api base_continuation_class immediate_suspension_result_class suspension_state suspension_result_class continuation immediate_result immediate_error suspended state result error completion context goto_label recursing = {
	base_continuation_class;
	immediate_suspension_result_class;
	suspension_state;
	suspension_result = (fun t -> TInst(suspension_result_class,[t]));
	suspension_result_class;
	continuation;
	immediate_result;
	immediate_error;
	suspended;
	state;
	result;
	error;
	completion;
	context;
	goto_label;
	recursing;
}