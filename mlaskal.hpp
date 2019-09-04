
#ifndef __DU6SEM_H
#define __DU6SEM_H

#include <vector>
#include "literal_storage.hpp"
#include "flat_icblock.hpp"
#include "dutables.hpp"

namespace mlc {
	struct var_decl
	{
		var_decl(const ls_id_index id,const std::uint_least32_t line) : id(id), line(line) {}
		const ls_id_index id;
		const std::uint_least32_t line;
	};
	struct expr_result
	{
		expr_result() = default;
		expr_result(const type_category cat,const icblock_pointer ic_ptr): ic_ptr(ic_ptr),cat(cat){}
		icblock_pointer ic_ptr;
		type_category cat;
	};
	struct comp_var
	{
		comp_var() = default;
		comp_var(const ls_id_index id,symbol_tables * tab): id(id), offset(0),cat(tab->find_symbol(id)->access_typed()->type()->cat()) {}
		comp_var(const ls_id_index id, const stack_address offset, const type_category cat) : id(id), offset(offset), cat(cat) {}
		ls_id_index id;
		stack_address offset;
		type_category cat;
	};
	parameter_list_ptr create_value_params_list(const std::vector<var_decl>& vars, const var_decl& type, symbol_tables * tab);
	parameter_list_ptr create_reference_params_list(const std::vector<var_decl>& vars, const var_decl& type, symbol_tables * tab);
	void create_and_add_fnc(const var_decl& to_add, const parameter_list_ptr parlist, const var_decl& type, symbol_tables * tab);
	void add_variables(const std::vector<var_decl>& vars, const type_pointer type_ptr, symbol_tables * tab);
	type_pointer get_type_pointer(const var_decl& type, symbol_tables * tab);
	field_list_ptr create_and_fill_field_list(const std::vector<var_decl>& vars, const type_pointer type_ptr);
	void create_const_copy(const var_decl& copy, const var_decl& orig, symbol_tables * tab);
	expr_result check_and_create_function_call(const var_decl var, const std::vector<expr_result>& handed, symbol_tables * tab);
	icblock_pointer check_and_create_procedure_call(const var_decl var, const std::vector<expr_result>& handed, symbol_tables * tab);
	icblock_pointer prepare_params_call_instructions(const var_decl var, const std::vector<expr_result>& handed, symbol_tables * tab);
	expr_result get_int(const ls_int_index id);
	expr_result get_real(const ls_real_index id);
	expr_result get_string(const ls_str_index id);
	expr_result get_variable(const var_decl& var, const comp_var& complete, symbol_tables * tab);
	icblock_pointer assignment(const var_decl & var, const comp_var& comp, const expr_result & rhs, symbol_tables * tab);
	expr_result unary_minus_op(const expr_result& oper, const std::uint_least32_t line);
	expr_result binary_oper_signadd_op(const expr_result& left, const expr_result& right,const DUTOKGE_OPER_SIGNADD sign,const std::uint_least32_t line);
	expr_result binary_oper_mul_op(const expr_result& left, const expr_result& right,const DUTOKGE_OPER_MUL sign,const std::uint_least32_t line);
	expr_result binary_oper_rel_op(const expr_result& left, const expr_result& right, const DUTOKGE_OPER_REL sign, const std::uint_least32_t line);
	expr_result binary_eq_op(const expr_result& left, const expr_result& right, const std::uint_least32_t line);
	expr_result binary_or_op(const expr_result& left, const expr_result& right,const std::uint_least32_t line);
	comp_var get_record_field(const std::vector<ls_id_index>& list, symbol_tables * tab);
	icblock_pointer unwrap_record_g(const field_entry tp, const stack_address address, symbol_tables * tab);
	icblock_pointer unwrap_record_l(const field_entry tp, const stack_address address, symbol_tables * tab);
	icblock_pointer assign_record_g(const field_entry tp, const stack_address address, symbol_tables * tab);
	icblock_pointer assign_record_l(const field_entry tp, const stack_address address, symbol_tables * tab);
	icblock_pointer load_from_offset_l(const field_entry tp, const stack_address address, const stack_address from_offset, stack_address& current_offset, symbol_tables * tab);
	icblock_pointer load_from_offset_g(const field_entry tp, const stack_address address, const stack_address from_offset, stack_address& current_offset, symbol_tables * tab);
	icblock_pointer load_on_offset_l(const field_entry tp, const stack_address address, const stack_address from_offset, stack_address& current_offset, symbol_tables * tab);
	icblock_pointer load_on_offset_g(const field_entry tp, const stack_address address, const stack_address from_offset, stack_address& current_offset, symbol_tables * tab);
}

#endif
