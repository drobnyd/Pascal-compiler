
#include "du5sem.hpp"
#include "duerr.hpp"
#include "gen_ainstr.hpp"

namespace mlc {
	parameter_list_ptr create_value_params_list(const std::vector<var_decl>& vars, const var_decl& type, symbol_tables * tab)
	{
		auto params = create_parameter_list();
		const type_pointer type_pointer = get_type_pointer(var_decl(type.id, type.line), tab);
		for (size_t i = 0; i < vars.size(); i++)
		{
			params->append_parameter_by_value(vars[vars.size() - 1 - i].id, type_pointer);
		}
		return params;
	}

	parameter_list_ptr create_reference_params_list(const std::vector<var_decl>& vars, const var_decl& type, symbol_tables * tab)
	{
		auto params = create_parameter_list();
		const type_pointer type_pointer = get_type_pointer(var_decl(type.id, type.line), tab);
		for (size_t i = 0; i < vars.size(); i++)
		{
			params->append_parameter_by_reference(vars[vars.size() - 1 - i].id, type_pointer);
		}
		return params;
	}

	void create_and_add_fnc(const var_decl& to_add, const parameter_list_ptr parlist, const var_decl& type, symbol_tables * tab)
	{
		const type_pointer type_pointer = get_type_pointer(var_decl(type.id, type.line), tab);
		tab->add_fnc(to_add.line, to_add.id, type_pointer, parlist);
	}

	void add_variables(const std::vector<var_decl>& vars, const type_pointer type_ptr, symbol_tables * tab)
	{
		for (size_t i = 0; i < vars.size(); i++)
		{
			tab->add_var(vars[i].line, vars[i].id, type_ptr);
		}
	}


	field_list_ptr create_and_fill_field_list(const std::vector<var_decl>& vars, const type_pointer type_ptr)
	{
		field_list_ptr result_field_list = create_field_list();
		for (size_t i = 0; i < vars.size(); i++)
		{
			result_field_list->append_field(vars[vars.size() - 1 - i].id, type_ptr);
		}
		return result_field_list;
	}

	void create_const_copy(const var_decl& copy, const var_decl& orig, symbol_tables * tab) {
		symbol_pointer sp = tab->find_symbol(orig.id);
		if (sp->kind() != SKIND_CONST) { message(DUERR_NOTCONST, orig.line, *orig.id); }
		ls_int_index  val_int;
		ls_real_index val_real;
		ls_str_index val_str;
		bool val_bool;
		switch (sp->access_const()->type()->cat())
		{
		case TCAT_INT:
			val_int = sp->access_const()->access_int_const()->int_value();
			tab->add_const_int(copy.line, copy.id, val_int);
			break;
		case TCAT_REAL:
			val_real = sp->access_const()->access_real_const()->real_value();
			tab->add_const_real(copy.line, copy.id, val_real);
			break;
		case TCAT_STR:
			val_str = sp->access_const()->access_str_const()->str_value();
			tab->add_const_str(copy.line, copy.id, val_str);
			break;
		case TCAT_BOOL:
			val_bool = sp->access_const()->access_bool_const()->bool_value();
			tab->add_const_bool(copy.line, copy.id, val_bool);
			break;
		default:
			message(DUERR_NOTCONST, orig.line, *orig.id);
			break;
		}
	}

	expr_result check_and_create_function_call(const var_decl var, const std::vector<expr_result>& handed, symbol_tables * tab)
	{
		if (tab->find_symbol(var.id)->kind() != SKIND_FUNCTION) message(DUERR_CANNOTCONVERT, var.line);
		auto const cat = tab->find_symbol(var.id)->access_typed()->type()->cat();
		auto ptr = icblock_create();
		switch (cat)
		{
		case TCAT_INT:
			ptr->append<ai::INITI>();
			break;
		case TCAT_REAL:
			ptr->append<ai::INITR>();
			break;
		case TCAT_STR:
			ptr->append<ai::INITS>();
			break;
		case TCAT_BOOL:
			ptr->append<ai::INITB>();
			break;
		default:
			message(DUERR_CONVERSION, var.line);
			break;
		}
		ptr = icblock_merge_and_kill(ptr, prepare_params_call_instructions(var, handed, tab));
		return expr_result(cat, ptr);
	}

	icblock_pointer check_and_create_procedure_call(const var_decl var, const std::vector<expr_result>& handed, symbol_tables * tab)
	{
		if (tab->find_symbol(var.id)->kind() != SKIND_PROCEDURE) message(DUERR_CANNOTCONVERT, var.line);
		return std::move(prepare_params_call_instructions(var, handed, tab));
	}

	icblock_pointer prepare_params_call_instructions(const var_decl var, const std::vector<expr_result>& handed, symbol_tables * tab)
	{
		auto ptr = icblock_create();
		auto clean_up_ptr = icblock_create();
		auto const orig = tab->find_symbol(var.id)->access_subprogram();
		if (orig->parameters()->size() != handed.size()) message(DUERR_PARNUM, var.line);
		auto in = handed.begin();
		for (auto it = orig->parameters()->begin(); it != orig->parameters()->end(); ++it)
		{
			auto cl_up_instr = icblock_create();
			auto const gotten = *in;
			switch (it->ltype->cat())
			{
			case TCAT_REAL:
				cl_up_instr->append<ai::DTORR>();
				clean_up_ptr = std::move(icblock_merge_and_kill(cl_up_instr, clean_up_ptr));
				if (gotten.cat == TCAT_REAL)
				{
					ptr = std::move(icblock_merge_and_kill(ptr, gotten.ic_ptr));
				}
				else if (gotten.cat == TCAT_INT)
				{
					ptr = icblock_merge_and_kill(ptr, gotten.ic_ptr);
					ptr->append<ai::CVRTIR>();
				}
				else { message(DUERR_CANNOTCONVERT, var.line); return nullptr; }
				break;
			case TCAT_INT:
				cl_up_instr->append<ai::DTORI>();
				clean_up_ptr = std::move(icblock_merge_and_kill(cl_up_instr, clean_up_ptr));
				if (gotten.cat == TCAT_REAL)
				{
					ptr = icblock_merge_and_kill(ptr, gotten.ic_ptr);
					message(DUERR_CONVERSION, var.line);
					ptr->append<ai::CVRTRI>();
				}
				else if (gotten.cat == TCAT_INT)
				{
					ptr = std::move(icblock_merge_and_kill(ptr, gotten.ic_ptr));
				}
				else { message(DUERR_CANNOTCONVERT, var.line); return nullptr; }
				break;
			case TCAT_STR:
				cl_up_instr->append<ai::DTORS>();
				clean_up_ptr = std::move(icblock_merge_and_kill(cl_up_instr, clean_up_ptr));
				if (gotten.cat == TCAT_STR)
				{
					ptr = std::move(icblock_merge_and_kill(ptr, gotten.ic_ptr));
				}
				else { message(DUERR_CANNOTCONVERT, var.line); return nullptr; }
				break;
			case TCAT_BOOL:
				cl_up_instr->append<ai::DTORB>();
				clean_up_ptr = std::move(icblock_merge_and_kill(cl_up_instr, clean_up_ptr));
				if (gotten.cat == TCAT_BOOL)
				{
					ptr = std::move(icblock_merge_and_kill(ptr, gotten.ic_ptr));
				}
				else { message(DUERR_CANNOTCONVERT, var.line); return nullptr; }
				break;
			default:
				message(DUERR_CANNOTCONVERT, var.line);
				return nullptr;
			}
			++in;
		}
		ptr->append<ai::CALL>(orig->code());
		return std::move(icblock_merge_and_kill(ptr, clean_up_ptr));
	}

	expr_result get_int(const ls_int_index id)
	{
		auto ptr = icblock_create();
		ptr->append<ai::LDLITI>(id);
		return expr_result(TCAT_INT, ptr);
	}

	expr_result get_real(const ls_real_index id)
	{
		auto ptr = icblock_create();
		ptr->append<ai::LDLITR>(id);
		return expr_result(TCAT_REAL, ptr);
	}

	expr_result get_string(const ls_str_index id)
	{
		auto ptr = icblock_create();
		ptr->append<ai::LDLITS>(id);
		return expr_result(TCAT_STR, ptr);
	}

	expr_result get_variable(const var_decl& var, const comp_var& complete, symbol_tables * tab)
	{
		auto sp = tab->find_symbol(var.id);
		auto ptr = icblock_create();
		stack_address address;
		type_category cat = sp->access_typed()->type()->cat();
		switch (sp->kind())
		{
		case SKIND_CONST:
			switch (cat)
			{
			case TCAT_BOOL:
				ptr->append<ai::LDLITB>(sp->access_const()->access_bool_const()->bool_value());
				break;
			case TCAT_INT:
				ptr->append<ai::LDLITI>(sp->access_const()->access_int_const()->int_value());
				break;
			case TCAT_REAL:
				ptr->append<ai::LDLITR>(sp->access_const()->access_real_const()->real_value());
				break;
			case  TCAT_STR:
				ptr->append<ai::LDLITS>(sp->access_const()->access_str_const()->str_value());
				break;
			default:
				message(DUERR_CANNOTCONVERT, var.line);
			}
			break;
		case SKIND_GLOBAL_VARIABLE:
			address = sp->access_global_variable()->address();
			cat = complete.cat;
			address += complete.offset;
			switch (cat)
			{
			case TCAT_BOOL:
				ptr->append<ai::GLDB>(address);
				break;
			case TCAT_INT:
				ptr->append<ai::GLDI>(address);
				break;
			case TCAT_REAL:
				ptr->append<ai::GLDR>(address);
				break;
			case  TCAT_STR:
				ptr->append<ai::GLDS>(address);
				break;
			case TCAT_RECORD:
				for (auto it = sp->access_typed()->type()->access_record()->begin(); it != sp->access_typed()->type()->access_record()->end(); ++it)
				{
					if ((*it).type()->cat() == TCAT_RECORD) ptr = icblock_merge_and_kill(ptr, unwrap_record_g(*it, address + (*it).offset(), tab));
					else if ((*it).type()->cat() == TCAT_BOOL) ptr->append<ai::GLDB>(address + (*it).offset());
					else if ((*it).type()->cat() == TCAT_REAL) ptr->append<ai::GLDR>(address + (*it).offset());
					else if ((*it).type()->cat() == TCAT_STR) ptr->append<ai::GLDS>(address + (*it).offset());
					else if ((*it).type()->cat() == TCAT_INT) ptr->append<ai::GLDI>(address + (*it).offset());
					else message(DUERR_CANNOTCONVERT, var.line);
				}
				break;
			default:
				message(DUERR_CANNOTCONVERT, var.line);
				break;
			}
			break;
		case  SKIND_LOCAL_VARIABLE:
			address = sp->access_local_variable()->address();
			cat = complete.cat;
			address += complete.offset;
			switch (cat)
			{
			case TCAT_BOOL:
				ptr->append<ai::LLDB>(address);
				break;
			case TCAT_INT:
				ptr->append<ai::LLDI>(address);
				break;
			case TCAT_REAL:
				ptr->append<ai::LLDR>(address);
				break;
			case  TCAT_STR:
				ptr->append<ai::LLDS>(address);
				break;
			case TCAT_RECORD:
				for (auto it = sp->access_typed()->type()->access_record()->begin(); it != sp->access_typed()->type()->access_record()->end(); ++it)
				{
					if ((*it).type()->cat() == TCAT_RECORD) ptr = icblock_merge_and_kill(ptr, unwrap_record_l(*it, address + (*it).offset(), tab));
					else if ((*it).type()->cat() == TCAT_BOOL) ptr->append<ai::LLDB>(address + (*it).offset());
					else if ((*it).type()->cat() == TCAT_REAL) ptr->append<ai::LLDR>(address + (*it).offset());
					else if ((*it).type()->cat() == TCAT_STR) ptr->append<ai::LLDS>(address + (*it).offset());
					else if ((*it).type()->cat() == TCAT_INT) ptr->append<ai::LLDI>(address + (*it).offset());
					else message(DUERR_CANNOTCONVERT, var.line);
				}
				break;
			default:
				message(DUERR_CANNOTCONVERT, var.line);
			}
			break;
		case SKIND_FUNCTION:
			ptr = icblock_merge_and_kill(ptr, check_and_create_function_call(var, std::vector<expr_result>(), tab).ic_ptr);
			break;
		default:
			message(DUERR_CANNOTCONVERT, var.line);
			break;
		}
		return expr_result(complete.cat, ptr);
	}

	icblock_pointer assignment(const var_decl & var, const comp_var& comp, const expr_result & rhs, symbol_tables * tab)
	{
		auto ptr = rhs.ic_ptr;
		auto sp = tab->find_symbol(var.id);
		// Returning a value from function
		if (sp->kind() == SKIND_FUNCTION)
		{
			if (tab->my_function_name() != var.id) { message(DUERR_NOTMINE, var.line); return nullptr; }
			switch (sp->access_typed()->type()->cat())
			{
			case TCAT_INT:
				if (rhs.cat == TCAT_INT) { ptr->append<ai::LSTI>(tab->my_return_address()); }
				else if (rhs.cat == TCAT_REAL) { message(DUERR_CONVERSION, var.line); ptr->append<ai::CVRTRI>(); ptr->append<ai::LSTI>(tab->my_return_address()); }
				else message(DUERR_CANNOTCONVERT, var.line);
				break;
			case TCAT_REAL:
				if (rhs.cat == TCAT_REAL) ptr->append<ai::LSTR>(tab->my_return_address());
				else if (rhs.cat == TCAT_INT) { ptr->append<ai::CVRTIR>(); ptr->append<ai::LSTR>(tab->my_return_address()); }
				break;
			case TCAT_BOOL:
				if (rhs.cat == TCAT_BOOL) ptr->append<ai::LSTB>(tab->my_return_address());
				else message(DUERR_CANNOTCONVERT, var.line);
				break;
			case TCAT_STR:
				if (rhs.cat == TCAT_STR)  ptr->append<ai::LSTS>(tab->my_return_address());
				else message(DUERR_CANNOTCONVERT, var.line);
				break;
			default:
				message(DUERR_CANNOTCONVERT, var.line);
				break;
			}
			return ptr;
		}
		auto const cat = comp.cat;
		icblock_pointer block;
		switch (cat)
		{
		case TCAT_BOOL:
			if (rhs.cat == TCAT_BOOL)
			{
				if (sp->kind() == SKIND_LOCAL_VARIABLE) ptr->append<ai::LSTB>(sp->access_local_variable()->address() + comp.offset);
				else if (sp->kind() == SKIND_GLOBAL_VARIABLE) ptr->append<ai::GSTB>(sp->access_global_variable()->address() + comp.offset);
				else message(DUERR_CANNOTCONVERT, var.line);
			}
			else message(DUERR_CANNOTCONVERT, var.line);
			break;
		case TCAT_INT:
			if (rhs.cat == TCAT_INT)
			{
				if (sp->kind() == SKIND_LOCAL_VARIABLE)  ptr->append<ai::LSTI>(sp->access_local_variable()->address() + comp.offset);
				else if (sp->kind() == SKIND_GLOBAL_VARIABLE) ptr->append<ai::GSTI>(sp->access_global_variable()->address() + comp.offset);
				else message(DUERR_CANNOTCONVERT, var.line);
			}
			else if (rhs.cat == TCAT_REAL)
			{
				message(DUERR_CONVERSION, var.line);
				if (sp->kind() == SKIND_LOCAL_VARIABLE) { ptr->append<ai::CVRTRI>(); ptr->append<ai::LSTI>(sp->access_local_variable()->address() + comp.offset); }
				else if (sp->kind() == SKIND_GLOBAL_VARIABLE) { ptr->append<ai::CVRTRI>(); ptr->append<ai::GSTI>(sp->access_global_variable()->address() + comp.offset); }
				else message(DUERR_CANNOTCONVERT, var.line);
			}
			else message(DUERR_CANNOTCONVERT, var.line);
			break;
		case TCAT_REAL:
			if (rhs.cat == TCAT_INT)
			{
				if (sp->kind() == SKIND_LOCAL_VARIABLE) { ptr->append<ai::CVRTIR>(); ptr->append<ai::LSTR>(sp->access_local_variable()->address() + comp.offset); }
				else if (sp->kind() == SKIND_GLOBAL_VARIABLE) { ptr->append<ai::CVRTIR>(); ptr->append<ai::GSTR>(sp->access_global_variable()->address() + comp.offset); }
				else message(DUERR_CANNOTCONVERT, var.line);
			}
			else if (rhs.cat == TCAT_REAL)
			{
				if (sp->kind() == SKIND_LOCAL_VARIABLE) ptr->append<ai::LSTR>(sp->access_local_variable()->address() + comp.offset);
				else if (sp->kind() == SKIND_GLOBAL_VARIABLE) ptr->append<ai::GSTR>(sp->access_global_variable()->address() + comp.offset);
				else message(DUERR_CANNOTCONVERT, var.line);
			}
			else message(DUERR_CANNOTCONVERT, var.line);
			break;
		case TCAT_STR:
			if (rhs.cat == TCAT_STR)
			{
				if (sp->kind() == SKIND_LOCAL_VARIABLE) ptr->append<ai::LSTS>(sp->access_local_variable()->address() + comp.offset);
				else if (sp->kind() == SKIND_GLOBAL_VARIABLE) ptr->append<ai::GSTS>(sp->access_global_variable()->address() + comp.offset);
				else message(DUERR_CANNOTCONVERT, var.line);
			}
			else message(DUERR_CANNOTCONVERT, var.line);
			break;
		case TCAT_RECORD:
			stack_address address;
			if (rhs.cat == TCAT_RECORD)
			{
				if (sp->kind() == SKIND_LOCAL_VARIABLE) {
					address = sp->access_local_variable()->address();
					auto on_offset = 0;
					for (auto it = sp->access_typed()->type()->access_record()->begin(); it != sp->access_typed()->type()->access_record()->end(); ++it)
					{
						if (on_offset == comp.offset)
						{
							block = assign_record_l(*it, address + on_offset, tab);
							break;
						}
						if ((*it).type()->cat() == TCAT_RECORD) load_from_offset_l(*it,address,comp.offset, on_offset,tab);
						else on_offset++;
					}
				}
				else if (sp->kind() == SKIND_GLOBAL_VARIABLE) {
					address = sp->access_global_variable()->address();
					auto on_offset = 0;
					for (auto it = sp->access_typed()->type()->access_record()->begin(); it != sp->access_typed()->type()->access_record()->end(); ++it)
					{
						if (on_offset == comp.offset)
						{
							block = assign_record_g(*it, address + on_offset, tab);
							break;
						}
						if ((*it).type()->cat() == TCAT_RECORD) load_from_offset_g(*it, address, comp.offset, on_offset, tab);
						else on_offset++;
					}
				}
				ptr = icblock_merge_and_kill(ptr, block);
			}
			break;
		default:
			message(DUERR_CANNOTCONVERT, var.line);
			break;
		}
		return std::move(icblock_merge_and_kill(rhs.ic_ptr, ptr));
	}

	expr_result unary_minus_op(const expr_result & oper, const std::uint_least32_t line)
	{
		auto ptr = icblock_create();
		if (oper.cat == TCAT_INT) ptr->append<ai::MINUSI>();
		else if (oper.cat == TCAT_REAL) ptr->append<ai::MINUSR>();
		else message(DUERR_CANNOTCONVERT, line);
		return expr_result(oper.cat, icblock_merge_and_kill(oper.ic_ptr, ptr));
	}

	expr_result binary_oper_signadd_op(const expr_result & left, const expr_result & right, const DUTOKGE_OPER_SIGNADD sign, const std::uint_least32_t line)
	{
		if (left.cat == TCAT_INT && right.cat == TCAT_INT)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_SIGNADD::DUTOKGE_PLUS) res->append<ai::ADDI>();
			else res->append<ai::SUBI>();
			return expr_result(TCAT_INT, res);
		}
		if (left.cat == TCAT_REAL && right.cat == TCAT_REAL)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_SIGNADD::DUTOKGE_PLUS) res->append<ai::ADDR>();
			else res->append<ai::SUBR>();
			return expr_result(TCAT_REAL, res);
		}
		if (left.cat == TCAT_INT && right.cat == TCAT_REAL)
		{
			left.ic_ptr->append<ai::CVRTIR>();
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_SIGNADD::DUTOKGE_PLUS) res->append<ai::ADDR>();
			else res->append<ai::SUBR>();
			return expr_result(TCAT_REAL, res);
		}
		if (left.cat == TCAT_REAL && right.cat == TCAT_INT)
		{
			right.ic_ptr->append<ai::CVRTIR>();
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_SIGNADD::DUTOKGE_PLUS) res->append<ai::ADDR>();
			else res->append<ai::SUBR>();
			return expr_result(TCAT_REAL, res);
		}
		if (left.cat == TCAT_STR && right.cat == TCAT_STR && sign == DUTOKGE_OPER_SIGNADD::DUTOKGE_PLUS)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::ADDS>();
			return expr_result(TCAT_STR, res);
		}
		message(DUERR_CANNOTCONVERT, line);
		return expr_result();

	}

	expr_result binary_oper_mul_op(const expr_result & left, const expr_result & right, const DUTOKGE_OPER_MUL sign, const std::uint_least32_t line)
	{
		if (left.cat == TCAT_INT && right.cat == TCAT_INT)
		{
			icblock_pointer res;
			if (sign == DUTOKGE_OPER_MUL::DUTOKGE_SOLIDUS)
			{
				left.ic_ptr->append<ai::CVRTIR>();
				right.ic_ptr->append<ai::CVRTIR>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::DIVR>();
				return expr_result(TCAT_REAL, res);
			}
			res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_MUL::DUTOKGE_ASTERISK) res->append<ai::MULI>();
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_DIV) res->append<ai::DIVI>();
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_MOD) res->append<ai::MODI>();
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
			return expr_result(TCAT_INT, res);
		}
		if (left.cat == TCAT_REAL && right.cat == TCAT_REAL)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_MUL::DUTOKGE_ASTERISK) res->append<ai::MULR>();
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_SOLIDUS) res->append<ai::DIVR>();
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_DIV)
			{
				message(DUERR_CONVERSION, line);
				left.ic_ptr->append<ai::CVRTRI>();
				right.ic_ptr->append<ai::CVRTRI>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::DIVI>();
				return expr_result(TCAT_INT, res);
			}
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_MOD)
			{
				message(DUERR_CONVERSION, line);
				left.ic_ptr->append<ai::CVRTRI>();
				right.ic_ptr->append<ai::CVRTRI>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::MODI>();
				return expr_result(TCAT_INT, res);
			}
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
			return expr_result(TCAT_REAL, res);
		}
		if (left.cat == TCAT_INT && right.cat == TCAT_REAL)
		{
			icblock_pointer res;
			if (sign == DUTOKGE_OPER_MUL::DUTOKGE_ASTERISK)
			{
				left.ic_ptr->append<ai::CVRTIR>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::MULR>();
				return expr_result(TCAT_REAL, res);
			}
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_DIV)
			{
				message(DUERR_CONVERSION, line);
				right.ic_ptr->append<ai::CVRTRI>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::DIVI>();
				return expr_result(TCAT_INT, res);
			}
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_MOD)
			{
				message(DUERR_CONVERSION, line);
				right.ic_ptr->append<ai::CVRTRI>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::MODI>();
				return expr_result(TCAT_INT, res);
			}
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_SOLIDUS)
			{
				left.ic_ptr->append<ai::CVRTIR>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::DIVR>();
				return expr_result(TCAT_REAL, res);
			}
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
		}
		if (left.cat == TCAT_REAL && right.cat == TCAT_INT)
		{
			icblock_pointer res;
			if (sign == DUTOKGE_OPER_MUL::DUTOKGE_ASTERISK)
			{
				right.ic_ptr->append<ai::CVRTIR>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::MULR>();
				return expr_result(TCAT_REAL, res);
			}
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_DIV)
			{
				message(DUERR_CONVERSION, line);
				left.ic_ptr->append<ai::CVRTRI>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::DIVI>();
				return expr_result(TCAT_INT, res);
			}
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_MOD)
			{
				message(DUERR_CONVERSION, line);
				left.ic_ptr->append<ai::CVRTRI>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::MODI>();
				return expr_result(TCAT_INT, res);
			}
			else if (sign == DUTOKGE_OPER_MUL::DUTOKGE_SOLIDUS)
			{
				right.ic_ptr->append<ai::CVRTIR>();
				res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
				res->append<ai::DIVR>();
				return expr_result(TCAT_REAL, res);
			}
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
		}
		if (left.cat == TCAT_BOOL && right.cat == TCAT_BOOL && sign == DUTOKGE_OPER_MUL::DUTOKGE_AND)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::AND>();
			return expr_result(TCAT_BOOL, res);
		}
		message(DUERR_CANNOTCONVERT, line);
		return expr_result();
	}

	expr_result binary_oper_rel_op(const expr_result & left, const expr_result & right, const DUTOKGE_OPER_REL sign, const std::uint_least32_t line)
	{
		if (left.cat == TCAT_INT && right.cat == TCAT_INT)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_REL::DUTOKGE_GE) res->append<ai::GEI>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_GT) res->append<ai::GTI>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LE) res->append<ai::LEI>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LT) res->append<ai::LTI>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_NE) res->append<ai::NEI>();
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_REAL && right.cat == TCAT_REAL)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_REL::DUTOKGE_GE) res->append<ai::GER>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_GT) res->append<ai::GTR>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LE) res->append<ai::LER>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LT) res->append<ai::LTR>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_NE) res->append<ai::NER>();
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_INT && right.cat == TCAT_REAL)
		{
			left.ic_ptr->append<ai::CVRTIR>();
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_REL::DUTOKGE_GE) res->append<ai::GER>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_GT) res->append<ai::GTR>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LE) res->append<ai::LER>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LT) res->append<ai::LTR>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_NE) res->append<ai::NER>();
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_REAL && right.cat == TCAT_INT)
		{
			right.ic_ptr->append<ai::CVRTIR>();
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_REL::DUTOKGE_GE) res->append<ai::GER>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_GT) res->append<ai::GTR>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LE) res->append<ai::LER>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LT) res->append<ai::LTR>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_NE) res->append<ai::NER>();
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_STR && right.cat == TCAT_STR)
		{
			right.ic_ptr->append<ai::CVRTIR>();
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			if (sign == DUTOKGE_OPER_REL::DUTOKGE_GE) res->append<ai::GES>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_GT) res->append<ai::GTS>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LE) res->append<ai::LES>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_LT) res->append<ai::LTS>();
			else if (sign == DUTOKGE_OPER_REL::DUTOKGE_NE) res->append<ai::NES>();
			else { message(DUERR_CANNOTCONVERT, line); return expr_result(); }
			return expr_result(TCAT_BOOL, res);
		}

		message(DUERR_CANNOTCONVERT, line);
		return expr_result();
	}

	expr_result binary_eq_op(const expr_result & left, const expr_result & right, const std::uint_least32_t line)
	{
		if (left.cat == TCAT_INT && right.cat == TCAT_INT)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::EQI>();
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_REAL && right.cat == TCAT_REAL)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::EQR>();
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_INT && right.cat == TCAT_REAL)
		{
			left.ic_ptr->append<ai::CVRTIR>();
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::EQR>();
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_REAL && right.cat == TCAT_INT)
		{
			right.ic_ptr->append<ai::CVRTIR>();
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::EQR>();
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_BOOL && right.cat == TCAT_BOOL)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::EQB>();
			return expr_result(TCAT_BOOL, res);
		}
		if (left.cat == TCAT_STR && right.cat == TCAT_STR)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::EQS>();
			return expr_result(TCAT_BOOL, res);
		}
		message(DUERR_CANNOTCONVERT, line);
		return expr_result();
	}

	expr_result binary_or_op(const expr_result & left, const expr_result & right, const std::uint_least32_t line)
	{
		if (left.cat == TCAT_BOOL && right.cat == TCAT_BOOL)
		{
			auto res = icblock_merge_and_kill(left.ic_ptr, right.ic_ptr);
			res->append<ai::OR>();
			return expr_result(TCAT_BOOL, res);
		}
		message(DUERR_CANNOTCONVERT, line);
		return expr_result();
	}

	comp_var get_record_field(const std::vector<ls_id_index>& list, symbol_tables * tab)
	{
		auto sp = tab->find_symbol(*list.begin());
		auto curr_rec = sp->access_typed()->type()->access_record();

		stack_address offset = 0;
		type_category cat = TCAT_RECORD;
		for (auto it = ++list.begin(); it < list.end(); ++it)
		{
			offset += curr_rec->find(*it)->offset();
			cat = curr_rec->find(*it)->type()->cat();
			if (cat == TCAT_RECORD) { curr_rec = curr_rec->find(*it)->type()->access_record(); }
		}
		return comp_var(*list.begin(), offset, cat);
	}

	icblock_pointer unwrap_record_g(const field_entry tp, const stack_address address, symbol_tables * tab)
	{
		icblock_pointer ptr = icblock_create();
		for (auto it = tp.type()->access_record()->begin(); it != tp.type()->access_record()->end(); ++it)
		{
			if ((*it).type()->cat() == TCAT_RECORD) ptr = icblock_merge_and_kill(ptr, unwrap_record_g(*it, address + (*it).offset(), tab));
			else if ((*it).type()->cat() == TCAT_BOOL) ptr->append<ai::GLDB>(address + (*it).offset());
			else if ((*it).type()->cat() == TCAT_REAL) ptr->append<ai::GLDR>(address + (*it).offset());
			else if ((*it).type()->cat() == TCAT_STR) ptr->append<ai::GLDS>(address + (*it).offset());
			else if ((*it).type()->cat() == TCAT_INT) ptr->append<ai::GLDI>(address + (*it).offset());
		}
		return ptr;
	}

	icblock_pointer unwrap_record_l(const field_entry tp, const stack_address address, symbol_tables * tab)
	{
		icblock_pointer ptr = icblock_create();
		for (auto it = tp.type()->access_record()->begin(); it != tp.type()->access_record()->end(); ++it)
		{
			if ((*it).type()->cat() == TCAT_RECORD) ptr = icblock_merge_and_kill(ptr, unwrap_record_g(*it, address + (*it).offset(), tab));
			else if ((*it).type()->cat() == TCAT_BOOL) ptr->append<ai::LLDB>(address + (*it).offset());
			else if ((*it).type()->cat() == TCAT_REAL) ptr->append<ai::LLDR>(address + (*it).offset());
			else if ((*it).type()->cat() == TCAT_STR) ptr->append<ai::LLDS>(address + (*it).offset());
			else if ((*it).type()->cat() == TCAT_INT) ptr->append<ai::LLDI>(address + (*it).offset());
		}
		return ptr;
	}

	icblock_pointer assign_record_l(const field_entry tp, const stack_address address, symbol_tables * tab)
	{
		icblock_pointer block = icblock_create();
		for (auto it = tp.type()->access_record()->begin(); it != tp.type()->access_record()->end(); ++it)
		{
			auto ic = icblock_create();
			if ((*it).type()->cat() == TCAT_RECORD) { block = icblock_merge_and_kill(assign_record_l(*it, address + (*it).offset(), tab), block); }
			else if ((*it).type()->cat() == TCAT_BOOL)
			{
				ic->append<ai::LSTB>(address + (*it).offset()); block = icblock_merge_and_kill(ic, block);
			}
			else if ((*it).type()->cat() == TCAT_REAL)
			{
				ic->append<ai::LSTR>(address + (*it).offset()); block = icblock_merge_and_kill(ic, block);
			}
			else if ((*it).type()->cat() == TCAT_STR) {
				ic->append<ai::LSTS>(address + (*it).offset()); block = icblock_merge_and_kill(ic, block);
			}
			else if ((*it).type()->cat() == TCAT_INT) {
				ic->append<ai::LSTI>(address + (*it).offset()); block = icblock_merge_and_kill(ic, block);
			}
		}
		return block;
	}

	icblock_pointer load_from_offset_l(const field_entry tp,const stack_address address, const stack_address from_offset, stack_address & on_offset, symbol_tables * tab)
	{
		for (auto it = tp.type()->access_record()->begin(); it != tp.type()->access_record()->end(); ++it)
		{
			if (on_offset == from_offset)
			{
				return assign_record_l(*it, address + on_offset, tab);
			}
			if ((*it).type()->cat() == TCAT_RECORD) load_from_offset_l(*it, address, from_offset, on_offset, tab);
			else on_offset++;
		}
		return icblock_create();
	}

	icblock_pointer load_from_offset_g(const field_entry tp, const stack_address address, const stack_address from_offset, stack_address & on_offset, symbol_tables * tab)
	{
		for (auto it = tp.type()->access_record()->begin(); it != tp.type()->access_record()->end(); ++it)
		{
			if (on_offset == from_offset)
			{
				return assign_record_g(*it, address + on_offset, tab);
			}
			if ((*it).type()->cat() == TCAT_RECORD) load_from_offset_g(*it, address, from_offset, on_offset, tab);
			else on_offset++;
		}
		return icblock_create();
	}

	icblock_pointer load_on_offset_l(const field_entry tp, const stack_address address, const stack_address from_offset, stack_address & current_offset, symbol_tables * tab)
	{
		for (auto it = tp.type()->access_record()->begin(); it != tp.type()->access_record()->end(); ++it)
		{
			if (current_offset == from_offset)
			{
				return unwrap_record_l(*it, address + current_offset, tab);
			}
			if ((*it).type()->cat() == TCAT_RECORD) load_on_offset_l(*it, address, from_offset, current_offset, tab);
			else current_offset++;
		}
		return icblock_create();
	}

	icblock_pointer load_on_offset_g(const field_entry tp, const stack_address address, const stack_address from_offset, stack_address & current_offset, symbol_tables * tab)
	{
		for (auto it = tp.type()->access_record()->begin(); it != tp.type()->access_record()->end(); ++it)
		{
			if (current_offset == from_offset)
			{
				return unwrap_record_g(*it, address + current_offset, tab);
			}
			if ((*it).type()->cat() == TCAT_RECORD) load_on_offset_g(*it, address, from_offset, current_offset, tab);
			else current_offset++;
		}
		return icblock_create();
	}

	icblock_pointer assign_record_g(const field_entry tp, const stack_address address, symbol_tables * tab)
	{
		icblock_pointer block = icblock_create();
		for (auto it = tp.type()->access_record()->begin(); it != tp.type()->access_record()->end(); ++it)
		{
			auto ic = icblock_create();
			if ((*it).type()->cat() == TCAT_RECORD)  block = icblock_merge_and_kill(assign_record_g(*it, address + (*it).offset(), tab), block);
			else if ((*it).type()->cat() == TCAT_BOOL) {
				ic->append<ai::GSTB>(address + (*it).offset()); block = icblock_merge_and_kill(ic, block);
			}
			else if ((*it).type()->cat() == TCAT_REAL) {
				ic->append<ai::GSTR>(address + (*it).offset()); block = icblock_merge_and_kill(ic, block);
			}
			else if ((*it).type()->cat() == TCAT_STR) {
				ic->append<ai::GSTS>(address + (*it).offset()); block = icblock_merge_and_kill(ic, block);
			}
			else if ((*it).type()->cat() == TCAT_INT) {
				ic->append<ai::GSTI>(address + (*it).offset()); block = icblock_merge_and_kill(ic, block);
			}
		}
		return block;
	}


	type_pointer get_type_pointer(const var_decl& type, symbol_tables * tab) {
		const auto ts = tab->find_symbol(type.id)->access_type();
		if (!ts) { message(DUERR_NOTTYPE, type.line, *type.id); }
		return ts->type();
	}
};

/*****************************************/