
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <typeinfo>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//

static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beqz(char *source, char *label, ostream &s)
{
  s << BEQZ << source << " " << label << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//

static int sp_from_fp;

static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
  sp_from_fp--;

}

static void emit_pop(char *reg, ostream &str)
{
  emit_addiu(SP,SP,4,str);
  emit_load(reg,0,SP,str);
  sp_from_fp++;

}

static void emit_push_FP(ostream &str)
{
  emit_store(FP,0,SP,str);
  emit_addiu(SP,SP,-4,str);
  sp_from_fp = 1;

}

static void emit_pop_FP(ostream &str)
{
  emit_addiu(SP,SP,4,str);
  emit_load(FP,0,SP,str);
  assert(sp_from_fp == 1);

}

static void emit_pop_n(int n, ostream &str)  {
  if(n == 0)
    return;
  emit_addiu(SP,SP,4*n,str);
  sp_from_fp += n;
}

static void emit_seq(char *dest, char *src1, char *src2, ostream &str)  {
  str << SEQ << dest << ' ' << src1 << ' ' << src2 << endl;
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_bool_to_Bool(ostream& str) {
  emit_push(ACC, str);
  emit_partial_load_address(ACC, str);
  emit_protobj_ref(Bool, str);
  str << endl;
  emit_jal("Object.copy", str);
  emit_pop(T1, str);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, str);
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;
  if(*str == 0)  {
    emit_protobj_ref(Str, s);
    s << LABEL;
  }
  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;

      emit_disptable_ref(Str, s);

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  if(str[0] == '0' && str[1] == 0)  {
    emit_protobj_ref(Int, s);
    s << LABEL;
  }

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

      emit_disptable_ref(Int, s);

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;
  if(val == 0)  {
    emit_protobj_ref(Bool, s);
    s << LABEL;
  }
  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

      emit_disptable_ref(Bool, s);

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.ALIGN
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}

void CgenClassTable::code_proto_objects() 
{
  str << endl << ALIGN;
  for(List<CgenNode> *l = nds; l; l = l->tl())
    l->hd()->code_proto_object(str);
}

void CgenClassTable::code_classnames()  
{
  str << ALIGN;
  str << CLASSNAMETAB << LABEL;

  str << WORD 
      << STRCONST_PREFIX << stringtable.lookup_string("String")->get_index()
      << endl;
  str << WORD 
      << STRCONST_PREFIX << stringtable.lookup_string("Int")->get_index()
      << endl;
  str << WORD 
      << STRCONST_PREFIX << stringtable.lookup_string("Bool")->get_index()
      << endl;

  for(int i = 3; i < last_tag; i++) {
    Symbol clsname = lookup_by_tag(i)->get_name();
    str << WORD 
      << STRCONST_PREFIX << stringtable.lookup_string(clsname->get_string())->get_index()
      << endl;
  }


}

void CgenClassTable::code_dispatch_tables()
{
  str << endl << ALIGN;
  for(List<CgenNode> *l = nds; l; l = l->tl())  {
    emit_disptable_ref(l->hd()->get_name(), str);
    str << LABEL;

    str << WORD;
    emit_init_ref(l->hd()->get_name(), str);
    str << endl;

    l->hd()->dispatch_table.code(str);
  }


}
//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_text()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())  {

    l->hd()->code_methods(this);
  }

}
void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this, -1));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this, -1));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this, -1));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this, last_tag++));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this, last_tag++));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this, intclasstag));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this, boolclasstag));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this, stringclasstag));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this, last_tag++));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = 0;
   intclasstag =    1;
   boolclasstag =   2;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   code();
   exitscope();
}

void CgenClassTable::code()
{

  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  root()->build_envs();

  code_proto_objects();
  code_classnames();
  code_dispatch_tables();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();
  code_text();

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

CgenNodeP CgenClassTable::lookup_by_tag(int tag)  {
  for(List<CgenNode> *l = nds; l; l = l->tl())  {
    if(l->hd()->get_tag() == tag)
      return l->hd();
  }
  return NULL;
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct, int t) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
   class_tag(t)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

void CgenNode::code_proto_object(ostream& str)
{
  if(get_name() == Str || get_name() == Int || get_name() == Bool)
    return;
  //GC Tag
  str << WORD << -1 << endl;

  emit_protobj_ref(get_name(), str); 
  str << LABEL;

  //Class Tag
  str << WORD << class_tag << endl;

  //Object Size
  str << WORD << DEFAULT_OBJFIELDS + attr_table.len() << endl;

  //Dispatch Pointer
  str << WORD;
  emit_disptable_ref(get_name(), str);
  str << endl;

  for(int i = 0; i < attr_table.len(); i++) {
    str << WORD << 0 << endl;
  }
}

void CgenNode::code_methods(CgenClassTableP table)
{
  table->current_class = this;
  ostream& str = table->stream();
  //init - also follows calling convention
  code_init_method(table);

  //Runtime methods
  if(get_name() == Object || get_name() == IO || get_name() == Str)
    return;

  //Methods
  for(int i = features->first(); features->more(i); i = features->next(i))  {
    auto feature = features->nth(i);
    if(!feature->is_attr())  {
      emit_method_ref(get_name(), feature->get_name(), str);
      str << LABEL;
      feature->code(table);
    }
  }


}

void CgenNode::code_init_method(CgenClassTableP table)  {
  ostream& str = table->stream();
  emit_init_ref(get_name(), str);
  str << LABEL;
  if(attr_table.len() == 0) {
    emit_return(str);
    return;
  }

  //Set default attrs
  bool t1_is_zero = false;
  for(int i = 0; i < attr_table.len(); i++) {
    auto p = attr_table.at(i);
    
    if(p.second == Str || p.second == Int || p.second == Bool)  {
      emit_partial_load_address(T1, str);
      emit_protobj_ref(p.second, str);
      str << endl;
      t1_is_zero = false;
    }
    else{
      if(!t1_is_zero)
        emit_move(T1, ZERO, str);
      t1_is_zero = true;
    }
    emit_store(T1, DEFAULT_OBJFIELDS + i, ACC, str);
  }
  //Follow calling convention so that expressions evaluate correctly
  emit_push(RA, str);
  emit_push(SELF, str);
  emit_move(SELF, ACC, str);
  emit_push_FP(str);
  emit_move(FP, SP, str);

  code_init_attrs(table);

  emit_pop_FP(str);
  emit_pop(SELF, str);
  emit_pop(RA, str);
  emit_return(str);
}

void CgenNode::code_init_attrs(CgenClassTableP table){
  if(get_name() != Object)
    parentnd->code_init_attrs(table);
  for(int i = features->first(); features->more(i); i = features->next(i))  {
    auto feature = features->nth(i);
    if(feature->is_attr())  {
      feature->code(table);
    }
  }
}

void CgenNode::build_envs() 
{
  for(int i = features->first(); features->more(i); i = features->next(i))  {
    auto feature = features->nth(i);
    if(!feature->is_attr())  {
      dispatch_table.add_identifier(feature->get_name(), get_name());
    }
    else {
      attr_table.add_identifier(feature->get_name(), feature->get_type());
    }
  }
  for (List<CgenNode> *l = children; l; l = l->tl())  {
    l->hd()->dispatch_table = Environment(dispatch_table);
    l->hd()->attr_table = Environment(attr_table);
    l->hd()->build_envs();
  }
}

int Environment::get_index(Symbol identifier) {
  int i = 0;
  for(auto p: table)  {
    if(p.first == identifier)
      return i;
    i++;
  }
  return -1;
}

Symbol Environment::get_class(Symbol identifier)  {
  for(auto p : table)
    if(p.first == identifier)
      return p.second;
  return No_class;
}

void Environment::add_identifier(Symbol identifier, Symbol class_name) {
  for(auto &p : table)
    if(p.first == identifier)  {
      p.second = class_name;
      return;
    }
  table.push_back(std::make_pair(identifier, class_name));
}

void Environment::code(ostream &str)  {
  int i = 0;

  for(auto p : table) {
    str << WORD;
    emit_method_ref(p.second, p.first, str);
    str << endl;
    i++;
  }
}

std::pair<Symbol, Symbol> Environment::at(int index) {
  int i = 0;
  for(auto p : table) {
    if(i == index)
      return p;
    i++;
  }
  return std::make_pair(No_class, No_class);
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(CgenClassTableP table) {
  ostream &str = table->stream();
  //local_vars.lookup(name) will store the result of expr
  expr->code(table);

  int attr_ind = table->current_class->attr_table.get_index(name);
  int* fp_offset = table->local_vars.lookup(name);
  //name is a local variable
  if(fp_offset != NULL)
    emit_store(ACC, *fp_offset, FP, str);
  //name is an attribute
  else if(attr_ind != -1)
    emit_store(ACC, DEFAULT_OBJFIELDS + attr_ind, SELF, str);

}

void static_dispatch_class::code(CgenClassTableP table) {
  ostream &str = table->stream();
// Push arg1 to argn
  int n_args = 0;
  for(int i = actual->first(); actual->more(i); i = actual->next(i))  {
    Expression e = actual->nth(i);
    e->code(table);
    emit_push(ACC, str);
    n_args++;
  }

  expr->code(table);
// Check for dispatch on void
  emit_load_imm(T1, get_line_number(), str);
  emit_move(T3, ACC, str);
  emit_load_address(ACC, "str_const0", str);
  emit_beqz(T3, "_dispatch_abort", str);
  emit_move(ACC, T3, str);

// Jump to dispatch[SELF][index]
  CgenNodeP cl = table->probe(type_name);

  int method_index = cl->dispatch_table.get_index(name) + 1;

  str << LW << T2<<' ';
  emit_disptable_ref(type_name, str);
  str << " + " << method_index * WORD_SIZE<< endl;
  emit_jalr(T2, str);

  sp_from_fp += n_args;

}

void dispatch_class::code(CgenClassTableP table) {
  ostream &str = table->stream();
// Push arg1 to argn
  int n_args = 0;
  for(int i = actual->first(); actual->more(i); i = actual->next(i))  {
    Expression e = actual->nth(i);
    e->code(table);
    emit_push(ACC, str);
    n_args++;
  }

  expr->code(table);
  // Check for dispatch on void
  emit_load_imm(T1, get_line_number(), str);
  emit_move(T3, ACC, str);
  emit_load_address(ACC, "str_const0", str);
  emit_beqz(T3, "_dispatch_abort", str);
  emit_move(ACC, T3, str);

  // Jump to dispatch[SELF][index]
  CgenNodeP cl = table->probe(expr->get_type());
  if(cl->get_name() == SELF_TYPE)
    cl = table->current_class;

  int method_index = cl->dispatch_table.get_index(name) + 1;
  emit_load(T2, DISPTABLE_OFFSET, ACC, str);
  emit_load(T2, method_index, T2, str);
  emit_jalr(T2, str);

  sp_from_fp += n_args;
}

void cond_class::code(CgenClassTableP table) {
  ostream &str = table->stream();
  static int ctr = 0;

  pred->code(table);
  //Jump if false
  emit_load(ACC, DEFAULT_OBJFIELDS, ACC, str);
  str << BEQZ << ACC << " cond_else" << ctr << endl;
  then_exp->code(table);
  //Jump to end
  str << BRANCH << "cond_end" << ctr << endl;
  
  str << "\tcond_else" << ctr << LABEL;
  else_exp->code(table);
  
  str << "\tcond_end" << ctr << LABEL;
  ctr++;
}

void loop_class::code(CgenClassTableP table) {
  ostream &str = table->stream();
  static int ctr = 0;
  str << "\tloop_start" << ctr << LABEL;
  pred->code(table);
  //Jump out of loop if false
  emit_load(ACC, DEFAULT_OBJFIELDS, ACC, str);
  str << BEQZ << ACC << " loop_end" << ctr << endl;

  body->code(table);
  //Jump to loop
  str << BRANCH << "loop_start" << ctr << endl;
  
  str << "\tloop_end" << ctr << LABEL;
  ctr++;
}

void typcase_class::code(CgenClassTableP table) {
    cout<< "NI";

}

void block_class::code(CgenClassTableP table) {
  for(int i = body->first(); body->more(i); i = body->next(i))
        body->nth(i)->code(table);
}

void let_class::code(CgenClassTableP table) {
  ostream &str = table->stream();
  //Push on stack and store offset from FP in local_vars

  //Default initialization
  if(typeid(*init) == typeid(no_expr_class) && (type_decl == Str || type_decl == Int || type_decl == Bool)){
    emit_partial_load_address(ACC, str);
    emit_protobj_ref(type_decl, str);
    str << endl;
  }
  else
    init->code(table);
  //ACC has init value
  emit_push(ACC, str);
  table->local_vars.enterscope();
  table->local_vars.addid(identifier, new int(sp_from_fp));
  body->code(table);
  table->local_vars.exitscope();
  emit_pop_n(1, str);

}

void plus_class::code(CgenClassTableP table) {
    cout<< "NI";

}

void sub_class::code(CgenClassTableP table) {
    cout<< "NI";

}

void mul_class::code(CgenClassTableP table) {
    cout<< "NI";

}

void divide_class::code(CgenClassTableP table) {
    cout<< "NI";

}

void neg_class::code(CgenClassTableP table) {
  //~
    cout<< "NI";

}

void lt_class::code(CgenClassTableP table) {
    cout<< "NI";

}

void eq_class::code(CgenClassTableP table) {
  ostream &str = table->stream();
  e1->code(table);
  emit_push(ACC, str);
  e2->code(table);
  emit_move(T2, ACC, str);
  emit_pop(T1, str);

  emit_load_bool(ACC, truebool, table->stream());
  emit_load_bool(A1, falsebool, table->stream());

  emit_jal("equality_test", str);

}

void leq_class::code(CgenClassTableP table) {
    cout<< "NI";

}

void comp_class::code(CgenClassTableP table) {
  //not
    cout<< "NI";

}

void int_const_class::code(CgenClassTableP table)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()), table->stream());
}

void string_const_class::code(CgenClassTableP table)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()), table->stream());
}

void bool_const_class::code(CgenClassTableP table)
{
  emit_load_bool(ACC, BoolConst(val), table->stream());
}

void new__class::code(CgenClassTableP table) {
  ostream &str = table->stream();
  if(type_name == SELF_TYPE)  {
    emit_move(ACC, SELF, str);
  }
  else {
    //$a0 = proto
    emit_partial_load_address(ACC, str);
    emit_protobj_ref(type_name, str);
    str << endl;
  }

  //jump to Object.copy
  emit_jal("Object.copy", str);
  //Object copy in ACC

  //Run class.init
  if(type_name == SELF_TYPE)  {
    emit_load(T1, DISPTABLE_OFFSET, ACC, str);
    emit_load(T1, 0, T1, str);
    emit_jalr(T1, str);
  }
  else {
    str << JAL;
    emit_init_ref(type_name, str);
    str << endl;
  }
  

}

void isvoid_class::code(CgenClassTableP table) {
  ostream& str = table->stream();
  e1->code(table);
  emit_seq(ACC, ACC, ZERO, str);
  emit_bool_to_Bool(str);
}

void no_expr_class::code(CgenClassTableP table) {
  emit_move(ACC, ZERO, table->stream());
}

void object_class::code(CgenClassTableP table) {
  ostream &str = table->stream();

  int attr_ind = table->current_class->attr_table.get_index(name);
  int* fp_offset = table->local_vars.lookup(name);
  if(name == self)
    emit_move(ACC, SELF, str);
  //name is a local variable
  else if(fp_offset != NULL)
    emit_load(ACC, *fp_offset, FP, str);
  //name is an attribute
  else if(attr_ind != -1)
    emit_load(ACC, DEFAULT_OBJFIELDS + attr_ind, SELF, str);
}




void attr_class::code(CgenClassTableP table) {
  if(typeid(*init) == typeid(no_expr_class))  {
    return;
  }
//Set up initialization
  ostream& str = table->stream();

  emit_push(ACC, str);
  init->code(table);

  int attr_ind = table->current_class->attr_table.get_index(name);
  emit_move(T1, ACC, str);
  emit_pop(ACC, str);
  emit_store(T1, DEFAULT_OBJFIELDS + attr_ind, ACC, str);


}

void method_class::code(CgenClassTableP table) {
// Set up locations for parameters
  table->local_vars.enterscope();
  int n = formals->len();
  for(int i = formals->first(), j = 1; formals->more(i); i = formals->next(i), j++) 
    table->local_vars.addid(formals->nth(i)->get_name(), new int(ARG_OFFSET+n-j));

  ostream& str = table->stream();
// Push RA on stack
  emit_push(RA, str);
// Push SELF on stack
  emit_push(SELF, str);
  emit_move(SELF, ACC, str);
// Push FP on stack
  emit_push_FP(str);
// Set fp to point to stack top
  emit_move(FP, SP, str);
// Evaluate expression - this will set ACC to return value
  expr->code(table);
// Restore FP
  emit_pop_FP(str);
// Restore SELF
  emit_pop(SELF, str);
// Jump to RA
  emit_pop(RA, str);
  // Pop arg1 to argn
  emit_pop_n(formals->len(), str);
  emit_return(str);

  table->local_vars.exitscope();

}