
//**************************************************************
///
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
#include <map>
#include <queue>
#include <vector>
extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

class GlobalCGenState;

class GlobalCGenState {
  private:
    int label_cntr;

  public:
    CgenClassTable *classtableptr;
    CgenNodeP curr_cgen_node;
    SymbolTable<Symbol, Symbol> *symtab;
    void init_label_cntr() { label_cntr = -1; }
    int increment_label_cntr() { return label_cntr = label_cntr + 1; }
};

GlobalCGenState cgen_state;

void restore_stack_after_call(ostream &s);
void setup_stack_for_call(ostream &s);

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
Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth, No_class,
    No_type, Object, out_int, out_string, prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

static char *gc_init_names[] = {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] = {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

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

/*
 perform code generation in two passes:
  - The first pass decides the object layout for each class,
  particularly the offset at which each attribute is stored in an object.
  - Using this information, the second pass recursively walks each feature
  and generates stack machine code for each expression.
*/
void program_class::cgen(ostream &os) {
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    cgen_state.init_label_cntr();
    // Set up the symbol table, must have an initial scope to add things to
    // cgen_state.symtab = new SymbolTable<Symbol,Symbol>();
    // cgen_state.symtab->enterscope();

    initialize_constants();

    CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

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

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream &s) {
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" << endl;
}

// FROM REGISTER TO MEMORY!!!!
// sw reg1 offset(reg2)
// store 32 bit word in reg1 at address reg2+offset
static void emit_store(char *source_reg, int offset, char *dest_reg, ostream &s) {
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")" << endl;
}

// li reg imm
// reg <- imm
static void emit_load_imm(char *dest_reg, int val, ostream &s) { s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream &s) {
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s) { s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s) {
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s) {
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s) {
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s) {
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s) { s << NEG << dest << " " << src1 << endl; }

// add reg1 reg2 reg3
// reg1 <- reg2 + reg3
static void emit_add(char *dest, char *src1, char *src2, ostream &s) {
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s) {
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

// add immediate
// addiu reg1 reg2 imm
// reg1 <- reg2 + imm
// u means underflow is not checked
static void emit_addiu(char *dest, char *src1, int imm, ostream &s) {
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s) {
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s) {
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s) {
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s) {
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s) { s << JALR << "\t" << dest << endl; }

// JUMP AND LINK is for a procedure call
// GOTO a label
// save the address of the next instruction (save where you will jump back to)
// the last thing on the caller side
// callee fishes out the return address from the RA register
static void emit_jal(char *address, ostream &s) { s << JAL << address << endl; }

static void emit_return(ostream &s) { s << RET << endl; }

static void emit_gc_assign(ostream &s) { s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream &s) { s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream &s) { s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s) { s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream &s) { s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s) {
    s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s) {
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s) {
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream &s) {
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str) {
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s) { emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s) { emit_store(source, DEFAULT_OBJFIELDS, dest, s); }

static void emit_test_collector(ostream &s) {
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s) {
    if (source != (char *)A1) emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
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

// /*
//   Optimization that makes code generation better
//   Instead of pushing and popping intermediates onto the stack,
//   we just index into it.

//   Compute the number of temporaries needed for an expression.
//   Simple recursive algorithm.
// */
// int compute_NT(Expression )
// {
//   switch( expr.get_name() )
//   {
//     case "plus": // NT(e1 + e2)
//       return std::max( compute_NT(e1), 1 + compute_NT(e2) );

//     case "sub": // NT(e1 - e2)
//       return std::max( compute_NT(e1), 1 + compute_NT(e2) );

//     case "cond": // NT(if e1 = e2 then e3 else e4)
//       return std::max( compute_NT(e1), 1 + compute_NT(e2), compute_NT(e3), compute_NT(e4) );

//     case "dispatch": // or method?  NT(id(e1,…,en)
//       int max_discovered = 0;
//       for each of the formals e_1 to e_n
//       {
//         max_discovered = std::max( compute_NT(e1), max_discovered );
//       }
//       return max_discovered;

//     case "int": // NT(int)
//      return 0;

//     case "id": // NT(id)
//       return 0;

//     default:
//       std::cout << "EXPRESSION INVALID!" << std::endl;
//       break;
//   }
// }

// /*
// 4 bytes for the return address
// 4 bytes for the frame pointer
// 4 * num_args to store pointer to each argument
// 4 * NTs to store temporary values on the stack
// */
// int compute_act_rec_num_bytes(int num_args)
// {
// 	return 4 * (compute_NT() + 2 + num_args);
// }

// // act_Rec_sz = compute_act_rec_num_bytes();
// // char[act_rec_sz]

//
// Strings
//
void StringEntry::code_ref(ostream &s) { s << STRCONST_PREFIX << index; }

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag) {
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL // label
      << WORD << stringclasstag << endl // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
      << WORD << STRINGNAME << DISPTAB_SUFFIX << endl;

    /***** Add dispatch information for class String ******/

    // dispatch table
    s << WORD;
    lensym->code_ref(s);
    s << endl; // string length
    emit_string_constant(s, str); // ascii string
    s << ALIGN; // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag) {
    for (List<StringEntry> *l = tbl; l; l = l->tl()) l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s) { s << INTCONST_PREFIX << index; }

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag) {
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL // label
      << WORD << intclasstag << endl // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
      << WORD << INTNAME << DISPTAB_SUFFIX << endl;

    /***** Add dispatch information for class Int ******/

    // dispatch table
    s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag) {
    for (List<IntEntry> *l = tbl; l; l = l->tl()) l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const { s << BOOLCONST_PREFIX << val; }

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag) {
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL // label
      << WORD << boolclasstag << endl // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
      << WORD << BOOLNAME << DISPTAB_SUFFIX << endl;

    /***** Add dispatch information for class Bool ******/

    // dispatch table
    s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data() {
    Symbol main = idtable.lookup_string(MAINNAME);
    Symbol string = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    str << GLOBAL << CLASSOBJTAB << endl;

    std::map<CgenNodeP, int>::iterator it = (class_tags.begin());
    while (it != class_tags.end()) {
        str << GLOBAL << it->first->get_name() << PROTOBJ_SUFFIX << endl;
        str << GLOBAL << it->first->get_name() << CLASSINIT_SUFFIX << endl;
        it++;
    }

    str << INTTAG << LABEL << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL << WORD << stringclasstag << endl;
    // loop through each of the classes
    // and print out their values
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text() {
    str << GLOBAL << HEAP_START << endl << HEAP_START << LABEL << WORD << 0 << endl << "\t.text" << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag) {
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc() {
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

void CgenClassTable::code_constants() {
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}

/*
  Traverse receives one node at a time, from root to leaves
  Start class tags at 3 (bc Int,Bool,String are 0,1,2 )
  class_tags is a map

  Features map: nodes->Features
*/
void CgenClassTable::traverse(CgenNodeP nd) {

    if (nd->get_name() == Int) {
        class_tags.insert(std::make_pair(nd, 1));
    } else if (nd->get_name() == Str) {
        class_tags.insert(std::make_pair(nd, 0));
    } else if (nd->get_name() == Bool) {
        class_tags.insert(std::make_pair(nd, 2));
    } else {

        class_tags.insert(std::make_pair(nd, increase_class_tag()));
    }

    // get all of the features for the current node we are at
    if (nd == root()) {
        features_map.insert(std::make_pair(nd, nd->get_features()));
        Features feats = nd->get_features();
        for (int i = feats->first(); feats->more(i); i = feats->next(i)) {
            Feature feat = feats->nth(i);
            if (feat->feat_is_method()) {
                // every single node has its own method map
                // add to the dispatch table!
                nd->method_map.insert(std::make_pair(feat->get_feature_name()->get_string(), nd));
            }
        }
    }
    CgenNodeP parent = nd->get_parentnd();

    // if you're root, it's messy and a separate case to handle
    if (nd != root()) {

        // separate root case
        Features feats = nd->get_features();
        for (int i = feats->first(); feats->more(i); i = feats->next(i)) {
            Feature feat = feats->nth(i);
            if (feat->feat_is_method()) {
                nd->method_map.insert(std::make_pair(feat->get_feature_name()->get_string(), nd));
            }
        }

        // just for your parent (the non-root case, when you have a parent)
        std::map<std::string, CgenNodeP>::iterator it = (parent->method_map.begin());
        while (it != parent->method_map.end()) {
            nd->method_map.insert(std::make_pair(it->first, it->second));
            it++;
        }
        features_map.insert(std::make_pair(nd, append_Features(nd->get_features(), parent->get_features())));
    }

    List<CgenNode> *c;
    for (c = nd->get_children(); c != NULL; c = c->tl()) { traverse(c->hd()); }
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s) {

    cgen_state.classtableptr = this;

    stringclasstag = 0 /* Change to your String class tag here */;
    intclasstag = 1 /* Change to your Int class tag here */;
    boolclasstag = 2 /* Change to your Bool class tag here */;

    enterscope();
    if (cgen_debug) cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();
    init_class_tag();
    traverse(root());

    // at index 0, 1, 2, 3 etc

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    addid(No_class, new CgenNode(class_(No_class, No_class, nil_Features(), filename), Basic, this));
    addid(SELF_TYPE, new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename), Basic, this));
    addid(prim_slot, new CgenNode(class_(prim_slot, No_class, nil_Features(), filename), Basic, this));

    //
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation of class name
    //        copy() : SELF_TYPE       returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //
    install_class(new CgenNode(
        class_(Object, No_class,
               append_Features(append_Features(single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                               single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                               single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename),
        Basic, this));

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //
    install_class(new CgenNode(
        class_(IO, Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
                           single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))),
                       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               filename),
        Basic, this));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    install_class(
        new CgenNode(class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename), Basic, this));

    //
    // Bool also has only the "val" slot.
    //
    install_class(
        new CgenNode(class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename), Basic, this));

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
    //       str_field                            the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //
    install_class(new CgenNode(
        class_(
            Str, Object,
            append_Features(
                append_Features(append_Features(append_Features(single_Features(attr(val, Int, no_expr())),
                                                                single_Features(attr(str_field, prim_slot, no_expr()))),
                                                single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
                single_Features(
                    method(substr, append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))),
                           Str, no_expr()))),
            filename),
        Basic, this));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd) {
    Symbol name = nd->get_name();

    if (probe(name)) { return; }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs) {
    for (int i = cs->first(); cs->more(i); i = cs->next(i)) install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
    for (List<CgenNode> *l = nds; l; l = l->tl()) set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) { children = new List<CgenNode>(n, children); }

void CgenNode::set_parentnd(CgenNodeP p) {
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

/*
  We are fishing out things like stringconst0 etc which exist in the stringtable,
  and which stand for "Object" etc.

  We use the code reference to look it up in the string table.
*/
void CgenClassTable::print_class_name_tab() {
    str << CLASSNAMETAB << ":" << endl;
    std::map<CgenNodeP, int>::iterator it = (class_tags.begin());
    while (it != class_tags.end()) {
        str << WORD;
        (stringtable.lookup_string(it->first->get_name()->get_string()))->code_ref(str);
        str << endl;
        it++;
    }
}

/*
  Print out _protObj, and _init for every class, in order of classtags.
*/
void CgenClassTable::print_class_obj_tab() {
    str << CLASSOBJTAB << ":" << endl;
    std::map<CgenNodeP, int>::iterator it = (class_tags.begin());
    while (it != class_tags.end()) {
        str << WORD << it->first->get_name()->get_string() << PROTOBJ_SUFFIX << endl;
        str << WORD << it->first->get_name()->get_string() << CLASSINIT_SUFFIX << endl;
        it++;
    }
}

/*
  Iterate through the features map, which has a list of all the CGenNodes
  Get a node, and the node's name. For that node's method map, we print
  out every single method. This is the dispatch table.
*/
void CgenClassTable::print_dispatch_tables() {
    std::map<CgenNodeP, Features>::iterator it = (get_features_map()).begin();
    while (it != get_features_map().end()) {
        CgenNodeP curr_node = it->first;

        str << curr_node->get_name() << DISPTAB_SUFFIX << ":" << endl;
        std::map<std::string, CgenNodeP>::iterator iter = (curr_node->method_map.begin());

        while (iter != curr_node->method_map.end()) {
            str << WORD << iter->second->get_name() << METHOD_SEP << iter->first << endl;
            iter++;
        }
        it++;
    }
}

int CgenClassTable::get_method_offset(std::string method_name, std::string node_name) {
    CgenNodeP nd;
    std::map<CgenNodeP, int>::iterator it = (class_tags.begin());
    while (it != class_tags.end()) {
        if (it->first->get_name()->get_string() == node_name) { nd = it->first; }
        it++;
    }
    int offset = 0;
    std::map<std::string, CgenNodeP>::iterator iter = (nd->method_map.begin());

    while (iter != nd->method_map.end()) {
        if (iter->first == method_name) return offset;
        offset += 1;
        iter++;
    }

    return 0;
}

/*
 */

int CgenClassTable::get_attribute_offset(std::string attribute, CgenNodeP nd) {

    int offset = 3;
    std::map<CgenNodeP, Features>::iterator it = (get_features_map()).begin();

    while (it != get_features_map().end()) {
        if (it->first->get_name()->get_string() == nd->get_name()->get_string()) {
            Features curr_attributes = it->second;

            for (int j = curr_attributes->first(); curr_attributes->more(j); j = curr_attributes->next(j)) {

                Feature attr = curr_attributes->nth(j);

                if (attr->get_feature_name()->get_string() == attribute) { return offset; }
                if (!attr->feat_is_method()) { offset += 1; }
            }
        }
        it++;
    }
    return 0;
}

void CgenClassTable::print_node_attrs() {
    std::map<CgenNodeP, Features>::iterator it = (get_features_map()).begin();
    while (it != get_features_map().end()) {

        str << it->first->get_name() << PROTOBJ_SUFFIX << ":" << endl;
        Features curr_attributes = it->second;
        int count = 0;
        for (int j = curr_attributes->first(); curr_attributes->more(j); j = curr_attributes->next(j)) {
            Feature count_at = curr_attributes->nth(j);

            if (!count_at->feat_is_method()) { count++; }
        }
        str << WORD << class_tags[it->first] << endl;
        str << WORD << count + 3 << endl; // size
        str << WORD << it->first->get_name() << DISPTAB_SUFFIX << endl;
        for (int i = curr_attributes->first(); curr_attributes->more(i); i = curr_attributes->next(i)) {
            Feature curr_attr = curr_attributes->nth(i);
            Symbol attr_type = curr_attr->get_type_decl();
            if (!curr_attr->feat_is_method()) {
                if (attr_type == Bool) {
                    str << WORD;
                    falsebool.code_ref(str);
                    str << endl;
                } else if (attr_type == Str) {
                    str << WORD;
                    (stringtable.lookup_string(""))->code_ref(str);
                    str << endl;
                } else if (attr_type == Int) {
                    str << WORD;
                    (inttable.lookup_string("0"))->code_ref(str);
                    str << endl;
                } else {
                    str << WORD << EMPTYSLOT << endl;
                }
            }
        }
        str << WORD << -1 << endl;
        it++;
    }
}

void CgenClassTable::code() {
    if (cgen_debug) cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug) cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug) cout << "coding constants" << endl;
    code_constants();

    print_class_name_tab();
    print_class_obj_tab();
    print_node_attrs();
    print_dispatch_tables();

    // {
    //    cout<<l->get_proto()->garbage_collector_tag <<endl;
    //    l->get_proto()->class_tag = i;
    //    i++;

    //    for(int i = l->get_proto()->attributes->first(); l->get_proto()->attributes->more(i); i =
    //    cl->get_proto()->attributes->next(i)){
    //      if (!i->is_method()) cout << i << endl;

    // check each attribute, if its a method, add to dispatch table
    // if its an attribute, dont move it
    //

    //                 Add your code to emit
    //                   - prototype objects
    //                   - class_nameTab
    //                   - dispatch tables
    //

    // for each of the classes
    // print the dispatch table for this class
    // print the name of the class + << DISPTAB_SUFFIX << ":";
    // Symbol sym;
    // emit_disptable_ref(sym, str);
    // str << WORD <<

    // emit_init_ref(Symbol sym, ostream& s)
    // { s << sym << CLASSINIT_SUFFIX; }

    // emit_label_ref(int l, ostream &s)
    // { s << "label" << l; }

    // emit_protobj_ref(Symbol sym, ostream& s)
    // { s << sym << PROTOBJ_SUFFIX; }

    // emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
    // { s << classname << METHOD_SEP << methodname; }

    if (cgen_debug) cout << "coding global text" << endl;
    code_global_text();

    //                 Add your code to emit
    //                   - object initializer
    //                   - the class methods
    //                   - etc...

    // str is the output stream

    // for(int i = nds->first(); nds->more(i); i = nds->next(i))
    // {
    //   CGenNode node = classes->nth(i);
    //   str << GLOBAL; myclass.code_ref(str);  str << endl;
    // }
    print_methods();
}

void CgenClassTable::print_methods() {
    std::map<CgenNodeP, int>::iterator it = class_tags.begin();

    while (it != class_tags.end()) {
        cgen_state.curr_cgen_node = it->first;
        str << it->first->get_name() << CLASSINIT_SUFFIX << ":" << endl;
        bool is_object_init = (strcmp(it->first->get_name()->get_string(), "Object") == 0);
        print_class_init_code(is_object_init, it->first);
        it++;
    }

    std::map<CgenNodeP, int>::iterator iter = class_tags.begin();
    while (iter != class_tags.end()) {
        cgen_state.curr_cgen_node = iter->first;
        if (!iter->first->basic()) {
            Features curr_attributes = iter->first->get_features();
            for (int j = curr_attributes->first(); curr_attributes->more(j); j = curr_attributes->next(j)) {
                Feature curr_feat = curr_attributes->nth(j);
                if (curr_feat->feat_is_method()) {
                    str << iter->first->get_name() << "." << curr_feat->get_feature_name()->get_string() << ":" << endl;
                    curr_feat->code(str);
                }
            }
        }
        iter++;
    }
}

/*
    Setup stack also moves what was in the accumulator
    into the self register
*/
void CgenClassTable::print_class_init_code(bool is_object_init, CgenNodeP nd) {
    setup_stack_for_call(str);
    // procedure call
    // save the address of the next instruction (save where you will jump back to)
    if (!is_object_init) {

        char *parent = (nd->get_parentnd())->get_name()->get_string();
        char parent_buf[1000];
        char init_buf[1000];
        strcpy(parent_buf, parent);
        strcpy(init_buf, CLASSINIT_SUFFIX);
        strcat(parent_buf, init_buf);
        // jal to the parent!

        emit_jal(parent_buf, str);
    }

    // loop over all of your attributes
    // and run the code on each of them

    Features curr_attributes = nd->get_features();
    for (int j = curr_attributes->first(); curr_attributes->more(j); j = curr_attributes->next(j)) {
        Feature curr_feat = curr_attributes->nth(j);
        if (!curr_feat->feat_is_method()) { curr_feat->code(str); }
    }
    // move SELF register contents into the accumulator
    emit_move(ACC, SELF, str);
    restore_stack_after_call(str);
}

CgenNodeP CgenClassTable::root() { return probe(Object); }

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct)
    : class__class((const class__class &)*nd), parentnd(NULL), children(NULL), basic_status(bstatus) {
    stringtable.add_string(name->get_string()); // Add class name to string table
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

/*
  Start with initial store S.
  Evaluate expression e (right hand side of assignment), in initial store S.

  We get back an updated store S1.
  Look up identifier in the store, get out the value.
  Build a new store, where we replace the old value with the new value.
  We get back the new store S2, the final store.
  Result is the value v and the updated store (includes all side effects).
*/
void assign_class::code(ostream &s) {

    // evaluate expression in initial store
    // emit_store(char *source_reg, int offset, char *dest_reg, s)

    // start with initial store S
    // evaluate expression e (right hand side of assignment), in initial store S
    // we get back an updated store S1
    // look up identifier in the store, get out the value
    // build a new store, where we replace the old value with the new value
    // we get back the new store S2, the final store
    // result is the value v and the updated store (includes all side effects)

    // go over this one together
    // first evaluate expression on right hand side of assignment, update store 1
    // look up store to get value??
    // look up identifier in environment, get final store

    expr->code(s);
    // result is now in the accumulator
    int offs = cgen_state.classtableptr->get_attribute_offset(name->get_string(), cgen_state.curr_cgen_node);

    emit_load_address(T1, SELF, s);
    emit_addiu(T1, T1, offs, s);
    emit_store(ACC, 0, T1, s);
    // emit_load_address(char *dest_reg, char *address, s);

    // sw reg1 offset(reg2)
    // store 32 bit word in reg1 at address reg2+offset
    // emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
}

void static_dispatch_class::code(ostream &s) {}

// /*
//   A FUNCTION CALL! ON THE CALLER SIDE
//   - 4*n + 4 arguments in the activation record
//   - 4 bytes per argument, and also the frame pointer
// */
void dispatch_class::code(ostream &s) {
    // Generate code for all of the arguemnts
    // save the actual parameters in reverse order
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual->nth(i)->code(s);
        // for each of the arguments
        // generate code for them
        // store accumulator onto the stack
        // add -4 to the stack
        // save the frame pointer onto the stack
        emit_store(ACC /* char *source_reg */, 0 /* offset */, SP /* char *dest_reg */, s);
        emit_addiu(SP, SP, -4, s);
    }
    // pass in the label of the beginning of the function f
    // jump and link

    expr->code(s);
    // expression result should now be in the accumulator
    // MAKE SURE THE XPRESSION DID NOT RETURN NUL

    // ALSO CHECK FOR ONE OF THE 3 RUNTIME ERRORS
    std::string class_param;
    if (expr->get_type() == SELF_TYPE) {

        class_param = cgen_state.curr_cgen_node->get_name()->get_string();

    } else {

        class_param = expr->get_type()->get_string();
    }
    int offs = cgen_state.classtableptr->get_method_offset(name->get_string() /*method name*/, class_param);

    int label_id = cgen_state.increment_label_cntr();

    emit_bne(ACC, ZERO, label_id, s);
    emit_load_string(ACC, stringtable.lookup(0), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(label_id, s);

    // SELF/OBJECT will already be in the accumulator
    emit_load(T1 /*dst */, 2 /*offs*/, ACC /*src*/, s);
    emit_load(T1, offs, T1, s); // WALK ALONG THE DISPATCH TABLE UNTIL YOU FIND WHAT YOU WANT
    emit_jalr(T1, s);
}

/*
  - Set up a function DEFINITION!!! ON THE CALLEE SIDE
  - The frame pointer points to the top, not bottom of the frame
  - The callee pops the return address, the actual arguments and the saved
      value of the frame pointer.
  - FP is the Frame Pointer
  - SP is the Stack Pointer
  - RA is the Return Address

  The full activation record is the:
    - frame pointer FP (the caller sets this up)
    - all of the arguments (the caller sets this up)
    - and the return address RA (callee sets this up)

  TODO: CATCH ERROR -- dispatch on void
*/
void method_class::code(ostream &s) {

    setup_stack_for_call(s);
    // make sure a0 points to self
    // cgen_state.symtab->enterscope();
    // add all the formals to the symbol table
    // for(int i = formals->first(); formals->more(i); i = formals->next(i))
    //{
    //  cgen_state.symtab->addid( formals->nth(i)->get_name(), formals->nth(i)->get_type_decl() );
    //}
    expr->code(s);
    // now, after the body has been executed, we restore the environment
    restore_stack_after_call(s);
    // cgen_state.symtab->exitscope();
}

void setup_stack_for_call(ostream &s) {
    // (reg1 <- reg2 + imm) -- push the stack ptr down by 12 bytes
    emit_addiu(SP, SP, -12, s);
    // sw reg1 offset(reg2)
    // store 32 bit word in reg1 at address reg2+offset
    emit_store(FP, 3, SP, s);
    // store what was in the SELF register at 8 above stack ptr
    emit_store(SELF, 2, SP, s);
    // store what was in return address to 4 above stack ptr
    emit_store(RA, 1, SP, s);
    // make the frame ptr now stack_ptr + 16
    emit_addiu(FP, SP, 16, s);
    // move what was in accumulator into the SELF register
    emit_move(SELF, ACC, s);
}

/*
    We restore the frame:
    - before and after method,
    - before and after init

    Restore the frame pointer
    Then jump and link to the entry point of the function
    LOAD(DEST_REG, OFFSET, SRC_REG);
*/
void restore_stack_after_call(ostream &s) {
    // load what was 12 above stack_ptr into the frame_ptr
    emit_load(FP, 3, SP, s);
    // load 12 above stack_ptr into the frame pointer
    emit_load(SELF, 2, SP, s);
    //  load what was 4 above the satck pointer into the return address
    emit_load(RA, 1, SP, s);
    // push the stack pointer back up to restore state
    emit_addiu(SP, SP, 12, s);
    // jump and link to the entry point of the function
    emit_return(s); // jumps to RA
}

/*
  if e1 == 1 then e2 else e3

  T1 is Temporary 1
  ACC is the accumulator
  


*/
void cond_class::code(ostream &s) {

    //  // if e1 then e2 else e3 fi
    //  // evaluate the predicate first in store S, get back store S1
    //  pred->code(s);
    //  emit_load_imm( T1 /* char *dest_reg */, 0 /* val */, s);
    //  // is the predicate true? (equal to 1?). Branch if equal (beq)
    //  emit_beq( ACC /* char *src1 */, T1 /* char *src2 */, true_branch /* int label */, s);

    // // if the predicate Bool(true)
    //  // In S1, we evaluate the "true" branch of the if/then/else
    //  // Get back a new store S2 after we evaluate e2, along with value v
    //  // do not evaluate e3

    //  emit_label_ref(int l, s)

    //  emit_branch( true_branch /* int l */, s);
    //  then_exp->code(s);
    //  // b end_if

    //  // if the predicate Bool(False)
    //  // evaluate e3 and do not evaluate e2
    //  emit_branch( false_branch /* int l */, s);
    //  else_exp->code(s);
    //  // end_if
}

/*
  emit_addiu( stack_ptr, stack_ptr, -4, s);
  emit_store( acc, 0, stack_ptr, s ); // save the result onto the stack
*/
void loop_class::code(ostream &s) {

    // If predicate e1 is Bool(false)
    // then return as the result void, and the value S1 contains side effects of evaluating e1

    // Evaluate in Store S
    // If predicate Bool(true)
    // Evaluate loop one more time, get back new store S1 (first loop had side effects)
    // Now evaluate in store S1, get back value e2, and new store S2
    // Create a new instance of the while loop rule, but start in new store this time
    // (The new store we got from evaluating one iteration)
}

/*
  A case expression provides a runtime type test on objects.
  The class tag uniquely identifies the dynamic type of the object.
  - Generate a series of conditionals comparing the class tag of the object to
  the tags of the types specified in the branches of the case expression.

  Determine the branch of the case expression to evaluate -- load the class tag of
  the object on which the case is testing and comparing that value with constants
  (in branch instructions).

  TODO: CATCH ERROR -- case on void
*/
void typcase_class::code(ostream &s) {

    // loop over each of the cases
    // find the one that is the closest ancestor
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        // Case* curr_case = cases->nth(i);
        // emit_label_def( FOR EACH CASE )
        // emit_label_ref( FOR EACH CASE )
        // emit_jal( TO THE CLOSEST ANCESTOR )
        // emit_branch( BACK TO THE ESAC)
    }
}

void block_class::code(ostream &s) {
    // sequence of expressions
    // order is enforced by the stores
    // evaluation begins in store S, evaluate e1 in it, get back S1
    // etc for each expression
    // value of the block is the value of the last expression
}

void let_class::code(ostream &s) {

    // initialize new variable
    // evaluate the variable in the store S1
    // allocate a memory location for the new variable to live in
    // I_new = newloc(S); // get back a fresh location not already used in store S
    // then have body
    // Get a new environment E
    // evaluate e2, get new value v2, and new store S2
    // S2 will contain value v2, after it's initialized to the proper value
}

/*
  ACC is the accumulator
  SP is the stack pointer
  T1 is the Temporary 1 register
*/
void plus_class::code(ostream &s) {
    e1->code(s);
    // result now stored in accumulator
    emit_store(ACC /* char *source_reg */, 0 /* offset */, SP /* char *dest_reg */, s);
    emit_addiu(SP /* dest */, SP /* src1 */, -4 /* imm */, s);
    e2->code(s);
    // result now stored in acculumator
    emit_load(T1 /* char *dest_reg */, 4 /* offset */, SP /* char *source_reg */, s);
    emit_add(ACC /*char *dest $a0 */, T1 /* $t1 */, ACC /* char *src2 $a0 */, s);
    emit_addiu(SP, SP, 4, s);
}

/*
  Subtraction code is identical to the code for addition, except we use the "sub" instruction
  instead of the "add" instruction in the penultimate line. Only the actual operation
  is different

  SP is stack pointer
  ACC is accumulator
  T1 is Temporary 1 register
*/
void sub_class::code(ostream &s) {
    e1->code(s);
    // result now stored in accumulator
    emit_store(ACC /* char *source_reg */, 0 /* offset */, SP /* char *dest_reg */, s);
    emit_addiu(SP /* dest */, SP /* src1 */, -4 /* imm */, s);
    e2->code(s);
    // result now stored in acculumator
    emit_load(T1 /* char *dest_reg */, 4 /* offset */, SP /* char *source_reg */, s);
    emit_sub(ACC /*char *dest $a0 */, T1 /* $t1 */, ACC /* char *src2 $a0 */, s);
    emit_addiu(SP, SP, 4, s);
}

void mul_class::code(ostream &s) {
    e1->code(s);
    // result now stored in accumulator
    emit_store(ACC /* char *source_reg */, 0 /* offset */, SP /* char *dest_reg */, s);
    emit_addiu(SP /* dest */, SP /* src1 */, -4 /* imm */, s);
    e2->code(s);
    // result now stored in acculumator
    emit_load(T1 /* char *dest_reg */, 4 /* offset */, SP /* char *source_reg */, s);
    emit_mul(ACC /*char *dest $a0 */, T1 /* $t1 */, ACC /* char *src2 $a0 */, s);
    emit_addiu(SP, SP, 4, s);
}

void divide_class::code(ostream &s) {
    e1->code(s);
    // result now stored in accumulator
    emit_store(ACC /* char *source_reg */, 0 /* offset */, SP /* char *dest_reg */, s);
    emit_addiu(SP /* dest */, SP /* src1 */, -4 /* imm */, s);
    e2->code(s);
    // result now stored in acculumator
    emit_load(T1 /* char *dest_reg */, 4 /* offset */, SP /* char *source_reg */, s);
    emit_div(ACC /*char *dest $a0 */, T1 /* $t1 */, ACC /* char *src2 $a0 */, s);
    emit_addiu(SP, SP, 4, s);
}

/*
 Load -1 into the register T1, and then multiply by -1.
*/
void neg_class::code(ostream &s) {
    e1->code(s);
    emit_load_imm(T1, -1, s);
    emit_mul(ACC, T1, ACC, s);
}

/*
blt
*/
void lt_class::code(ostream &s) {}

/*
beq if equal

USE THE BUILT IN RUNTIME TEST FROM COOL RUNTIME

COPY THE REFERENCE COMPILER SOLUTION equality_test

*/
void eq_class::code(ostream &s) {}

void leq_class::code(ostream &s) {}

void comp_class::code(ostream &s) {}

void int_const_class::code(ostream &s) {
    // Create an Integer object, does not affect the store
    // set value to the literal
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream &s) {
    // Create a String object, does not affect the store
    // set the value to the string literal
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream &s) {
    // Create a Bool object, does not affect the store
    // set value to either true or false
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
    // check if type is SELF_TYPE
    // new SELF_TYPE will allocate an object with the same dynamic type as self
    // look at current self object, allocate of that type (find out concrete class we are allocating)

    // GET THE NAME OF THE PROTOTYPE OBJECT
    //
    char *expr_classname = get_type()->get_string();
    char classname_buf[1000];
    char protobj_buf[1000];
    strcpy(classname_buf, expr_classname);
    strcpy(protobj_buf, PROTOBJ_SUFFIX);
    strcat(classname_buf, protobj_buf);
    emit_load_address(ACC, classname_buf, s);
    emit_jal("Object.copy", s);

    // allocate n new locations to hold all n attribtues of an object (enough space for every attribute)
    // Form the new object
    // bind ith object to the ith location
    // Update the store S and initialize all of the attributes to their default values

    // integer default is 0
    // bool default is false
    // string default is empty string
    // for any other class, default value is void

    // we build a new environment (nothing to do with envirinment in which we called new)
    // only field names of the class are in scope

    // GET THE CLASSNAME

    char init_buf[1000];
    strcpy(classname_buf, expr_classname);
    strcpy(init_buf, CLASSINIT_SUFFIX);
    strcat(classname_buf, init_buf);
    emit_jal(classname_buf, s);
    // evaluate a block -- list of init expressions
    // Build the AST for this block, call block.code()
    // this is before we even run the initializers for the attributes
    // we order the attributes as greatest ancestor first, so attribbutes you inherited from ancestor go first
    // then within a class, then in the textual order in which they are listed in the class
    // then, evaluate all of the initializers and set the resulting attribute values
    // return the newly allocated object

    // new SELF_TYPE will allocate an object with the same dynamic type as self
}

void isvoid_class::code(ostream &s) {}

void no_expr_class::code(ostream &s) {}

/*
  if it is the self identifier, it just evaluates to so (self object)
  store unchanged

  If a variable evaluates to an object, we find where is it located.
  We load it into the accumulator.
  what object happens to be located there right now
  looking up a variable does not affect the store
*/
void object_class::code(ostream &s) {
    int offs = cgen_state.classtableptr->get_attribute_offset(name->get_string(), cgen_state.curr_cgen_node);
    emit_load(ACC, offs, SELF, s);
}
