#include "cool-tree.h"
#include "emit.h"
#include "symtab.h"
#include <assert.h>
#include <stdio.h>
#include <unordered_map>
using std::unordered_map;
#include <unordered_set>
using std::unordered_set;
#include <deque>
using std::deque;

enum Basicness { Basic, NotBasic };
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

unordered_map<Symbol, unordered_map<Symbol, int> *> name2func2offset;
unordered_map<Symbol, unordered_map<Symbol, method_class *> *> name2func2method;
unordered_map<Symbol, unordered_map<Symbol, int> *> name2attr2offset;
unordered_map<Symbol, int> classtag;

struct where {
    int offset;
    char *reg;
};
SymbolTable<Symbol, where> var2addr;

class CgenClassTable : public SymbolTable<Symbol, CgenNode> {
  private:
    List<CgenNode> *nds, *mds;
    ostream &str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;

    unordered_set<Symbol> basicclass;
    int tagpool = 4;

    unordered_map<Symbol, Class_> name2class;
    unordered_map<Symbol, deque<Class_> *> name2chain;

    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

    void inverse();
    void class_table();
    void dispatch_table();
    void prototype();

    void object_init();
    void class_methods();

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);

  public:
    CgenClassTable(Classes, ostream &str);
    void code();
    CgenNodeP root();
};

class CgenNode : public class__class {
  private:
    CgenNodeP parentnd; // Parent of class
    List<CgenNode> *children; // Children of class
    Basicness basic_status; // `Basic' if class is basic
                            // `NotBasic' otherwise

  public:
    CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }
};

class BoolConst {
  private:
    int val;

  public:
    BoolConst(int);
    void code_def(ostream &, int boolclasstag);
    void code_ref(ostream &) const;
};
