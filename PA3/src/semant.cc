#include "semant.h"
#include "utilities.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

extern int semant_debug;
extern char *curr_filename;
SymbolTable<Symbol, Symbol> O;
SymbolTable<Symbol, method_class *> M;
Class_ global_class;
ClassTable *classtable;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth, No_class,
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

method_class *ismethod(Feature x) { return dynamic_cast<method_class *>(x); }

bool less_equal(Symbol son, Symbol dad) {
    // cout << son << "<" << dad << endl;
    if (son == dad) return true;
    if (dad == SELF_TYPE && son != SELF_TYPE) return false;
    if (son == SELF_TYPE) son = global_class->getname();
    if (dad == SELF_TYPE) dad = global_class->getname();
    if (!classtable->name2class.count(son) || !classtable->name2class.count(dad)) return false;
    for (auto xclass : *classtable->name2chain[son])
        if (xclass->getname() == dad) return true;
    return false;
}

bool undefined(Symbol xclass) { return xclass != SELF_TYPE && !classtable->name2class.count(xclass); }

Symbol lowest(Symbol a, Symbol b) {
    if (a == b) return a;
    if (a == SELF_TYPE) a = global_class->getname();
    if (b == SELF_TYPE) b = global_class->getname();
    auto ret = Object;
    for (auto i = classtable->name2chain[a]->begin(), j = classtable->name2chain[b]->begin();
         i != classtable->name2chain[a]->end() && j != classtable->name2chain[b]->end() &&
         (*i)->getname() == (*j)->getname();
         i++, j++)
        ret = (*i)->getname();
    return ret;
}

bool ClassTable::dfs(Symbol x) {
    visit[x] = 2;
    if (name2class.count(name2class[x]->getdad())) {
        if (visit.count(name2class[x]->getdad()) && visit[name2class[x]->getdad()] == 2) {
            semant_error(name2class[x]) << "Class " << name2class[x]->getname()
                                        << " is involved in an inheritance cycle.\n";
            loop = name2class[x]->getdad();
            return true;
        } else if (!visit.count(name2class[x]->getdad())) {
            if (dfs(name2class[x]->getdad())) {
                if (loop != No_class)
                    semant_error(name2class[x])
                        << "Class " << name2class[x]->getname() << " is involved in an inheritance cycle.\n";
                if (loop == x) loop = No_class;
                return true;
            }
        }
    }
    visit[x] = 1;
    return false;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {
    install_basic_classes();
    // Install classes & Check for class definition errors
    for (auto xclass : *classes) {
        if (xclass->getname() == Object || xclass->getname() == SELF_TYPE || xclass->getname() == IO ||
            xclass->getname() == Int || xclass->getname() == Bool || xclass->getname() == Str)
            semant_error(xclass) << "Redefinition of basic class " << xclass->getname() << ".\n";
        else if (name2class.count(xclass->getname()))
            semant_error(xclass) << "Class " << xclass->getname() << " was previously defined.\n";
        else if (xclass->getdad() == SELF_TYPE || xclass->getdad() == Int || xclass->getdad() == Bool ||
                 xclass->getdad() == Str)
            semant_error(xclass) << "Class " << xclass->getname() << " cannot inherit class " << xclass->getdad()
                                 << ".\n";
        else
            name2class[xclass->getname()] = xclass;
    }
    for (auto entry : name2class)
        if (entry.second->getdad() != No_class && !name2class.count(entry.second->getdad()))
            semant_error(entry.second) << "Class " << entry.second->getname() << " inherits from an undefined class "
                                       << entry.second->getdad() << ".\n";

    // Check for class inheritance errors
    for (auto entry : name2class)
        if (!visit.count(entry.first))
            if (dfs(entry.first)) break;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object, No_class,
               append_Features(append_Features(single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                               single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                               single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename);
    name2class[Object_class->getname()] = Object_class;

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
        class_(IO, Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
                           single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))),
                       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               filename);
    name2class[IO_class->getname()] = IO_class;

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class = class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename);
    name2class[Int_class->getname()] = Int_class;

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class = class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);
    name2class[Bool_class->getname()] = Bool_class;

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class = class_(
        Str, Object,
        append_Features(
            append_Features(append_Features(append_Features(single_Features(attr(val, Int, no_expr())),
                                                            single_Features(attr(str_field, prim_slot, no_expr()))),
                                            single_Features(method(length, nil_Formals(), Int, no_expr()))),
                            single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
            single_Features(method(substr,
                                   append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))),
                                   Str, no_expr()))),
        filename);
    name2class[Str_class->getname()] = Str_class;
}

void ClassTable::check_main() {
    // Check Main class
    if (!name2class.count(Main)) semant_error() << "Class Main is not defined.\n";
}

void ClassTable::check_method() {
    bool main_method_flag = false;
    for (auto entry : name2class) {
        auto xclass = entry.second;
        auto &methods = *(xclass->methods = new unordered_map<Symbol, Feature>);
        auto &attrs = *(xclass->attrs = new unordered_map<Symbol, Feature>);
        for (auto feature : *xclass->getfeatures()) {
            if (ismethod(feature)) {
                if (methods.count(feature->getname()))
                    semant_error(xclass, feature) << "Method " << feature->getname() << " is multiply defined.\n";
                methods[feature->getname()] = feature;
                main_method_flag |= xclass->getname() == Main && feature->getname() == main_meth;
            } else {
                if (attrs.count(feature->getname()))
                    semant_error(xclass, feature)
                        << "Attribute " << feature->getname() << " is multiply defined in class.\n";
                attrs[feature->getname()] = feature;
            }
        }
        // Check Main method
        if (xclass->getname() == Main && !main_method_flag)
            semant_error(name2class[Main]) << "No 'main' method in class Main.\n";
    }
}

void ClassTable::check_all() {
    check_method();
    check_main();
    for (auto entry : name2class) {
        auto xname = entry.first;
        auto &chain = *(name2chain[entry.first] = new deque<Class_>);
        for (auto i = xname; i != Object; i = name2class[i]->getdad()) chain.push_front(name2class[i]);
        chain.push_front(name2class[Object]);
    }
    for (auto entry : name2class) {
        auto xname = entry.first;
        auto xclass = entry.second;
        global_class = xclass;
        auto &chain = *name2chain[xname];
        if (xname == Object || xname == IO || xname == Int || xname == Bool || xname == Str) continue;
        for (auto yclass : chain) {
            if (yclass == chain.back()) break;
            M.enterscope();
            O.enterscope();
            for (auto feature : *yclass->getfeatures())
                if (ismethod(feature)) {
                    if (!M.lookup(feature->getname()))
                        M.addid(feature->getname(), new (method_class *)(ismethod(feature)));
                } else {
                    if (feature->getname() != self && !O.lookup(feature->getname())) {
                        O.addid(feature->getname(), new Symbol(feature->gettype()));
                    }
                }
        }
        // check redefined method and attr
        M.enterscope();
        O.enterscope();
        for (auto feature : *xclass->getfeatures()) {
            if (ismethod(feature)) {
                auto old = M.lookup(feature->getname());
                if (old) {
                    auto oldmethod = *old;
                    auto newmethod = ismethod(feature);
                    if (oldmethod->gettype() != newmethod->gettype()) {
                        semant_error(xclass, newmethod)
                            << "In redefined method " << newmethod->getname() << ", return type "
                            << newmethod->gettype() << " is different from original return type "
                            << oldmethod->gettype() << ".\n";
                    }
                    auto i = oldmethod->getformals()->begin();
                    auto j = newmethod->getformals()->begin();
                    if ((i != oldmethod->getformals()->end()) ^ (j != newmethod->getformals()->end())) {
                        semant_error(xclass, newmethod)
                            << "Incompatible number of formal parameters in redefined method " << newmethod->getname()
                            << ".\n";
                    }
                    while (i != oldmethod->getformals()->end() && j != newmethod->getformals()->end()) {
                        if ((*j)->gettype() != (*i)->gettype())
                            semant_error(xclass, newmethod)
                                << "In redefined method " << newmethod->getname() << ", parameter type "
                                << (*j)->gettype() << " is different from original type " << (*i)->gettype() << "\n";
                        ++i;
                        ++j;
                        if ((i != oldmethod->getformals()->end()) ^ (j != newmethod->getformals()->end())) {
                            semant_error(xclass, newmethod)
                                << "Incompatible number of formal parameters in redefined method "
                                << newmethod->getname() << ".\n";
                        }
                    }
                } else
                    M.addid(feature->getname(), new (method_class *)(ismethod(feature)));
            } else {
                auto old = O.lookup(feature->getname());
                if (feature->getname() == self) {
                    semant_error(xclass, feature) << "'self' cannot be the name of an attribute.\n";
                } else if (old)
                    semant_error(xclass, feature)
                        << "Attribute " << feature->getname() << " is an attribute of an inherited class.\n";
                else
                    O.addid(feature->getname(), new Symbol(feature->gettype()));
            }
        }
        for (auto feature : *xclass->getfeatures()) {
            if (ismethod(feature)) {
                if (xclass->getname() == Main && feature->getname() == main_meth) {
                    if (ismethod(feature)->getformals()->len())
                        semant_error(xclass) << "'main' method in class Main should have no arguments.\n";
                }
                feature->gettruetype();
            } else {
                feature->gettruetype();
            }
        }
        for (auto yclass : chain) {
            O.exitscope();
            M.exitscope();
        }
    }
}

void method_class::gettruetype() {
    O.enterscope();
    for (auto formal : *formals) {
        bool intact = true;
        if (formal->gettype() == SELF_TYPE) {
            intact = false;
            classtable->semant_error(global_class, formal)
                << "Formal parameter " << formal->getname() << " cannot have type SELF_TYPE.\n";
        }
        if (formal->getname() == self) {
            intact = false;
            classtable->semant_error(global_class, formal) << "'self' cannot be the name of a formal parameter.\n";
        }
        if (undefined(formal->gettype())) {
            intact = false;
            classtable->semant_error(global_class, formal)
                << "Class " << formal->gettype() << " of formal parameter " << formal->getname() << " is undefined.\n";
        }
        if (O.probe(formal->getname())) {
            intact = false;
            classtable->semant_error(global_class, formal)
                << "Formal parameter " << formal->getname() << " is multiply defined.\n";
        }
        if (formal->gettype() == SELF_TYPE) O.addid(formal->getname(), new Symbol(Object));
        if (intact) O.addid(formal->getname(), new Symbol(formal->gettype()));
    }
    if (undefined(gettype())) {
        classtable->semant_error(global_class, this)
            << "Undefined return type " << gettype() << " in method " << name << ".\n";
        auto truetype = expr->gettruetype();
    } else {
        auto truetype = expr->gettruetype();
        if (!less_equal(truetype, gettype()))
            classtable->semant_error(global_class, this)
                << "Inferred return type " << truetype << " of method " << name
                << " does not conform to declared return type " << return_type << ".\n";
    }
    O.exitscope();
}

void attr_class::gettruetype() {
    if (undefined(type_decl))
        classtable->semant_error(global_class, this)
            << "Class " << type_decl << " of attribute " << name << " is undefined.\n";
    else {
        auto truetype = init->gettruetype();
        if (truetype != No_type && !less_equal(truetype, type_decl))
            classtable->semant_error(global_class, this)
                << "Inferred type " << truetype << " of initialization of attribute " << name
                << " does not conform to declared type " << type_decl << ".\n";
    }
}

Symbol branch_class::gettruetype() {
    O.enterscope();
    if (undefined(type_decl))
        O.addid(name, new Symbol(Object));
    else
        O.addid(name, new Symbol(type_decl));
    auto type = expr->gettruetype();
    O.exitscope();
    return type;
}

Symbol assign_class::gettruetype() {
    if (name == self) {
        classtable->semant_error(global_class, this) << "Cannot assign to 'self'.\n";
        return type = expr->gettruetype();
    }
    if (!O.lookup(name)) {
        classtable->semant_error(global_class, this) << "Assignment to undeclared variable " << name << ".\n";
        return type = expr->gettruetype();
    }
    auto truetype = expr->gettruetype();
    if (!less_equal(truetype, *O.lookup(name))) {
        classtable->semant_error(global_class, this)
            << "Type " << truetype << " of assigned expression does not conform to declared type " << *O.lookup(name)
            << " of identifier " << name << ".\n";
        return type = *O.lookup(name);
    }
    return type = truetype;
}
Symbol static_dispatch_class::gettruetype() {
    auto truetype = expr->gettruetype();
    if (type_name == SELF_TYPE) {
        classtable->semant_error(global_class, this) << "Static dispatch to SELF_TYPE.\n";
        return type = Object;
    }
    if (undefined(type_name)) {
        classtable->semant_error(global_class, this) << "Static dispatch to undefined class " << type_name << ".\n";
        return type = Object;
    }
    if (undefined(truetype)) return type = Object;
    bool obj = false;
    if (!less_equal(truetype, type_name)) {
        classtable->semant_error(global_class, this)
            << "Expression type " << truetype << " does not conform to declared static dispatch type " << type_name
            << ".\n";
        obj = true;
    }
    method_class *target = 0;
    for (auto xclass : *classtable->name2chain[type_name]) {
        if (xclass->methods->count(name)) target = ismethod((*xclass->methods)[name]);
    }
    if (!target) {
        classtable->semant_error(global_class, this) << "Static dispatch to undefined method " << name << ".\n";
        return type = Object;
    }
    auto i = target->getformals()->begin();
    auto j = actual->begin();
    if ((i != target->getformals()->end()) ^ (j != actual->end())) {
        classtable->semant_error(global_class, this)
            << "Method " << name << " invoked with wrong number of arguments.\n";
    }
    while (i != target->getformals()->end() && j != actual->end()) {
        auto realtype = (*j)->gettruetype();
        if (!less_equal(realtype, (*i)->gettype()))
            classtable->semant_error(global_class, this)
                << "In call of method " << name << ", type " << realtype << " of parameter " << (*i)->getname()
                << " does not conform to declared type " << (*i)->gettype() << ".\n";
        ++i;
        ++j;
        if ((i != target->getformals()->end()) ^ (j != actual->end())) {
            classtable->semant_error(global_class, this)
                << "Method " << name << " invoked with wrong number of arguments.\n";
        }
    }
    if (undefined(target->gettype()) || obj) return type = Object;
    if (target->gettype() == SELF_TYPE) return type = type_name;
    return type = target->gettype();
}
Symbol dispatch_class::gettruetype() {
    auto truetype = expr->gettruetype();
    if (undefined(truetype)) {
        classtable->semant_error(global_class, this) << "Dispatch on undefined class " << truetype << ".\n";
        return type = Object;
    }
    if (truetype == SELF_TYPE) truetype = global_class->getname();
    method_class *target = 0;
    for (auto xclass : *classtable->name2chain[truetype]) {
        if (xclass->methods->count(name)) target = ismethod((*xclass->methods)[name]);
    }
    if (!target) {
        classtable->semant_error(global_class, this) << "Dispatch to undefined method " << name << ".\n";
        return type = Object;
    }
    auto i = target->getformals()->begin();
    auto j = actual->begin();
    if ((i != target->getformals()->end()) ^ (j != actual->end())) {
        classtable->semant_error(global_class, this)
            << "Method " << name << " called with wrong number of arguments.\n";
    }
    while (i != target->getformals()->end() && j != actual->end()) {
        auto realtype = (*j)->gettruetype();
        if (!less_equal(realtype, (*i)->gettype()))
            classtable->semant_error(global_class, this)
                << "In call of method " << name << ", type " << realtype << " of parameter " << (*i)->getname()
                << " does not conform to declared type " << (*i)->gettype() << ".\n";
        ++i;
        ++j;
        if ((i != target->getformals()->end()) ^ (j != actual->end())) {
            classtable->semant_error(global_class, this)
                << "Method " << name << " called with wrong number of arguments.\n";
        }
    }
    if (undefined(target->gettype())) return type = Object;
    if (target->gettype() == SELF_TYPE) {
        if (truetype == global_class->getname())
            return type = SELF_TYPE;
        else
            return type = truetype;
    }
    return type = target->gettype();
}
Symbol cond_class::gettruetype() {
    if (pred->gettruetype() != Bool)
        classtable->semant_error(global_class, this) << "Predicate of 'if' does not have type Bool.\n";
    return type = lowest(then_exp->gettruetype(), else_exp->gettruetype());
}
Symbol loop_class::gettruetype() {
    if (pred->gettruetype() != Bool)
        classtable->semant_error(global_class, this) << "Loop condition does not have type Bool.\n";
    body->gettruetype();
    return type = Object;
}
Symbol typcase_class::gettruetype() {
    auto exptype = expr->gettruetype();
    unordered_set<Symbol> types;
    type = No_type;
    for (auto xbranch : *cases) {
        bool intact = true;
        auto branch = dynamic_cast<branch_class *>(xbranch);
        if (branch->getname() == self) {
            classtable->semant_error(global_class, branch) << "'self' bound in 'case'.\n";
        }
        if (branch->getdecltype() == SELF_TYPE) {
            classtable->semant_error(global_class, branch)
                << "Identifier " << branch->getname() << " declared with type SELF_TYPE in case branch.\n";
            intact = false;
        }
        if (undefined(branch->getdecltype())) {
            classtable->semant_error(global_class, branch)
                << "Class " << branch->getdecltype() << " of case branch is undefined.\n";
            intact = false;
        }
        if (types.count(branch->getdecltype())) {
            classtable->semant_error(global_class, branch)
                << "Duplicate branch " << branch->getdecltype() << " in case statement.\n";
            intact = false;
        }
        if (intact) types.insert(branch->getdecltype());
        if (type == No_type)
            type = branch->gettruetype();
        else
            type = lowest(type, branch->gettruetype());
    }
    return type;
}
Symbol block_class::gettruetype() {
    for (auto exp : *body) type = exp->gettruetype();
    return type;
}
Symbol let_class::gettruetype() {
    if (identifier == self)
        classtable->semant_error(global_class, this) << "'self' cannot be bound in a 'let' expression.\n";
    O.enterscope();
    O.addid(identifier, new Symbol(type_decl));
    auto truetype = init->gettruetype();
    if (undefined(type_decl))
        classtable->semant_error(global_class, this)
            << "Class " << type_decl << " of let-bound identifier " << identifier << " is undefined.\n";
    else if (truetype != No_type && !less_equal(truetype, type_decl))
        classtable->semant_error(global_class, this)
            << "Inferred type " << truetype << " of initialization of " << identifier
            << " does not conform to identifier's declared type " << type_decl << ".\n";
    type = body->gettruetype();
    O.exitscope();
    return type;
}
Symbol plus_class::gettruetype() {
    auto truetypeL = e1->gettruetype(), truetypeR = e2->gettruetype();
    if (truetypeL != Int || truetypeR != Int)
        classtable->semant_error(global_class, this)
            << "non-Int arguments: " << truetypeL << " + " << truetypeR << endl;
    return type = Int;
}
Symbol sub_class::gettruetype() {
    auto truetypeL = e1->gettruetype(), truetypeR = e2->gettruetype();
    if (truetypeL != Int || truetypeR != Int)
        classtable->semant_error(global_class, this)
            << "non-Int arguments: " << truetypeL << " - " << truetypeR << endl;
    return type = Int;
}
Symbol mul_class::gettruetype() {
    auto truetypeL = e1->gettruetype(), truetypeR = e2->gettruetype();
    if (truetypeL != Int || truetypeR != Int)
        classtable->semant_error(global_class, this)
            << "non-Int arguments: " << truetypeL << " * " << truetypeR << endl;
    return type = Int;
}
Symbol divide_class::gettruetype() {
    auto truetypeL = e1->gettruetype(), truetypeR = e2->gettruetype();
    if (truetypeL != Int || truetypeR != Int)
        classtable->semant_error(global_class, this)
            << "non-Int arguments: " << truetypeL << " / " << truetypeR << endl;
    return type = Int;
}
Symbol neg_class::gettruetype() {
    auto truetype = e1->gettruetype();
    if (e1->gettruetype() != Int)
        classtable->semant_error(global_class, this) << "Argument of '~' has type " << truetype << " instead of Int.\n";
    return type = Int;
}
Symbol lt_class::gettruetype() {
    auto truetypeL = e1->gettruetype(), truetypeR = e2->gettruetype();
    if (truetypeL != Int || truetypeR != Int)
        classtable->semant_error(global_class, this)
            << "non-Int arguments: " << truetypeL << " < " << truetypeR << endl;
    return type = Bool;
}
Symbol eq_class::gettruetype() {
    auto truetypeL = e1->gettruetype(), truetypeR = e2->gettruetype();
    if (truetypeL != truetypeR)
        if (truetypeL == Int || truetypeR == Int || truetypeL == Bool || truetypeR == Bool || truetypeL == Str ||
            truetypeR == Str)
            classtable->semant_error(global_class, this) << "Illegal comparison with a basic type.\n";
    return type = Bool;
}
Symbol leq_class::gettruetype() {
    auto truetypeL = e1->gettruetype(), truetypeR = e2->gettruetype();
    if (truetypeL != Int || truetypeR != Int)
        classtable->semant_error(global_class, this)
            << "non-Int arguments: " << truetypeL << " <= " << truetypeR << endl;
    return type = Bool;
}
Symbol comp_class::gettruetype() {
    auto truetype = e1->gettruetype();
    if (e1->gettruetype() != Bool)
        classtable->semant_error(global_class, this)
            << "Argument of 'not' has type " << truetype << " instead of Bool.\n";
    return type = Bool;
}
Symbol int_const_class::gettruetype() { return type = Int; }
Symbol bool_const_class::gettruetype() { return type = Bool; }
Symbol string_const_class::gettruetype() { return type = Str; }
Symbol new__class::gettruetype() {
    if (undefined(type_name)) {
        classtable->semant_error(global_class, this) << "'new' used with undefined class " << type_name << ".\n";
        return type = Object;
    }
    return type = type_name;
}
Symbol isvoid_class::gettruetype() {
    e1->gettruetype();
    return type = Bool;
}
Symbol no_expr_class::gettruetype() { return type = No_type; }
Symbol object_class::gettruetype() {
    if (name == self) return type = SELF_TYPE;
    if (O.lookup(name)) return type = *O.lookup(name);
    classtable->semant_error(global_class, this) << "Undeclared identifier " << name << ".\n";
    return type = Object;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c) { return semant_error(c->get_filename(), c); }

ostream &ClassTable::semant_error(Class_ c, tree_node *t) { return semant_error(c->get_filename(), t); }

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error() {
    semant_errors++;
    return error_stream;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    classtable->check_all();

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}
