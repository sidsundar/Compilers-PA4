#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <list>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int last_tag = 3;


   CgenNodeP lookup_by_tag(int tag);
// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_proto_objects();
   void code_classnames();
   void code_dispatch_tables();

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
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};

class DispatchTable
{
    std::list< std::pair<Symbol, Symbol> > table;
  public:
    int get_index(Symbol method_name);
    Symbol get_dispatch(Symbol method_name);
    void add_method(Symbol method_name, Symbol class_name);
    void code(ostream &str);
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int class_tag;
   

public:
   DispatchTable dt;
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table,
            int tag);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   Symbol get_name()  { return name; }
   int get_tag() { return class_tag; }
   void code_proto_object(ostream& str);
   int code_proto_attrs(ostream& str, bool print=true);
   void code_methods(ostream& str);
   void build_dispatch_table();
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

