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
   void code_text();

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
   CgenNodeP current_class;
   SymbolTable<Symbol,int> local_vars; 
   //Store location as offset from fp in #words(-ve for let decl and +ve for actual params)

   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   ostream& stream() { return str; }
};

class Environment
{
    std::list< std::pair<Symbol, Symbol> > table;
  public:
    int get_index(Symbol identifier);
    Symbol get_class(Symbol identifier);
    void add_identifier(Symbol identifier, Symbol class_name);
    std::pair<Symbol, Symbol> at(int);
    void code(ostream &str);
    int len() { return table.size(); }
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int class_tag;
   

public:
   Environment dispatch_table;
   Environment attr_table;
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
   void code_methods(CgenClassTableP);
   void build_envs();
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

