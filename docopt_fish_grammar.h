#ifndef DOCOPT_FISH_GRAMMAR_H
#define DOCOPT_FISH_GRAMMAR_H

#include "docopt_fish_types.h"
#include <string>
#include <vector>
#include <map>
#include <stdint.h>

namespace docopt_fish
OPEN_DOCOPT_IMPL

using std::vector;
using std::unique_ptr;

/* Usage grammar:
  
 usage = WORD alternation_list

 alternation_list = [expression_list] # logical ORs of values
 
 expression_list = [expression] # concatenation
 
 expression = simple_clause opt_ellipsis |
              OPEN_PAREN alternation_list CLOSE_PAREN opt_ellipsis |
              OPEN_SQUARE alternation_list CLOSE_SQUARE opt_ellipsis
              options_shortcut
 
 simple_clause = option_clause |
                 fixed_clause |
                 variable_clause
                 
 option_clause = WORD
 fixed_clause = WORD
 variable_clause = WORD
                 
 opt_ellipsis = <empty> | ELLIPSIS
 
 options_shortcut = "[options]"
 
 */

#pragma mark -
#pragma mark Usage Grammar
#pragma mark -

struct alternation_list;
struct expression_list_t;
struct expression_t;
struct simple_clause_t;
struct opt_ellipsis_t;
struct option_clause_t;
struct fixed_clause_t;
struct variable_clause_t;

struct expression_list_t {
    vector<expression_t> expressions;
    
    // expression_list = expression opt_expression_list
    expression_list_t() {}
    std::string name() const { return "expression_list"; }
    
    template<typename T>
    void visit_children(T *v) const {
        for (const auto &expr : expressions) {
            v->visit(expr);
        }
    }
    
    template<typename T>
    expression_list_t *parse(T *);
};

struct alternation_list_t {
    vector<expression_list_t> alternations;
    
    std::string name() const { return "alternation_list"; }
    
    template<typename T>
    void visit_children(T *v) const {
        for (const auto &altern : alternations) {
            v->visit(altern);
        }
    }
    
    void assign_corresponding_option_long_names();

};

struct usage_t {
    rstring_t prog_name;
    alternation_list_t alternation_list;
    
    std::string name() const { return "usage"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(prog_name);
        v->visit(alternation_list);
    }
    
    /* Turn the receiver into a "default" usage that has an empty program name and just the [options] portion. */
    void make_default();
    
    /* Hackish function to build a parse tree from a (possibly empty) list of variable names, optionally including an options shortcut.
       Each variable becomes an element in an alternation list.
     */
    void set_from_variables(const std::vector<rstring_t> &variables, bool include_options_shortcut);
};

struct opt_ellipsis_t {
    rstring_t ellipsis;
    bool present = false;

    std::string name() const { return "opt_ellipsis"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(ellipsis);
    }
};

struct options_shortcut_t {
    // The options shortcut does not need to remember its token, since we never use it
    // It's always assuemd to be present
    bool present = false;
    
    std::string name() const { return "options_shortcut"; }
    template<typename T>
    void visit_children(T *v UNUSED) const {}
};

struct simple_clause_t {
    unique_ptr<option_clause_t> option;
    unique_ptr<fixed_clause_t> fixed;
    unique_ptr<variable_clause_t> variable;
    
    std::string name() const { return "simple_clause"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(option);
        v->visit(fixed);
        v->visit(variable);
    }
};

struct expression_t {
    // production 0
    unique_ptr<simple_clause_t> simple_clause;
    
    // Collapsed for productions 1 and 2
    rstring_t open_token;
    unique_ptr<alternation_list_t> alternation_list;
    rstring_t close_token;
    
    // Collapsed for all
    opt_ellipsis_t opt_ellipsis;
    
    // may or may not be present
    options_shortcut_t options_shortcut;
    
    // Invariant: at most one of (simple_clause, alternation_list) may be set
    uint8_t production = uint8_t(-1);
    
    std::string name() const { return "expression"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(simple_clause);
        v->visit(open_token);
        v->visit(alternation_list);
        v->visit(close_token);
        v->visit(opt_ellipsis);
        v->visit(options_shortcut);
    }
};

// An option like '--track', optionally with a value
struct option_clause_t {
    rstring_t word;
    option_t option;
    std::string name() const { return "option"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(word);
    }
};

// Fixed like 'checkout'
struct fixed_clause_t {
    rstring_t word;
    std::string name() const { return "fixed"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(word);
    }

};

// Variable like '<branch>'
struct variable_clause_t {
    rstring_t word;
    std::string name() const { return "variable"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(word);
    }
};

bool parse_one_usage(const rstring_t &src, const option_list_t &shortcut_options, usage_t *out_usage, vector<error_t> *out_errors);


// Node visitor class, using CRTP. Child classes should override accept().
template<typename T>
struct node_visitor_t {
    template<typename NODE_TYPE>
    void visit_internal(const NODE_TYPE &node)
    {
        T *derived_this = static_cast<T *>(this);
        derived_this->accept(node);
        node.visit_children(derived_this);
    }
    
    template<typename NODE_TYPE>
    void visit_internal(const unique_ptr<NODE_TYPE> &node)
    {
        if (node.get()) {
            this->visit_internal(*node);
        }
    }
    
    
    /* Function called from overrides of visit_children. We invoke an override of accept(), and then recurse to children. */
    template<typename NODE_TYPE>
    void visit(const NODE_TYPE &t)
    {
        this->visit_internal(t);
    }
    
    /* Visit is called from node visit_children implementations. A token has no children. */
    void visit(const rstring_t &token) {
        static_cast<T *>(this)->accept(token);
    }
    
    /* Public entry point */
    template<typename ENTRY_TYPE>
    void begin(const ENTRY_TYPE &t) {
        this->visit_internal(t);
    }
};


CLOSE_DOCOPT_IMPL


#endif
