#ifndef DOCOPT_FISH_GRAMMAR_H
#define DOCOPT_FISH_GRAMMAR_H

#include "docopt_fish_types.h"
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <stdint.h>

namespace docopt_fish
OPEN_DOCOPT_IMPL

using std::vector;
using std::auto_ptr;

/* Usage grammar:
 
 usages = [usage]
 
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
                 
 opt_ellipsis = <empty> |
 ELLIPSIS
 
 options_shortcut = OPEN_SQUARE WORD CLOSE_SQUARE
 
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

/* Base class of all intermediate states */
struct base_t {
    // Which production was used
    uint8_t production;
    
    // Range of tokens used
    range_t token_range;
    
    // Constructors. The default production index is 0. The second constructor specifies a production.
    base_t() : production(0){}
    base_t(uint8_t p) : production(p) {}
};

struct expression_list_t : public base_t {
    vector<expression_t> expressions;
    
    // expression_list = expression opt_expression_list
    expression_list_t() {}
    expression_list_t(const vector<expression_t> &v) : expressions(v) {}
    std::string name() const { return "expression_list"; }
    
    template<typename T>
    void visit_children(T *v) const {
        for (size_t i=0; i < expressions.size(); i++) {
            v->visit(expressions.at(i));
        }
    }
    
    template<typename T>
    expression_list_t *parse(T *);
};

struct alternation_list_t : public base_t {
    vector<expression_list_t> alternations;
    
    alternation_list_t() {}
    alternation_list_t(vector<expression_list_t> v) : alternations(v) {}
    std::string name() const { return "alternation_list"; }
    
    template<typename T>
    void visit_children(T *v) const {
        for (size_t i=0; i < alternations.size(); i++) {
            v->visit(alternations.at(i));
        }
    }

};

struct usage_t : public base_t {
    token_t prog_name;
    alternation_list_t alternation_list;
    
    // usage = <empty>
    usage_t() : base_t(0) {}

    // usage = word alternation_list usage
    usage_t(token_t t, const alternation_list_t &v) : base_t(2), prog_name(t), alternation_list(v) {}
    
    std::string name() const { return "usage"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(prog_name);
        v->visit(alternation_list);
    }
};

struct usages_t : public base_t {
    vector<usage_t> usages;
    
    std::string name() const { return "usages"; }
    
    template<typename T>
    void visit_children(T *v) const {
        for (size_t i=0; i < this->usages.size(); i++) {
            v->visit(this->usages.at(i));
        }
    }
};

struct opt_ellipsis_t : public base_t {
    token_t ellipsis;

    // opt_ellipsis = <empty>
    // opt_ellipsis = ELLIPSIS
    opt_ellipsis_t(token_t t) : base_t(t.empty() ? 0 : 1), ellipsis(t) {}
    
    std::string name() const { return "opt_ellipsis"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(ellipsis);
    }
};

struct options_shortcut_t : public base_t {
    // The options shortcut does not need to remember its token, since we never use it
    options_shortcut_t() : base_t() {}
    
    std::string name() const { return "options_shortcut"; }
    template<typename T>
    void visit_children(T *v UNUSED) const {}
};

template<typename P>
std::auto_ptr<P> auto_copy(const std::auto_ptr<P> &rhs) {
    if (! rhs.get()) {
        return std::auto_ptr<P>(NULL);
    } else {
        return std::auto_ptr<P>(new P(*rhs));
    }
}

struct simple_clause_t : public base_t {
    std::auto_ptr<option_clause_t> option;
    std::auto_ptr<fixed_clause_t> fixed;
    std::auto_ptr<variable_clause_t> variable;
    
    simple_clause_t(std::auto_ptr<option_clause_t> &o) : base_t(0), option(o) {}
    simple_clause_t(std::auto_ptr<fixed_clause_t> &f) : base_t(1), fixed(f) {}
    simple_clause_t(std::auto_ptr<variable_clause_t> &v) : base_t(2), variable(v) {}
    
    std::string name() const { return "simple_clause"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(option);
        v->visit(fixed);
        v->visit(variable);
    }
    
    simple_clause_t(const simple_clause_t &rhs) : option(auto_copy(rhs.option)), fixed(auto_copy(rhs.fixed)), variable(auto_copy(rhs.variable)) {
    }
    
};

struct expression_t : public base_t {
    // production 0
    std::auto_ptr<simple_clause_t> simple_clause;
    
    // Collapsed for productions 1 and 2
    token_t open_token;
    std::auto_ptr<alternation_list_t> alternation_list;
    token_t close_token;
    
    // Collapsed for all
    std::auto_ptr<opt_ellipsis_t> opt_ellipsis;
    
    std::auto_ptr<options_shortcut_t> options_shortcut;
    
    expression_t(const expression_t &rhs) : simple_clause(auto_copy(rhs.simple_clause)), open_token(rhs.open_token), alternation_list(auto_copy(rhs.alternation_list)), close_token(rhs.close_token), opt_ellipsis(auto_copy(rhs.opt_ellipsis)), options_shortcut(auto_copy(rhs.options_shortcut)) {
        
    }
    
    //expression = simple_clause
    expression_t(std::auto_ptr<simple_clause_t> c, std::auto_ptr<opt_ellipsis_t> e) : base_t(0), simple_clause(c), opt_ellipsis(e) {}
    
    //expression = OPEN_PAREN expression_list CLOSE_PAREN opt_ellipsis |
    //expression = OPEN_SQUARE expression_list CLOSE_SQUARE opt_ellipsis
    expression_t(token_t a, bool is_paren, std::auto_ptr<alternation_list_t> el, token_t b, std::auto_ptr<opt_ellipsis_t> e)
    : base_t(is_paren ? 1  : 2), open_token(a), alternation_list(el), close_token(b), opt_ellipsis(e)
    {}
    
    //expression = options_shortcut
    expression_t(std::auto_ptr<options_shortcut_t> os) : base_t(3), options_shortcut(os)
    {}
    
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
struct option_clause_t : public base_t {
    const token_t word;
    const option_t option;
    option_clause_t(const token_t &t, const option_t &o) : word(t), option(o) {}
    std::string name() const { return "option"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(word);
    }
};

// Fixed like 'checkout'
struct fixed_clause_t : public base_t {
    const token_t word;
    fixed_clause_t(const token_t &t) : word(t) {}
    std::string name() const { return "fixed"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(word);
    }

};

// Variable like '<branch>'
struct variable_clause_t : public base_t {
    const token_t word;
    variable_clause_t(const token_t &t) : word(t) {}
    std::string name() const { return "variable"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(word);
    }
};

template<typename string_t>
usages_t *parse_usage(const string_t &src, const range_t &src_range, const option_list_t &shortcut_options, vector<error_t<string_t> > *out_errors);

// Node visitor class, using CRTP. Child classes should override accept().
template<typename T>
struct node_visitor_t {
    /* Additional overrides */
    template<typename NODE_TYPE>
    void visit_internal(const NODE_TYPE &node)
    {
        T *derived_this = static_cast<T *>(this);
        derived_this->accept(node);
        node.visit_children(derived_this);
    }
    
    template<typename NODE_TYPE>
    void visit_internal(const std::auto_ptr<NODE_TYPE> &node)
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
    void visit(const token_t &token) {
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
