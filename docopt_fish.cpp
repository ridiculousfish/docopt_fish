#include "docopt_fish.h"
#include <memory>
#include <assert.h>
#include <cstring>
#include <cstdint>
#include <iostream>

using namespace docopt_fish;

using std::auto_ptr;

static const size_t npos = (size_t)(-1);

/* Overloads */
static void assign_narrow_string_to_string(const char *s, std::string *result) {
    *result = s;
}

static void assign_narrow_string_to_string(const char *s, std::wstring *result) {
    size_t len = std::strlen(s);
    for (size_t i=0; i < len; i++) {
        char c = s[i];
        assert(c <= 127); //ASCII only
        result->push_back(wchar_t(c));
    }
}

// Need a wstring implementation of this
static size_t find_case_insensitive(const std::string &haystack, const char *needle, size_t haystack_start) {
    assert(haystack_start < haystack.size());
    const char *haystack_cstr = haystack.c_str();
    const char *found = strcasestr(haystack_cstr + haystack_start, needle);
    return found ? found - haystack_cstr : std::string::npos;
}

/* Class representing a range of a string */
struct range_t {
    size_t start;
    size_t length;
    range_t() : start(0), length(0)
    {}
    range_t(size_t s, size_t l) : start(s), length(l)
    {}
    
    size_t end() const {
        size_t result = start + length;
        assert(result >= start); //don't overflow
        return result;
    }
    
    bool empty() const {
        return length == 0;
    }
    
    bool operator==(const range_t &rhs) const {
        return this->start == rhs.start && this->length == rhs.length;
    }
    
    // Range comparison so these can be used as dictionary keys
    bool operator<(const range_t &rhs) const {
        if (this->start != rhs.start) {
            return this->start < rhs.start;
        }
        return this->length < rhs.length;
    }
    
    // Merges a range into this range. Empty ranges are discarded.
    void merge(const range_t &rhs) {
        if (this->empty()) {
            *this = rhs;
        } else if (! rhs.empty()) {
            this->start = std::min(this->start, rhs.start);
            this->length = std::max(this->end(), rhs.end()) - this->start;
        }
    }
};
typedef std::vector<range_t> range_list_t;

/* A token is just a range of some string, with a type */
struct token_t {
    enum type_t {
        none = '_',
        open_paren = '(',
        close_paren = ')',
        open_square = '[',
        close_square = ']',
        ellipsis = '.', // represents three dots ...
        bar = '|',
        newline = 'n', //newline,
        options = 'o', //represents [options]
        word='X' // represents a word, possibly with angle brackets <foo>
    };
    
    range_t range;
    type_t type;
    
    /* Constructor */
    token_t(size_t a, size_t b, char type_char) : range(a, b)
    {
        assert(strchr("()[]|.noX", type_char));
        this->type = static_cast<type_t>(type_char);
    }
    
    token_t(size_t a, size_t b, type_t t) : range(a, b), type(t)
    {
    }
    
    token_t() : range(0, 0), type(none)
    {}
    
    bool empty() const {
        return range.length == 0;
    }
    
    // For debugging
    const char *name() const {
        switch (this->type) {
            case none: return "<NONE>";
            case open_paren: return "(";
            case close_paren: return ")";
            case open_square: return "[";
            case close_square: return "]";
            case bar: return "|";
            case options: return "[options]";
            case word: return "<word>";
            default: return NULL;
        }
    }
};
typedef std::vector<token_t> token_list_t;

/* An option represents something like '--foo=bar' */
struct option_t {
    enum type_t {
        single_short, // '-f'
        single_long, // '-foo'
        double_long // '--foo'
    } type;
    
    // name of the option, like '--foo'
    range_t name;
    
    // value of the option. Empty for no value.
    range_t value;
    
    // Range of the description. Empty for none.
    range_t description_range;
    
    // Whether we require an = sign to match
    bool require_equals;
    
    option_t(const range_t &n, const range_t &v, size_t leading_dash_count, bool requals = false) : name(n), value(v), description_range(0, 0), require_equals(requals) {
        // Set the type. If there is only one dash, we infer single_long and single_short by the length of the name
        if (leading_dash_count > 1) {
            this->type = option_t::double_long;
        } else if (n.length > 1) {
            this->type = option_t::single_long;
        } else {
            this->type = option_t::single_short;
        }
    }
    
    /* We want a value if we have a non-empty value range */
    bool has_value() const {
        return ! value.empty();
    }
    
    bool operator==(const option_t &rhs) const {
        return this->type == rhs.type && this->name == rhs.name && this->value == rhs.value;
    }
    
    /* Helper function for dumping */
    template<typename string_t>
    string_t describe(const string_t &src) const {
        string_t result;
        result.append(src, name.start, name.length);
        if (! value.empty()) {
            result.push_back(':');
            result.push_back(' ');
            result.append(src, name.start, name.length);
        }
        return result;
    }
};
typedef std::vector<option_t> option_list_t;

/* Hide open and close brackets to avoid an annoying leading indent inside our class */
#define OPEN_DOCOPT_IMPL {
#define CLOSE_DOCOPT_IMPL }


#pragma mark -
#pragma mark Usage Grammar
#pragma mark -

/* Usage grammar:
 
    usage = <empty> |
            WORD expression_list usage

    expression_list = expression opt_expression_list
 
    opt_expression_list = <empty> |
                          expression_list
 
    expression = compound_clause or_continuation
 
    or_continuation = <empty> |
                      VERT_BAR expression
 
    compound_clause = simple_clause opt_ellipsis |
                      OPEN_PAREN expression_list CLOSE_PAREN opt_ellipsis |
                      OPEN_SQUARE expression_list CLOSE_SQUARE opt_ellipsis
                      options_shortcut
 
    simple_clause = WORD
 
    opt_ellipsis = <empty> |
                   ELLIPSIS
                   
    options_shortcut = OPEN_SQUARE WORD CLOSE_SQUARE
 
*/

class expression_list_t;
class opt_expression_list_t;
class expression_t;
class or_continuation_t;
class compound_clause_t;
class simple_clause_t;
class opt_ellipsis_t;

/* Context passed around in our recursive descent parser */
struct parse_context_t {
    const token_list_t tokens;
    range_t remaining_range;
    
    parse_context_t(token_list_t &lst) : tokens(lst), remaining_range(0, lst.size())
    {}

    /* Returns true if there are no more next tokens */
    bool is_at_end() const {
        return remaining_range.length == 0;
    }
    
    /* Returns true if there is a next token, and it has the given type */
    bool next_token_has_type(token_t::type_t t) const {
        bool result = false;
        if (! this->is_at_end()) {
            result = tokens.at(remaining_range.start).type == t;
        }
        return result;
    }
    
    token_t::type_t next_token_type() const {
        if (this->is_at_end()) {
            return token_t::none;
        } else {
            return tokens.at(remaining_range.start).type;
        }
    }

    /* Returns the next token, decreasing the remaining range. Requires that there be a next token */
    token_t acquire_token() {
        assert(! this->is_at_end());
        size_t idx = remaining_range.start;
        remaining_range.start += 1;
        remaining_range.length -= 1;
        return tokens.at(idx);
    }
};


/* Helpers to parse when the productions are fixed. */
template<typename PARENT, typename CHILD>
PARENT *parse_1(parse_context_t *ctx) {
    PARENT *result = NULL;
    auto_ptr<CHILD> child(CHILD::parse(ctx));
    if (child.get() != NULL) {
        result = new PARENT(child);
    }
    return result;
}

template<typename PARENT, typename CHILD1, typename CHILD2>
PARENT *parse_2(parse_context_t *ctx) {
    PARENT *result = NULL;
    auto_ptr<CHILD1> child1(CHILD1::parse(ctx));
    if (child1.get()) {
        auto_ptr<CHILD2> child2(CHILD2::parse(ctx));
        if (child2.get()) {
            result = new PARENT(child1, child2);
        }
    }
    return result;
}

template<typename PARENT, typename CHILD>
PARENT *parse_1_or_empty(parse_context_t *ctx) {
    PARENT *result = NULL;
    auto_ptr<CHILD> child(CHILD::parse(ctx));
    if (child.get()) {
        result = new PARENT(child);
    } else {
        result = new PARENT();
    }
    return result;
}

// Node visitor class, using CRTP. Child classes should override accept(
template<typename T>
struct node_visitor_t {
    /* Additional overrides */
    template<typename IGNORED_TYPE>
    void will_visit_children(const IGNORED_TYPE& t) {}
    
    template<typename IGNORED_TYPE>
    void did_visit_children(const IGNORED_TYPE& t) {}

    template<typename NODE_TYPE>
    void visit_internal(const NODE_TYPE &node)
    {
        T *derived_this = static_cast<T *>(this);
        derived_this->accept(node);
        derived_this->will_visit_children(node);
        node.visit_children(this);
        derived_this->did_visit_children(node);
    }
    
    
    /* Function called from overrides of visit_children. We invoke an override of accept(), and then recurse to children. */
    template<typename NODE_TYPE>
    void visit(const NODE_TYPE &t)
    {
        if (t.get() != NULL) {
            this->visit_internal(*t);
        }
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

/* Helper class for pretty-printing */
class node_dumper_t : public node_visitor_t<node_dumper_t> {
    std::string text;
    unsigned int depth;
    
    // Instances of this class are expected to be quite transient. src is unowned.
    const std::string *src;
    
    std::vector<std::string> lines;
    
public:
    template<typename NODE_TYPE>
    void accept(const NODE_TYPE& node) {
        std::string result(2 * depth, ' ');
        result.append(node.name());
        lines.push_back(result);
    }
    
    template<typename NODE_TYPE>
    void will_visit_children(const NODE_TYPE &t) {
        depth = depth + 1 - t.unindent();
    }
    
    template<typename NODE_TYPE>
    void did_visit_children(const NODE_TYPE &t) {
        depth = depth - 1 + t.unindent();
    }

    
    void accept(const token_t &t1) {
        if (t1.type != token_t::none) {
            std::string result(2 * depth, ' ');
            char buff[32];
            
            if (t1.type == token_t::word && src != NULL) {
                const std::string word(*src, t1.range.start, t1.range.length);
                snprintf(buff, sizeof buff, "'%s' ", word.c_str());
            } else {
                snprintf(buff, sizeof buff, "'%s' ", t1.name());
            }
            result.append(buff);
            
            if (t1.range.length == 1) {
                snprintf(buff, sizeof buff, "{%lu}", t1.range.start);
            } else {
                snprintf(buff, sizeof buff, "{%lu-%lu}", t1.range.start, t1.range.length);
            }
            result.append(buff);
            lines.push_back(result);
        }
    }

    template<typename NODE_TYPE>
    static std::string dump_tree(const NODE_TYPE &node, const std::string &src) {
        node_dumper_t dumper;
        dumper.src = &src;
        dumper.begin(node);
        std::string result;
        for (size_t i=0; i < dumper.lines.size(); i++) {
            result.append(dumper.lines.at(i));
            result.push_back('\n');
        }
        dumper.src = NULL;
        return result;
    }
};

/* Base class of all intermediate states */
struct base_t {
    // Which production was used
    uint8_t production;
    
    // Range of tokens used
    range_t token_range;
    
    // Constructors. The default production index is 0. The second constructor specifies a production.
    base_t() : production(0){}
    base_t(uint8_t p) : production(p) {}
    
    // How much to un-indent children when pretty-printing
    unsigned unindent() const { return 0; }
};

struct expression_list_t : public base_t {
    auto_ptr<expression_t> expression;
    auto_ptr<opt_expression_list_t> opt_expression_list;
    
    // expression_list = expression opt_expression_list
    expression_list_t(auto_ptr<expression_t> c1, auto_ptr<opt_expression_list_t> c2) : expression(c1), opt_expression_list(c2) {}
    static expression_list_t *parse(parse_context_t *ctx) {  return parse_2<expression_list_t, expression_t, opt_expression_list_t>(ctx); }
    std::string name() const { return "expression_list"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(expression);
        v->visit(opt_expression_list);
    }
    
    /* Don't indent children of opt_expression_list to keep the list looking flatter */
    unsigned unindent() const { return 1*0; }
};


struct usage_t : public base_t {
    token_t prog_name;
    auto_ptr<expression_list_t> expression_list;
    auto_ptr<usage_t> next_usage;

    // usage = <empty>
    usage_t() : base_t(0) {}
    
    // usage = word expression_list usage
    usage_t(token_t t, auto_ptr<expression_list_t> c, auto_ptr<usage_t> next) : base_t(1), prog_name(t), expression_list(c), next_usage(next) {}
    
    static usage_t *parse(parse_context_t *ctx) {
        // If we reach the end, we return an empty usage
        if (ctx->is_at_end()) {
            return new usage_t();
        }
        
        usage_t *result = NULL;
        
        // TODO: generate error for missing word name
        if (ctx->next_token_has_type(token_t::word)) {
            token_t p = ctx->acquire_token();
            auto_ptr<expression_list_t> el(expression_list_t::parse(ctx));
            if (el.get()) {
                // Consume as many newlines as we can, and then try to generate the tail
                while (ctx->next_token_has_type(token_t::newline)) {
                    ctx->acquire_token();
                }
                
                auto_ptr<usage_t> next(usage_t::parse(ctx));
                result = new usage_t(p, el, next);
            }
        }
        return result;
    }
    
    std::string name() const { return "usage"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(prog_name);
        v->visit(expression_list);
        v->visit(next_usage);
    }
};

struct opt_expression_list_t : public base_t {
    auto_ptr<expression_list_t> expression_list;
    
    // opt_expression_list = empty
    opt_expression_list_t() {}
    
    // opt_expression_list = expression_list
    opt_expression_list_t(auto_ptr<expression_list_t> c) : base_t(1), expression_list(c) {}
    static opt_expression_list_t *parse(parse_context_t *ctx) { return parse_1_or_empty<opt_expression_list_t, expression_list_t>(ctx); }
    std::string name() const { return "opt_expression_list"; }
    unsigned unindent() const { return 1*0; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(expression_list);
    }
};

struct expression_t : public base_t {
    auto_ptr<compound_clause_t> compound_clause;
    auto_ptr<or_continuation_t> or_continuation;
    
    // expression = compound_clause or_continuation
    expression_t(auto_ptr<compound_clause_t> c, auto_ptr<or_continuation_t> oc) : compound_clause(c), or_continuation(oc) {}
    static expression_t *parse(parse_context_t *ctx) { return parse_2<expression_t, compound_clause_t, or_continuation_t>(ctx); }
    std::string name() const { return "expression"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(compound_clause);
        v->visit(or_continuation);
    }
};

struct or_continuation_t : public base_t {
    token_t vertical_bar;
    auto_ptr<expression_t> expression;
    
    // or_continuation = <empty>
    or_continuation_t() {}
    
    // or_continuation =  VERT_BAR or_clause
    or_continuation_t(token_t b, auto_ptr<expression_t> c) : base_t(1), vertical_bar(b), expression(c) {}
    
    static or_continuation_t *parse(parse_context_t *ctx) {
        or_continuation_t *result = NULL;
        if (ctx->next_token_has_type(token_t::bar)) {
            token_t token = ctx->acquire_token();
            auto_ptr<expression_t> c(expression_t::parse(ctx));
            if (c.get()) {
                result = new or_continuation_t(token, c);
            } else {
                // TODO: generate an error about trailing bar
                fprintf(stderr, "Trailing bar\n");
            }
        } else {
            result = new or_continuation_t();
        }
        return result;
    }
    
    std::string name() const { return "or_continuation"; }
    
    template<typename T>
    void visit_children(T *v) const {
        v->visit(vertical_bar);
        v->visit(expression);
    }
};

struct opt_ellipsis_t : public base_t {
    token_t ellipsis;
    
    // opt_ellipsis = <empty>
    opt_ellipsis_t() {}
    
    // opt_ellipsis = ELLIPSIS
    opt_ellipsis_t(token_t t) : base_t(1), ellipsis(t) {}
    
    static opt_ellipsis_t *parse(parse_context_t *ctx) {
        opt_ellipsis_t *result = NULL;
        if (ctx->next_token_has_type(token_t::ellipsis)) {
            result = new opt_ellipsis_t(ctx->acquire_token());
        } else {
            result = new opt_ellipsis_t();
        }
        return result;
    }
    
    std::string name() const { return "opt_ellipsis"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(ellipsis);
    }
};

struct options_shortcut_t : public base_t {
    // The options shortcut does not need to remember its token, since we never use it
    options_shortcut_t(const token_t &t1) : base_t() {}
    
    static options_shortcut_t *parse(parse_context_t *ctx) {
        if (! ctx->next_token_has_type(token_t::options)) {
            return NULL;
        }
        return new options_shortcut_t(ctx->acquire_token());
    }
    
    std::string name() const { return "options_shortcut"; }
    template<typename T>
    void visit_children(T *v) const {}
};

struct simple_clause_t : public base_t {
    token_t word;
    
    simple_clause_t(const token_t &t) : word(t)
    {}
    
    static simple_clause_t *parse(parse_context_t *ctx) {
        if (! ctx->next_token_has_type(token_t::word)) {
            return NULL;
        }
        token_t word = ctx->acquire_token();
        return new simple_clause_t(word);
    }

    std::string name() const { return "simple_clause"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(word);
    }

};

struct compound_clause_t : public base_t {
    // production 0
    auto_ptr<simple_clause_t> simple_clause;
    
    // Collapsed for productions 1 and 2
    token_t open_token;
    auto_ptr<expression_list_t> expression_list;
    token_t close_token;
    
    // Collapsed for all
    auto_ptr<opt_ellipsis_t> opt_ellipsis;
    
    auto_ptr<options_shortcut_t> options_shortcut;
    
    //compound_clause = simple_clause
    compound_clause_t(auto_ptr<simple_clause_t> c, auto_ptr<opt_ellipsis_t> e) : base_t(0), simple_clause(c), opt_ellipsis(e) {}

    //compound_clause = OPEN_PAREN expression_list CLOSE_PAREN opt_ellipsis |
    //compound_clause = OPEN_SQUARE expression_list CLOSE_SQUARE opt_ellipsis
    compound_clause_t(token_t a, auto_ptr<expression_list_t> el, token_t b, auto_ptr<opt_ellipsis_t> e)
        : base_t(a.type == token_t::open_paren ? 1  : 2), open_token(a), expression_list(el), close_token(b), opt_ellipsis(e)
    {}
    
    //compound_clause = options_shortcut
    compound_clause_t(auto_ptr<options_shortcut_t> os) : base_t(3), options_shortcut(os)
    {}

    static compound_clause_t *parse(parse_context_t * ctx) {
        compound_clause_t *result = NULL;
        if (ctx->is_at_end()) {
            fprintf(stderr, "At end\n");
            return NULL;
        }
        
        switch (ctx->next_token_type()) {
            case token_t::open_square:
            case token_t::open_paren:
            {
                token_t init_token = ctx->acquire_token();
                auto_ptr<expression_list_t> contents(expression_list_t::parse(ctx));
                if (contents.get()) {
                    bool is_paren = (init_token.type == token_t::open_paren);
                    token_t::type_t close_type = (is_paren ? token_t::close_paren : token_t::close_square);
                    if (ctx->next_token_has_type(close_type)) {
                        token_t close_token = ctx->acquire_token();
                        auto_ptr<opt_ellipsis_t> ellipsis(opt_ellipsis_t::parse(ctx));
                        assert(ellipsis.get() != NULL); // should never fail
                        result = new compound_clause_t(init_token, contents, close_token, ellipsis);
                    } else {
                        // TODO: generate error of unclosed paren
                    }
                }
                break;
            }
            
            case token_t::options:
            {
                auto_ptr<options_shortcut_t> shortcut(options_shortcut_t::parse(ctx));
                if (shortcut.get()) {
                    result = new compound_clause_t(shortcut);
                }
                break;
            }
            
            case token_t::close_paren:
            case token_t::close_square:
            case token_t::newline:
                result = NULL;
                break;
                
            case token_t::ellipsis:
            case token_t::bar:
                // Indicates leading ellipsis / bar
                // TODO: generate error
                break;
                
            case token_t::word:
            {
                auto_ptr<simple_clause_t> simple_clause(simple_clause_t::parse(ctx));
                if (simple_clause.get()) {
                    auto_ptr<opt_ellipsis_t> ellipsis(opt_ellipsis_t::parse(ctx));
                    assert(ellipsis.get() != NULL); // should never fail
                    result = new compound_clause_t(simple_clause, ellipsis);
                }
                break;
            }
            
            case token_t::none:
                assert(0 && "none-type token returned from next_token_type when not at end of token stream");
                break;
        }
        return result;
    }
    
    std::string name() const { return "compound_clause"; }
    template<typename T>
    void visit_children(T *v) const {
        v->visit(simple_clause);
        v->visit(open_token);
        v->visit(expression_list);
        v->visit(close_token);
        v->visit(opt_ellipsis);
        v->visit(options_shortcut);
    }
};

#pragma mark -


/* Helper class for collecting options from a tree */
template<typename NODE_TYPE>
struct node_collector_t : public node_visitor_t<node_collector_t<NODE_TYPE> > {
    std::vector<const NODE_TYPE *> results;
    
    // The requested type we capture
    void accept(const NODE_TYPE& node) {
        results.push_back(&node);
    }
    
    // Other types we ignore
    template<typename IGNORED_TYPE>
    void accept(const IGNORED_TYPE& t) {}

    
};

/* Helper class for collecting all nodes of a given type */
template<typename ENTRY_TYPE, typename TYPE_TO_COLLECT>
std::vector<const TYPE_TO_COLLECT *> collect_nodes(const ENTRY_TYPE &entry) {
    node_collector_t<TYPE_TO_COLLECT> collector;
    collector.begin(entry);
    std::vector<const TYPE_TO_COLLECT *> result;
    result.swap(collector.results);
    return result;
}


/* Wrapper template class that takes either a string or wstring as string_t */
template<typename string_t>
class docopt_impl OPEN_DOCOPT_IMPL

/* Class representing an error */
struct error_t {
    /* Where the error occurred */
    size_t location;
    
    /* Text of the error */
    string_t text;
};
typedef std::vector<error_t> error_list_t;

/* Constructor takes the source */
public:
const string_t source;
docopt_impl(const string_t &s) : source(s)
{}

/* Helper function to make a string (narrow or wide) from a C string */
static string_t string_from_narrow_string(const char *s) {
    string_t result;
    assign_narrow_string_to_string(s, &result);
    return result;
}

/* List of errors */
error_list_t errors;
static void append_error(error_list_t *errors, size_t where, const char *txt) {
    if (errors != NULL) {
        errors->resize(errors->size() + 1);
        error_t *error = &errors->back();
        error->location = where;
        assign_narrow_string_to_string(txt, &error->text);
    }
}


/* Helper typedefs */
typedef base_argument_t<string_t> arg_t;
typedef std::vector<string_t> string_list_t;
typedef typename string_t::value_type char_t;

/* Immutable class representing an optional string */
class optional_string_t {
public:
    bool is_missing;
    string_t ivalue;
    
    optional_string_t() : is_missing(true) {}
    optional_string_t(const string_t &s) : is_missing(false), ivalue(s) {}
    
    void operator=(const string_t &s) {
        is_missing = false;
        ivalue = s;
    }
    
    bool missing() const {
        return is_missing;
    }
    
    const string_t &value() const {
        assert(! this->is_missing);
        return ivalue;
    }
    
    const char_t *c_str_or_empty() const {
        // A missing optional string has an empty underlying string
        return ivalue.c_str();
    }
};


/* A positional argument */
struct positional_argument_t {
    string_t value;
    
    positional_argument_t(const string_t &s) : value(s)
    {}
};
typedef std::vector<positional_argument_t> positional_argument_list_t;

/* A resolved option with a name and optionally a value */
struct resolved_option_t {
    option_t option;
    optional_string_t value;
    
    resolved_option_t(const option_t &opt, const optional_string_t &val) : option(opt), value(val)
    {}
};
typedef std::vector<resolved_option_t> resolved_option_list_t;

/* Helper function to efficiently iterate over lines of a string 'source'. On input, inout_range.end() should be initialized to the start point for the iteration. On return, the range will contain the line, with its end pointing just after the trailing newline, or possibly at source.size(). The length of the line is thus the length of the range (and is guaranteed to be positive). Returns true if a line was returned, false if we reached the end.

    If end is specified, it is treated as the end of the string (i.e. str.size())
*/
static bool get_next_line(const string_t &str, range_t *inout_range, size_t end = string_t::npos)
{
    const size_t effective_end = std::min(str.size(), end);
    bool success = false;
    if (inout_range->end() < effective_end)
    {
        // Start at the end of the last line, or zero if this is the first call
        size_t line_start = inout_range->end();
        size_t line_end;
        size_t newline = str.find(char_t('\n'), line_start);
        if (newline > effective_end) {
            // Point just after the last character
            line_end = effective_end;
        } else {
            // Point just after the newline
            line_end = newline + 1;
        }
        // Empty lines are impossible
        assert(line_end > line_start);
        inout_range->start = line_start;
        inout_range->length = line_end - line_start;
        success = true;
    }
    return success;
}

/* Helper function for string containment. Returns true if c is found in cstr, which is ASCII. */
static bool contains(const char *cstr, char_t c) {
    assert(c != 0);
    for (size_t i=0; cstr[i]; i++) {
        if (cstr[i] == c) {
            return true;
        }
    }
    return false;
}

/* Returns true if the two strings are equal. */
static bool str_equals(const char *a, const string_t &str) {
    size_t len = str.size();
    const char_t *cstr = str.c_str();
    for (size_t i=0; i < len; i++) {
        // See if we've either reached the end of a, or the characters are different
        if (a[i] == '\0' || cstr[i] != a[i]) {
            return false;
        }
    }
    
    // We made it through. Ensure they have the same lengths
    // Note that we already dereferenced the previous characters of a
    return a[len] == '\0';
}

/* Helper function for string equality. Returns true if a and b are equal up to the first len characters */
static bool substr_equals(const char *a, const char_t *b, size_t len) {
    bool equals = true;
    for (size_t i=0; i < len; i++) {
        if (a[i] != b[i]) {
            equals = false;
            break;
        }
        if (a[i] == 0) {
            // End of string. Note that since these characters were equal, it's sufficient to test one of them.
            break;
        }
    }
    return equals;
}

/* Helper function for string equality. Returns true if a and b are equal up to the first len characters */
static bool substr_equals(const char *a, const string_t &b, size_t len) {
    return substr_equals(a, b.c_str(), len);
}

/* Copies out the string of a range */
string_t string_for_range(const range_t &range) {
    assert(range.end() < this->source.length());
    return string_t(this->source, range.start, range.length);
}

/* Copies out the string of a token */
string_t string_for_token(const token_t &tok) {
    return string_for_range(tok.range);
}

/* Copies out the string of a token */
void set_string_for_token(const token_t &tok, string_t *result) {
    assert(tok.range.end() < this->source.length());
    result->assign(this->source, tok.range.start, tok.range.length);
}


/* Returns true if the given token matches the given narrow string, up to len characters */
bool token_substr_equals(const token_t &tok, const char *str, size_t len) {
    assert(tok.range.end() <= this->source.length());
    if (len > tok.range.length) {
        /* If our token is too short, then it doesn't match */
        return false;
    } else {
        return substr_equals(str, this->source.c_str() + tok.range.start, len);
    }
}

/* Returns true if the given token matches the given string */
bool token_equals(const token_t &tok, const string_t &str) {
    assert(tok.range.end() < this->source.length());
    if (str.size() != tok.range.length) {
        /* If our token is too short, then it doesn't match */
        return false;
    } else {
        return this->source.compare(tok.range.start, tok.range.length, str) == 0;
    }
}

/* Tokenize usage specifications from 'source' */
token_list_t tokenize_usage() {
    token_list_t tokens;
    const range_list_t usage_sections = this->source_ranges_for_section("Usage:");
    for (size_t usage_idx = 0; usage_idx < usage_sections.size(); usage_idx++) {
        const range_t &usage_range = usage_sections.at(usage_idx);
        fprintf(stderr, "usage_range: %lu %lu\n", usage_range.start, usage_range.length);
        if (usage_range.empty()) {
            continue;
        }

        /* The following are tokens:
             [, ], (, ), |, ..., strings_without_spaces, <stuff in brackets>, newlines
             A string_without_spaces abutting <stuff_in_brackets> like so:
                foo<bar baz>
             counts as only a single token.
        */
        const size_t start = usage_range.start, end = usage_range.end();
        assert(end >= start);
        bool within_brackets = false;
        size_t bracket_start = npos;
        size_t word_start = npos;
        
        const token_t::type_t word_type = token_t::word;
        
        const char_t *sc = source.c_str();
        
        const char *options_shortcut = "[options]";
        const size_t options_shortcut_len = strlen(options_shortcut);
        
        for (size_t i=start; i < end;)
        {
            const char_t c = sc[i];
            if (! within_brackets)
            {
                if (substr_equals(options_shortcut, sc + i, options_shortcut_len)) {
                    // Some sort of parenthesis
                    if (word_start != npos) {
                        tokens.push_back(token_t(word_start, i - word_start, word_type));
                        word_start = npos;
                    }
                    tokens.push_back(token_t(i, 1, token_t::options));
                    i += options_shortcut_len;
                } else if (contains("()[]|", c)) {
                    // Some sort of parenthesis
                    if (word_start != npos) {
                        tokens.push_back(token_t(word_start, i - word_start, word_type));
                        word_start = npos;
                    }
                    tokens.push_back(token_t(i, 1, c));
                    i += 1;
                } else if (contains(" \t\n", c)) {
                    // Whitespace
                    if (word_start != npos) {
                        tokens.push_back(token_t(word_start, i - word_start, word_type));
                        word_start = npos;
                    }
                    if (c == '\n') {
                        tokens.push_back(token_t(i, 1, token_t::newline));
                    }
                    i += 1;
                } else if (substr_equals("...", sc + i, 3)) {
                    // Ellipsis
                    if (word_start != npos) {
                        tokens.push_back(token_t(word_start, i - word_start, word_type));
                        word_start = npos;
                    }
                    tokens.push_back(token_t(i, 3, '.'));
                    i += 3;
                } else {
                    // Normal char, or possibly bracket
                    if (word_start == npos) {
                        word_start = i;
                    }
                    if (c == '<') {
                        within_brackets = true;
                        bracket_start = i;
                    }
                    i += 1;
                }
            } else {
                // Within brackets
                if (c == '>') {
                    within_brackets = false;
                    bracket_start = npos;
                }
                i += 1;
            }
        }
        
        if (within_brackets) {
            append_error(&this->errors, bracket_start, "Unclosed bracket");
        }
        
        // grab trailing word
        if (word_start != npos) {
            tokens.push_back(token_t(word_start, end - word_start, word_type));
            word_start = npos;
        }
    }
    return tokens;
}

/* Collects options, i.e. tokens of the form --foo */
template<typename ENTRY_TYPE>
option_list_t collect_options(const ENTRY_TYPE &entry) {
    option_list_t resulting_options;
    std::vector<const simple_clause_t *> nodes = collect_nodes<ENTRY_TYPE, simple_clause_t>(entry);
    for (size_t i=0; i < nodes.size(); i++) {
        const token_t &tok = nodes.at(i)->word;
        if (this->token_substr_equals(tok, "-", 1)) {
            resulting_options.push_back(parse_option_from_string(this->source, tok.range, &this->errors));
        }
    }
    return resulting_options;
}

static bool char_is_valid_in_parameter(char_t c) {
    const char *invalid = "|<>,= \t";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

static bool char_is_valid_in_variable(char_t c) {
    const char *invalid = "-|,= \t";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

template<char T>
static bool it_equals(char_t c) { return c == T; }

template<typename T>
static range_t scan_while(const string_t &str, range_t *remaining, T func) {
    range_t result(remaining->start, 0);
    while (result.end() < remaining->end() && func(result.end())) {
        result.length += 1;
        remaining->start += 1;
        remaining->length -= 1;
    }
    return result;
}

static option_t parse_option_from_string(const string_t &str, range_t *remaining, bool within_options_spec, error_list_t *errors) {
    assert(remaining->length > 0);
    
    // Count how many leading dashes
    const size_t start = remaining->start;
    range_t leading_dash_range = scan_while(str, remaining, it_equals<'-'>);
    assert(leading_dash_range.length > 0);
    if (leading_dash_range.length > 2) {
        append_error(errors, start, "Too many dashes");
    }
    
    // Walk over characters valid in a name
    range_t name_range = scan_while(str, remaining, char_is_valid_in_parameter);
    if (within_options_spec) {
        scan_while(str, remaining, isspace);
    }
    
    // Check to see if there's an = sign
    const range_t equals_range = scan_while(str, remaining, it_equals<'='>);
    if (equals_range.length > 1) {
        append_error(errors, equals_range.start, "Too many equal signs");
    }

    // Try to scan a variable
    if (within_options_spec) {
        scan_while(str, remaining, isspace);
    }
    range_t variable_range = scan_while(str, remaining, char_is_valid_in_variable);
    
    // Skip over commas and whitespace for the next range
    if (within_options_spec) {
        scan_while(str, remaining, isspace);
        scan_while(str, remaining, it_equals<','>);
        scan_while(str, remaining, isspace);
    }
    return option_t(name_range, variable_range, leading_dash_range.length, ! equals_range.empty());
}


/* Separates a substring of str into name and value ranges about an = sign. String must start with - */
option_t parse_option_from_string(const string_t &str, const range_t &range, error_list_t *errors) const {
    assert(range.length > 0);
    size_t start = range.start;
    size_t len = range.length;
    size_t end = range.end();
    
    // Count how many leading dashes
    size_t leading_dash_count = 0;
    while (leading_dash_count < len && str.at(start + leading_dash_count) == char_t('-')) {
        leading_dash_count++;
    }
    assert(leading_dash_count > 0);
    if (leading_dash_count > 2) {
        append_error(errors, start, "Too many dashes");
    }
    
    // find an =. We ignore any = signs after the first
    size_t equals_pos = str.find(char_t('='), start);
    if (equals_pos >= end) {
        // Ignore any = sign at or past the end of our substring
        equals_pos = string_t::npos;
    }
    
    // Construct ranges
    range_t name, value;
    if (equals_pos == string_t::npos) {
        // No value, name only
        name = range_t(start + leading_dash_count, len - leading_dash_count);
        value = range_t(len, 0);
    } else {
        // Has a value. The value starts just after the = sign
        assert(equals_pos > leading_dash_count);
        name = range_t(start + leading_dash_count, equals_pos - leading_dash_count - start);
        value = range_t(equals_pos + 1, end - equals_pos - 1);
    }
    
    return option_t(name, value, leading_dash_count);
}

/* Given an option spec in the given range, that extends from the initial - to the end of the description, parse out a list of options */
void parse_one_option_spec(const range_t &range, option_list_t *out_result, error_list_t *errors) const {
    assert(! range.empty());
    assert(this->source.at(range.start) == char_t('-'));
    const size_t end = range.end();
    
    // Look for two spaces. Those separate the description.
    // This is a two-space "C-string"
    const char_t two_spaces[] = {char_t(' '), char_t(' '), char_t('\0')};
    size_t options_end = this->source.find(two_spaces, range.start);
    if (options_end > end) {
        options_end = end; // no description
    }
    
    // Determine the description range. Skip over its leading whitespace
    range_t description_range = range_t(options_end, end - options_end);
    scan_while(this->source, &description_range, isspace);
    
    // Parse the options portion
    assert(options_end >= range.start);
    range_t remaining(range.start, options_end - range.start);
    scan_while(this->source, &remaining, isspace);
    while (! remaining.empty()) {
        // Skip whitespace
        option_t opt = parse_option_from_string(this->source, &remaining, true, errors);
        if (opt.name.empty()) {
            // Failed to get an option, give up
            break;
        }
        opt.description_range = description_range;
        out_result->push_back(opt);
    }
}

// Returns true if the source line contains an option spec, that is, has at least one leading space, and then a dash
static bool line_contains_option_spec(const string_t &str, const range_t &range) {
    range_t remaining = range;
    range_t space = scan_while(str, &remaining, isspace);
    range_t dashes = scan_while(str, &remaining, it_equals<'-'>);
    return space.length > 0 && dashes.length > 0;
}

/* Finds the headers containing name (for example, "Options:") and returns source ranges for them. Header lines are not included. */
range_list_t source_ranges_for_section(const char *name) const {
    range_list_t result;
    bool in_desired_section = false;
    range_t line_range;
    while (get_next_line(this->source, &line_range)) {
        size_t line_start = line_range.start;
        // It's a header line if the first character is not whitespace
        bool is_header = ! isspace(source.at(line_start));
        if (is_header) {
            // Check to see if the name is found before the first colon
            // Note that if name is not found at all, name_pos will have value npos, which is huge (and therefore not smaller than line_end)
            size_t name_pos = find_case_insensitive(source, name, line_start);
            size_t line_end = line_range.end();
            in_desired_section = (name_pos < line_end && name_pos < source.find(':', line_start));
            
            if (in_desired_section) {
                // Append a new empty range. We will add to it.
                result.push_back(range_t(0, 0));
            }
        }
        
        if (in_desired_section && ! is_header) {
            // Extend the last range with this line
            result.back().merge(line_range);
        }
    }
    return result;
}

/* Parses the option specification at the given location */
option_list_t parse_options_spec(error_list_t *errors) const {
    option_list_t result;
    
    const range_list_t section_ranges = source_ranges_for_section("Options:");
    for (size_t range_idx = 0; range_idx < section_ranges.size(); range_idx++) {
        const range_t section_range = section_ranges.at(range_idx);
        const size_t section_end = section_range.end();
        range_t line_range(section_range.start, 0);
        while (get_next_line(source, &line_range, section_end)) {
            // These are all valid option specifications:
            // --foo
            // --foo <bar>
            // --foo=<bar>
            // --foo=<bar>, -f=<bar>
            // There may also be a description after two spaces
            // The description may span multiple lines.
            

            // Check to see if this line starts with a -
            if (line_contains_option_spec(this->source, line_range)) {
                // It's a new option. Determine how long its description goes.
                range_t option_spec_range = line_range;
                range_t local_range = line_range;
                while (get_next_line(this->source, &local_range, section_end)) {
                    if (! line_contains_option_spec(this->source, local_range)) {
                        option_spec_range.merge(local_range);
                        // Set our outermost lines to this line, so we'll skip past it next iteration
                        line_range = local_range;
                    }
                }
                
                // Got the description. Parse out an option.
                parse_one_option_spec(option_spec_range, &result, errors);
            }
        }
    }
    return result;
}

/* Extracts a long option from the arg at idx, and appends the result to out_result. Updates idx. */
bool parse_long(const string_list_t &argv, size_t *idx, const option_list_t &options, resolved_option_list_t *out_result, error_list_t *out_errors) {
    const string_t &arg = argv.at(*idx);
    assert(substr_equals("--", arg, 2));
    
    /* Parse the argument into an 'option'. Note that this option does not appear in the options list. TODO: Need to distinguish between equivalent ways of specifying parameters (--foo=bar and --foo bar) */
    error_list_t local_errors;
    const option_t arg_as_option = parse_option_from_string(arg, range_t(0, arg.size()), &local_errors);
    // TODO: What if we get an error, e.g. there's more than two dashes?
    assert(arg_as_option.type == option_t::double_long);
    
    /* Get list of matching long options. These are pointers into our options array */
    std::vector<const option_t *> matches;
    for (size_t i=0; i < options.size(); i++) {
        const option_t &opt = options.at(i);
        // This comparison is terrifying. It's just comparing two substrings: one in source (the given option) and the name portion of the argument
        if (opt.type == option_t::double_long && this->source.compare(opt.name.start, opt.name.length, arg, arg_as_option.name.start, arg_as_option.name.length) == 0) {
            matches.push_back(&opt);
        }
    }
    
    /* TODO: handle unambiguous prefix */
    /* TODO: Better error reporting */
    /* TODO: can eliminate matches array entirely, just use a single index */
    
    bool success = false;
    size_t match_count = matches.size();
    if (match_count > 1) {
        append_error(out_errors, matches.at(0)->name.start, "Option specified too many times");
    } else if (match_count < 1) {
        append_error(out_errors, -1, "Unknown option");
    } else {
        bool errored = false;
        assert(match_count == 1);
        const option_t *match = matches.at(0);
        optional_string_t value;
        
        /* Ensure the option and argument agree on having a value */
        if (match->has_value()) {
            if (arg_as_option.has_value()) {
                // The arg was specified as --foo=bar
                value = string_t(arg, arg_as_option.value.start, arg_as_option.value.length);
            } else {
                // The arg was (hopefully) specified as --foo bar
                if (*idx + 1 < argv.size()) {
                    value = argv.at(*idx + 1);
                    *idx += 1;
                } else {
                    append_error(&errors, match->value.start, "Option expects an argument");
                    errored = true;
                }
            }
        } else if (arg_as_option.has_value()) {
            // A value was specified as --foo=bar, but none was expected
            append_error(&errors, match->name.start, "Option does not expect an argument");
            errored = true;
        }
        if (! errored) {
            out_result->push_back(resolved_option_t(*match, value));
            *idx += 1;
        }
        success = true;
    }
    return success;
}

/* The Python implementation calls this "parse_argv" */
void separate_argv_into_options_and_positionals(const string_list_t &argv, const option_list_t &options, positional_argument_list_t *out_positionals, resolved_option_list_t *out_resolved_options, bool options_first = false) {
    
    size_t idx = 0;
    while (idx < argv.size()) {
        const string_t arg = argv.at(idx);
        if (str_equals("--", arg)) {
            // Literal --. The remaining arguments are positional. Insert everything left and exit early
            out_positionals->insert(out_positionals->end(), argv.begin() + idx + 1, argv.end());
            break;
        } else if (substr_equals("--", arg, 2)) {
            // Leading long option
            if (parse_long(argv, &idx, options, out_resolved_options, &this->errors)) {
                // parse_long will have updated idx and out_resolved_options
            } else {
                // We have to update idx
                idx += 1;
            }
        } else {
            // TODO: single options
            out_positionals->push_back(arg);
            idx += 1;
        }
    }
}

/* The result of parsing argv */
typedef std::map<string_t, base_argument_t<string_t> > option_map_t;
typedef std::vector<option_map_t> option_map_list_t;

struct match_state_t {
    option_map_t option_map;
    size_t next_positional_index;
    
    match_state_t() : next_positional_index(0)
    {}
    
    void swap(match_state_t &rhs) {
        this->option_map.swap(rhs.option_map);
        std::swap(this->next_positional_index, rhs.next_positional_index);
    }
};

typedef std::vector<match_state_t> match_state_list_t;

static match_state_list_t no_match() { return match_state_list_t(); }

struct match_context_t {
    /* Note: these are stored references. Match context objects are expected to be transient and stack-allocated. */
    const positional_argument_list_t &positionals;
    const resolved_option_list_t &resolved_options;
    const option_list_t options_section;
    
    bool has_more_positionals(const match_state_t *state) const {
        assert(state->next_positional_index <= positionals.size());
        return state->next_positional_index < positionals.size();
    }
    
    bool has_unconsumed_options(const match_state_t *state, const string_t &src) const {
        /* An unconsumed option means an option from resolved_options that was not matched during tree descent, i.e. is not in option_map. This can be derived from option_map. */
        string_t name;
        for (size_t i=0; i < resolved_options.size(); i++) {
            const range_t &opt_range = resolved_options.at(i).option.name;
            name.assign(src, opt_range.start, opt_range.length);
            if (state->option_map.find(name) == state->option_map.end()) {
                return true;
            }
        }
        return false;
    }
    
    const positional_argument_t &next_positional(match_state_t *state) const {
        assert(state->next_positional_index < positionals.size());
        return positionals.at(state->next_positional_index);
    }
    
    const positional_argument_t &acquire_next_positional(match_state_t *state) const {
        assert(state->next_positional_index < positionals.size());
        return positionals.at(state->next_positional_index++);
    }
    
    match_context_t(const positional_argument_list_t &p, const resolved_option_list_t &r) : positionals(p), resolved_options(r)
    {}
};

static void state_list_destructive_append_to(match_state_list_t *source, match_state_list_t *dest) {
    size_t src_count = source->size();
    size_t dst_init_count = dest->size();
    
    // Add a bunch of empty maps to the destination
    dest->resize(dst_init_count + src_count);
    
    // Swap corresponding empty maps
    for (size_t i=0; i < src_count; i++) {
        match_state_t &src_state = source->at(i);
        match_state_t &dst_state = dest->at(i + dst_init_count);
        dst_state.swap(src_state);
    }
    
    // Clean up source since it's now useless
    source->clear();
}

static void state_destructive_append_to(match_state_t *state, match_state_list_t *dest) {
    dest->resize(dest->size() + 1);
    dest->back().swap(*state);
}

template<typename T>
match_state_list_t try_match(T& ptr, match_state_t *state, const match_context_t *ctx) {
    match_state_list_t result;
    if (ptr.get()) {
        result = this->match(*ptr, state, ctx);
    }
    return result;
}

template<typename T>
match_state_list_t try_match(T& ptr, match_state_list_t &state_list, const match_context_t *ctx) {
    match_state_list_t total_result;
    if (ptr.get()) {
        for (size_t i=0; i < state_list.size(); i++) {
            match_state_t *state = &state_list.at(i);
            match_state_list_t child_result = this->match(*ptr, state, ctx);
            state_list_destructive_append_to(&child_result, &total_result);
        }
    }
    return total_result;
}

/* Match overrides */
match_state_list_t match(const usage_t &node, match_state_t *state, const match_context_t *ctx) {
    if (! ctx->has_more_positionals(state)) {
        // todo: error handling
        return no_match();
    }
    /* Must duplicate the state for the next usage */
    match_state_t copied_state = *state;
    ctx->acquire_next_positional(state);
    match_state_list_t main_branch = try_match(node.expression_list, state, ctx);
    match_state_list_t next_branch = try_match(node.next_usage, &copied_state, ctx);
    state_list_destructive_append_to(&next_branch, &main_branch);
    return main_branch;
}

match_state_list_t match(const expression_list_t &node, match_state_t *state, const match_context_t *ctx) {
    match_state_list_t intermed_state_list = try_match(node.expression, state, ctx);
    return try_match(node.opt_expression_list, intermed_state_list, ctx);
}

match_state_list_t match(const opt_expression_list_t &node, match_state_t *state, const match_context_t *ctx) {
    match_state_list_t result;
    if (node.expression_list.get()) {
        return match(*node.expression_list, state, ctx);
    } else {
        // end of the list
        return match_state_list_t(1, *state);
    }
}

match_state_list_t match(const expression_t &node, match_state_t *state, const match_context_t *ctx) {
    // Must duplicate the state for the second branch
    match_state_t copied_state = *state;
    match_state_list_t result1 = try_match(node.compound_clause, state, ctx);
    match_state_list_t result2 = try_match(node.or_continuation, &copied_state, ctx);
    
    // Combine the two lists into result1
    state_list_destructive_append_to(&result2, &result1);
    return result1;
}

match_state_list_t match(const or_continuation_t &node, match_state_t *state, const match_context_t *ctx) {
    return try_match(node.expression, state, ctx);
}

match_state_list_t match(const compound_clause_t &node, match_state_t *state, const match_context_t *ctx) {
    // Check to see if we have ellipsis. If so, we keep going as long as we can.
    bool has_ellipsis = (node.opt_ellipsis.get() != NULL && node.opt_ellipsis->production > 0);
    match_state_list_t result;
    
    switch (node.production) {
        case 0:
        {
            /* This is a simple clause which may have ellipsis, like foo...
               If we have ellipsis, we match one time, and then construct a sequence of 'intermediate state lists'.
               An intermediate state represents the result of matching N times. Each time we construct
               a new intermediate state, we append (copy) all of its states into the result; thus we may
               match one time, two times, three times...
               We stop when we get no more matches, which usually happens when we run out of positionals.
            */
            result = try_match(node.simple_clause, state, ctx);
            if (has_ellipsis) {
                match_state_list_t intermediate_states = result;
                while (! intermediate_states.empty()) {
                    match_state_list_t next_states = try_match(node.simple_clause, intermediate_states, ctx);
                    result.insert(result.end(), next_states.begin(), next_states.end());
                    intermediate_states.swap(next_states);
                }
            }
            break;
        }
            
        case 1:
        {
            /* This is a parenthesized clause which may have ellipsis, like (foo)...
               Same algorithm as the simple clause above.
               TODO: this may loop forever with states that do not consume any values, e.g. ([foo])...
            */
            result = try_match(node.expression_list, state, ctx);
            if (has_ellipsis) {
                match_state_list_t intermediate_states = result;
                while (! intermediate_states.empty()) {
                    match_state_list_t next_states = try_match(node.expression_list, intermediate_states, ctx);
                    result.insert(result.end(), next_states.begin(), next_states.end());
                    intermediate_states.swap(next_states);
                }
            }
            break;
        }
            
        case 2:
        {
            /* This is a square-bracketed clause which may have ellipsis, like [foo]...
               Same algorithm as the simple clause above, except that we also append the initial state as a not-taken branch.
            */
            match_state_t not_taken_branch = *state;
            result = try_match(node.expression_list, state, ctx);
            if (has_ellipsis) {
                match_state_list_t intermediate_states = result;
                while (! intermediate_states.empty()) {
                    match_state_list_t next_states = try_match(node.simple_clause, intermediate_states, ctx);
                    result.insert(result.end(), next_states.begin(), next_states.end());
                    intermediate_states.swap(next_states);
                }
            }
            // Append the not taken branch
            state_destructive_append_to(&not_taken_branch, &result);
            break;
        }
        
        case 3:
        {
            // This is the [options] clause. It does not have ellipsis.
            
            break;
        }
        
        default:
            assert(0 && "unknown production");
            return no_match();
    }
    
    return result;
}

match_state_list_t match(const simple_clause_t &node, match_state_t *state, const match_context_t *ctx) {
    // Check to see if this is an argument or a variable
    match_state_list_t result;
    const range_t &range = node.word.range;
    char_t first_char = this->source.at(range.start);
    
    if (first_char == char_t('<')) {
        if (ctx->has_more_positionals(state)) {
            // Variable argument. Modify the state.
            assert(range.length >= 2);
            const string_t name = string_t(this->source, range.start + 1, range.length - 2);
            arg_t *arg = &state->option_map[name];
            arg->key = name;
            arg->values.push_back(ctx->acquire_next_positional(state).value);
            state_destructive_append_to(state, &result);
        }
    } else if (first_char == char_t('-')) {
        // Option
        const option_t opt_in_doc = parse_option_from_string(this->source, range, NULL);
        
        // Find the matching option from the option list
        const resolved_option_t *resolved_opt = NULL;
        for (size_t i=0; i < ctx->resolved_options.size(); i++) {
            const resolved_option_t *opt_in_argv = &ctx->resolved_options.at(i);
            if (opt_in_argv->option == opt_in_doc) {
                resolved_opt = opt_in_argv;
                break;
            }
        }
        
        if (resolved_opt != NULL) {
            // woo hoo
            const string_t name = this->string_for_range(opt_in_doc.name);
            arg_t *arg = &state->option_map[name];
            arg->key.assign(name);
            if (! resolved_opt->value.missing()) {
                arg->values.push_back(resolved_opt->value.value());
            }
            arg->count += 1;
            state_destructive_append_to(state, &result);
        }
        
    } else {
        // Fixed argument
        if (ctx->has_more_positionals(state)) {
            const string_t &name = ctx->next_positional(state).value;
            if (this->source.compare(range.start, range.length, name) == 0) {
                // The fixed argument matches
                arg_t *arg = &state->option_map[name];
                arg->key.assign(name);
                arg->values.push_back(string_t(1, char_t('1')));
                arg->count += 1;
                ctx->acquire_next_positional(state);
                state_destructive_append_to(state, &result);
            }
        }
    }
    return result;
}

/* Matches argv */
void match_argv(const positional_argument_list_t &positionals, const resolved_option_list_t &resolved_options, const usage_t &tree) {
    match_context_t ctx(positionals, resolved_options);
    match_state_t init_state;
    match_state_list_t result = match(tree, &init_state, &ctx);
    
    std::cerr << "Result count: " << result.size() << "\n";
    for (size_t i=0; i < result.size(); i++) {
        const match_state_t &state = result.at(i);
        bool is_incomplete = ctx.has_more_positionals(&state) || ctx.has_unconsumed_options(&state, this->source);
        std::cerr <<  "Result " << i << (is_incomplete ? " (INCOMPLETE)" : "") << ":\n";
        for (typename option_map_t::const_iterator iter = state.option_map.begin(); iter != state.option_map.end(); ++iter) {
            const string_t &name = iter->first;
            const arg_t &arg = iter->second;
            std::cerr << "\t" << name << ": ";
            for (size_t j=0; j < arg.values.size(); j++) {
                if (j > 0) {
                    std::cerr << ", ";
                }
                std::cerr << arg.values.at(j);
            }
            std::cerr << '\n';
        }
    }
}

CLOSE_DOCOPT_IMPL;

#if 0
std::map<std::string, argument_t> docopt_parse(const std::string &doc, const std::vector<std::string> &argv) {
    
}

std::map<std::wstring, wargument_t> docopt_wparse(const std::wstring &doc, const std::vector<std::wstring> &argv) {
    
}
#endif

int main(void) {
    using namespace docopt_fish;
    const std::string usage =
        "Usage: \n"
        "  naval_fate ship new <name>...\n"
        "  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]\n"
        "  naval_fate.py ship shoot <x> <y>\n"
        "  naval_fate.py stuff [options]\n"
        "Options:\n"
        "  -h --help  Show this screen\n"
        "  -a, --all  List everything\n"
        "  -h, --human-readable  Display in human-readable format\n"
        "  -i <file>, --input <file>  Set input file\n";
    
    docopt_impl<std::string> impl(usage);
    token_list_t tokens = impl.tokenize_usage();
    fprintf(stderr, "%s\n", usage.c_str());
    for (size_t i=0; i < tokens.size(); i++) {
        const token_t &tok = tokens.at(i);
        fprintf(stderr, "%lu: '%c' [%lu, %lu]\n", i, (char)tok.type, tok.range.start, tok.range.length);
    }
    
    parse_context_t ctx(tokens);
    usage_t *tree = usage_t::parse(&ctx);
    
    // Extract options from the usage sections
    option_list_t usage_options = impl.collect_options(*tree);
    
    // Extract options from the options section
    option_list_t shortcut_options = impl.parse_options_spec(&impl.errors);
    
    // Combine these into a single list
    option_list_t all_options;
    all_options.reserve(usage_options.size() + shortcut_options.size());
    all_options.insert(all_options.end(), usage_options.begin(), usage_options.end());
    all_options.insert(all_options.end(), shortcut_options.begin(), shortcut_options.end());
    
    // Dump them
    for (size_t i=0; i < all_options.size(); i++) {
        fprintf(stderr, "Option %lu: %s\n", i, all_options.at(i).describe(usage).c_str());
    }
    
    std::string dumped = node_dumper_t::dump_tree(*tree, usage);
    fprintf(stderr, "%s\n", dumped.c_str());
    
    std::vector<std::string> argv;
    argv.push_back("naval_fate");
#if 1
    argv.push_back("ship");
    argv.push_back("new");
    argv.push_back("alpha");
    argv.push_back("beta");
    argv.push_back("gamma");
#elif 0
    argv.push_back("mine");
    argv.push_back("set");
    argv.push_back("10");
    argv.push_back("20");
    argv.push_back("--moored");
#else
    argv.push_back("ship");
    argv.push_back("shoot");
    argv.push_back("3");
    argv.push_back("5");
#endif
    
    docopt_impl<std::string>::positional_argument_list_t positionals;
    docopt_impl<std::string>::resolved_option_list_t resolved_options;
    impl.separate_argv_into_options_and_positionals(argv, all_options, &positionals, &resolved_options);
    
    for (size_t i=0; i < positionals.size(); i++) {
        fprintf(stderr, "positional %lu: %s\n", i, positionals.at(i).value.c_str());
    }
    
    for (size_t i=0; i < resolved_options.size(); i++) {
        const docopt_impl<std::string>::resolved_option_t &opt = resolved_options.at(i);
        range_t range = opt.option.name;
        const std::string name = std::string(usage, range.start, range.length);
        fprintf(stderr, "%lu: %s (%s)\n", i, name.c_str(), opt.value.c_str_or_empty());
    }

    impl.match_argv(positionals, resolved_options, *tree);
    
    delete tree;
    
    return 0;
}
